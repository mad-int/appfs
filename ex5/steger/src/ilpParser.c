#include "ilpParser.h"
#include "linearProgram.h"

#define BUF_SIZE 512
#define USAGE "file must consist of: \n#rows #columns constraints, e.g.:\n\n\
3 # columns (variables)\n5 # rows (constraints)\n2 3 -5 4 5 <= 8\n3 6 0 8 6 <= 10\
\n0 0 1 1 7 <= 1\n"

static enum readMode mode;


//! reads all defining data for some lp from a file
/*
	\param filename the name of the file
*/
struct linearProgram* createLPFromFile(const char* filename) {
	assert(NULL != filename);
	
	mode = READ_COL;
	
	FILE* fp;
	char buffer[BUF_SIZE];
	char* s;
	int lines = 0;
	
	struct linearProgram* binaryLP;
	int numOfCols = 0;
	int numOfRows = 0;
	int currentConstraint = 0;
	char* type;
	
	if (NULL == (fp = fopen(filename, "r"))) {
		fprintf(stderr, "Can't open file %s\n",filename);
		printf(USAGE);
		exit(EXIT_FAILURE);
	}
	// need a helpful string for strtok
	char* help_me = allocate(BUF_SIZE, sizeof(*help_me));
	char* end_pos;
	while(NULL != (s = fgets(buffer, sizeof(buffer), fp))) {
		end_pos = strpbrk(s, "#\n\r");
		if (NULL != end_pos) {
			*end_pos = '\0'; // terminate string here, now we can process this line		
		}
		
		/* Skip over leading space
		*/
		while(isspace(*s)) 
			s++;
		
		/* Skip over empty lines
		*/
		if (!*s) /* <=> (*s == '\0') */
			continue;
		
		if (mode == READ_COL) {
			lines++;
			numOfCols = parseDecimal(s);
			mode++;
		}
		else if (mode == READ_ROW) {
			lines++;
			numOfRows = parseDecimal(s);
			
			/* ready to allocate memory for binaryLP.coeffs, rhs-vector and types-enum */
			binaryLP = initializeLP(numOfCols, numOfRows);
			// ready to parse constraint matrix
			mode++;
		}
		else {
			assert(mode == READ_CONSTR);
			lines++;
			if (currentConstraint + 1 > binaryLP->row) {
				printf("Second decimal must correspond to number of constraints!\n");
				exit(EXIT_FAILURE);
			}
			if (NULL == (type = strpbrk(s, "<>="))) {
				printf("Constraint in line %d has wrong shape (<,> or = missing)!\n", lines);
				exit(EXIT_FAILURE);
			} // check which type of constraint
			setType(*type, currentConstraint, binaryLP->types);
			
			strcpy(help_me, s);
			if (-1 == fillRow(binaryLP->coeffs[currentConstraint], binaryLP->col, strtok(s,"><="), " ")) {
				// free pointer:
				deleteLinearProgram(binaryLP);
				deallocate(help_me);
				printf("Incorrect number or format of coefficients in line %d.\n", lines);
				printf(USAGE);
				exit(EXIT_FAILURE);
			}
			char* test = strtok(help_me, "><=");
			if(NULL == (test = strtok(NULL, " ><="))) {
				printf("No rhs in line %d.\n", lines);
				printf(USAGE);
				exit(EXIT_FAILURE);
			}
			if (!validateNumber(test)) {
				printf("No number on right-hand-side of line %d.\n", lines);
				exit(EXIT_FAILURE);
			}
			binaryLP->rhs[currentConstraint] = atof(test);
			if (NULL != (test = strtok(NULL, " "))) {
				printf("More than one value on right-hand-side of line %d!\n", lines);
				exit(EXIT_FAILURE);
			}
			currentConstraint++;
		}
	}
	if (currentConstraint < numOfRows) {
		printf("Too few rows!\n");
		exit(EXIT_FAILURE);
	}
	if (lines == 0) {
		printf("The file was empty! Nice try...\n");
		exit(EXIT_FAILURE);
	}
	free(help_me);
	
	fclose(fp);
	return binaryLP;
}

//! Checks if some string is a number (if applicable with sign)
/* checks if a string contains a number, for the first char '+','-','.' and any digit '0-9' is allowed
	for all other chars any digit or '.' is allowed
	\param string the string to be validated
*/
int validateNumber(char* string) {
	assert(NULL != string);
	
	int n = strlen(string);
	
	assert(n>=1);
	char c = string[0];
	// first char can be number or sign
	
	if(!((c >= '0' && c <= '9') || c == '+' || c == '-' || c == '.')) {
		return 0;
	}
	// only sign not allowed
	if (n == 1 && (c == '+' || c == '-')) {
		return 0;
	}
	for(int i = 1; i < n; ++i)
	{
		if(!((string[i] >= '0' && string[i] <= '9') || string[i] == '.')) {
			return 0;
		}
	}
	return 1;
}

//! fills some array row with numbers contained in inputString.
/*	This method shall be used only inside method createLPFromFile
	The numbers must be seperated by a delimiter (e.g whitespace)

	\return number of decimals read, -1 if number of coeffs in file does not match
*/
int fillRow(num* row, int col, char* inputString, char* delimiter) {
	int i = 0;
	int asInteger;
	float asFloat;
	assert(NULL != inputString);
	assert(NULL != row);
	// Seperate each number
	char* value = strtok(inputString, delimiter);
	while (NULL != value) {
		if (i+1 > col) {
			printf("Number of columns more than specified in line 1!\n");
			return -1;
		}
		// Check if it is a number
		if (!validateNumber(value)) {
			return -1;
		}
		row[i] = atof(value);
		asInteger = atoi(value);
		asFloat = atof(value);
		
		// Check 
		if (((asFloat - asInteger) != 0) && !IS_DOUBLE) {
			return -1;
		}
		
		
		i++;
		// Next number
		value = strtok(NULL, delimiter);
	}
	if (i < col) {
		return -1;
	}
	return i;
}

//! converts some decimal that represents the dimension of an LP to an int
/*	if the Decimal cannot be parsed, the program exits, as it does if the dimension is not valid (<= 0 or > 32)
	\param s the string to be parsed
	\return the converted integer
	
*/
int parseDecimal(char* s) {
	int t = atoi(s);
	float test = atof(s);
	
	if (!t || ((test - (float)t) != 0)) {
		fprintf(stderr, "Can't parse dimension %d!\n",mode+1);
		printf(USAGE);
		exit(EXIT_FAILURE);
	}
	if (t <= 0 || t >32) {
		printf("number of rows/columns must not exceed 32 (and be > 0)\n");
		exit(EXIT_FAILURE);
	}
	return t;
}