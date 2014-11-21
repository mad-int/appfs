#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h> // strpbrk
#include <ctype.h>  // isspace
#include <time.h>   // clock
// local
#include "constraintArray.h"
#include "constraint.h"
#include "allocate.h"

#define MAX_NUMBER_LENGTH 32 // maximum length of a string representing a number
#define MAX_LINE_LEN 512 // Maximum input line length

ConstArray* newConstArray(int dim, int numberOfConstraints) {

	assert(numberOfConstraints > 0);
	assert(dim > 0);

	ConstArray* c = (ConstArray*) allocate(1, sizeof(*c));
	assert(c != NULL);

	c->dim = dim;

	c->numberOfConstraints = numberOfConstraints;

	Constraint** constraints = (Constraint**) allocate(numberOfConstraints,
			sizeof(*constraints));
	assert(constraints != NULL);

	c->constraints = constraints;
	return c;
}

void freeConstArray(ConstArray* c) {

	int i;
	for (i = 0; i < c->numberOfConstraints; i++) {
		freeConstraint(c->constraints[i]);
	}
	deallocate(c->constraints);
	deallocate(c);
}

void setConstraint(ConstArray* cA, Constraint* c, int index) {

	assert(index >= 0);
	assert(index < cA->numberOfConstraints);
	assert(cA->dim == c->dim);

	cA->constraints[index] = c;
}

int checkForAllConstraints(ConstArray* c, int booleanAssignment) {

	int i;
	for (i = 0; i < c->numberOfConstraints; i++) {

		if (!(checkForSingleConstraint(c->constraints[i], booleanAssignment))) {
			return 0;
		}
	}
	return 1;
}

char* getBinaryFromInt(int binaryAssignment, int length) {

	char* string = allocate(length, sizeof(*string));

	int mask = 1;
	int i;
	for (i = 0; i < length; i++) {
		string[i] = (0 == (mask & binaryAssignment)) ? '0' : '1';
		mask = mask << 1;
	}
	return string;
}

void printAllFeasibleBinaryAssignments(ConstArray* c) {

	// assuming a 64bit system
	long testBinary;
	long upperBound = 1L << (c->dim);
	int feasSolutions = 0;
	clock_t start = clock();

	for (testBinary = 0; testBinary < upperBound; testBinary++) {
		if (checkForAllConstraints(c, testBinary)) {
			char* string;
			string = getBinaryFromInt(testBinary, c->dim);

			printf("Binary assignment %s is valid!\n", string);
			feasSolutions = 1;
			deallocate(string);
		}
	}
	clock_t end = clock();
	if (!feasSolutions) {
		printf("No feasible solution found!\n");
	}
	double timeInS = (double) (end - start) / CLOCKS_PER_SEC;
	printf(
			"Time for enumerating all binaries, checking for feasibility and printing the feasible ones is: %f s\n",
			timeInS);
}

int printConstArray(ConstArray* c) {

	int i, j;

	printf("Printing constraint array:\n");
	printf("dimension: %i\n", c->dim);
	printf("number of constraints: %i\n", c->numberOfConstraints);
	printf("Constraints:\n");
	char* cString = allocate(MAX_NUMBER_LENGTH, sizeof(*cString));
	;
	for (i = 0; i < c->numberOfConstraints; i++) {
		Constraint* constraint = c->constraints[i];
		char* typeString;
		switch (constraint->type) {
		case LESS_EQUAL:
			typeString = "<=";
			break;
		case EQUAL:
			typeString = "==";
			break;
		default:
			typeString = ">=";
		}
		for (j = 0; j < c->dim; j++) {
			Numb coeff = getCoefficient(constraint, j);
			if (EXIT_FAILURE == parseNumbToString(&cString, coeff)) {
				fprintf(stderr,
						"Error parsing number to string! Given number was %i. coefficient of constraint %i.",
						j + 1, i + 1);
				deallocate(cString);
				return EXIT_FAILURE;
			}
			char* positiveSpace = " ";
			if (0 > coeff) {
				positiveSpace = "";
			}
			printf(" + %s%s x%i", positiveSpace, cString, j);
		}

		if (EXIT_FAILURE == parseNumbToString(&cString, constraint->rhs)) {
			fprintf(stderr,
					"Error parsing number to string! Given number was rhs of constraint %i.",
					i + 1);
			deallocate(cString);
			return EXIT_FAILURE;
		}
		char* positiveSpace = " ";
		if (0 > constraint->rhs) {
			positiveSpace = "";
		}
		printf(" %s %s%s\n", typeString, positiveSpace, cString);
	}
	deallocate(cString);
	return EXIT_SUCCESS;
}

int parseOneIntegerLine(char* s, int* ptrToValue, char* valueTyp,
		const char* filename) {

	char* debug;
	int value = strtol(s, &debug, 10);
	// whitespaces after the number are okay
	while (isspace(*debug)) {
		debug++;
	}
	if (0 != strcmp(debug, "\0")) {
		fprintf(stderr,
				"Invalid data file %s: Invalid %s definition! Expected line format is one integer number optionally surrounded by whitespaces! Given format was: '%s'\n",
				filename, valueTyp, s);
		return EXIT_FAILURE;
	}

	*ptrToValue = value;
	return EXIT_SUCCESS;
}

ConstArray* read_constArray_from_file(const char* filename) {
	assert(NULL != filename);
	assert(0 < strlen(filename));

// given data for file reading
	FILE* fp;
	char buf[MAX_LINE_LEN];
	char* s;
	int lines = 0;

// own data for constraint array building
	ConstArray* c;
	int dim = -1;
	int nConstraints = -1;
	int constraintIndex = 0;
	int dimensionIndex;
	Numb* coeffs;
	Numb rhs;
	char* delim = " ";
	char* token;
	constType type = -1;

	if (NULL == (fp = fopen(filename, "r"))) {
		fprintf(stderr, "Can't open file %s\n", filename);
		return NULL;
	}
	while (NULL != (s = fgets(buf, sizeof(buf), fp))) {
		char* t = strpbrk(s, "#\n\r");

		lines++;

		if (NULL != t) /* else line is not terminated or too long */
			*t = '\0'; /* clip comment or newline */

		/* Skip over leading space
		 */
		while (isspace(*s))
			s++;

		/* Skip over empty lines
		 */
		if (!*s) /* <=> (*s == '\0') */
			continue;

		if (-1 == dim) {
			if (EXIT_FAILURE
					== parseOneIntegerLine(s, &dim, "number of variables",
							filename)) {
				return NULL;
			}
			if (0 < dim && 33 > dim) {
				continue;
			} else {
				fprintf(stderr,
						"Invalid data file %s: Number x of variables has to be 1 <= x <= 32! Given x was %i.\n",
						filename, dim);
				return NULL;
			}
			printf("dim %i\n", dim);
		}

		if (-1 == nConstraints) {
			if (EXIT_FAILURE
					== parseOneIntegerLine(s, &nConstraints,
							"number of constraints", filename)) {
				return NULL;
			}
			if (0 < nConstraints) {
				c = newConstArray(dim, nConstraints);
				continue;
			} else {
				fprintf(stderr,
						"Invalid data file %s: Number of constraints has to be a positive integer! Given number was %i.\n",
						filename, nConstraints);
				return NULL;
			}
			printf("nConstraints %i\n", nConstraints);
		}

		//printf("Current line: %s\n",s);

		dimensionIndex = 0;
		coeffs = (Numb*) allocate(dim, sizeof(*coeffs));

		if (constraintIndex >= nConstraints) {
			fprintf(stderr,
					"Invalid data file %s: To many constraints! There should be at most %i.\n",
					filename, nConstraints);
			return NULL;
		}

		// while there are tokens and the token is not "<=", "==" or ">="
		for (token = strtok(s, delim);
				NULL != token && 0 != strcmp(token, "<=")
						&& 0 != strcmp(token, "==") && 0 != strcmp(token, ">=");
				token = strtok(NULL, delim)) {

			if (0 != strcmp(token, "<=")) {
				Numb value;
				if (EXIT_FAILURE == parseStringToNumb(token, &value)) {
					fprintf(stderr,
							"Invalid data file %s: Error parsing coefficient %i of constraint %i: Illegal %s number format of coefficient: '%s'!\n",
							filename, dimensionIndex + 1, constraintIndex + 1,
							number_type, token);
					return NULL;
				}
				coeffs[dimensionIndex] = value;
				dimensionIndex++;
			}
		}

		// token should be "<=" now
		if (NULL == token) {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: No comparison operator and no right-hand-side given!\n",
					filename, constraintIndex + 1);
			return NULL;
		} else {
			if (0 == strcmp(token, "<=")) {
				type = LESS_EQUAL;
			} else if (0 == strcmp(token, "==")) {
				type = EQUAL;
			} else {
				// no other token possible since otherwise the for loop above would not have finished
				type = GREATER_EQUAL;
			}
		}

		if (dimensionIndex != dim) {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: Incorrect number of variables! It should be %i but there are %i coefficients. \n",
					filename, constraintIndex + 1, dim, dimensionIndex);
			return NULL;
		}
		// rhs token
		token = strtok(NULL, delim);
		if (NULL != token) {
			if (EXIT_FAILURE == parseStringToNumb(token, &rhs)) {
				fprintf(stderr,
						"Invalid data file %s: Error parsing rhs of constraint %i: Illegal %s number format of rhs: '%s'!\n",
						filename, constraintIndex + 1, number_type, token);
				return NULL;
			}
		} else {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: No right-hand-side given!\n",
					filename, constraintIndex + 1);
			return NULL;
		}
		if (NULL != strtok(NULL, delim)) {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: Multiple right-hand-sides given!\n",
					filename, constraintIndex + 1);
			return NULL;
		}
		setConstraint(c, newConstraint(dim, coeffs, rhs, type),
				constraintIndex);
		constraintIndex++;
	}
	fclose(fp);

	if (constraintIndex != nConstraints) {
		fprintf(stderr,
				"Invalid data file %s: Not enough constraints! There are %i but there should be %i.\n",
				filename, constraintIndex, nConstraints);
		return NULL;
	}

	return c;
}

ConstArray* createDummyConstArray() {

	int maxDim = 5;
	int numberOfConstraints = 3;

	Numb* coeffs1 = (Numb*) allocate(maxDim, sizeof(*coeffs1));
	Numb* coeffs2 = (Numb*) allocate(maxDim, sizeof(*coeffs2));
	Numb* coeffs3 = (Numb*) allocate(maxDim, sizeof(*coeffs3));
	// coeffs 1:
	coeffs1[0] = 1.0;
	coeffs1[1] = 2.0;
	coeffs1[2] = 0.0;
	coeffs1[3] = -3.0;
	coeffs1[4] = 0.0;

	// coeffs 2:
	coeffs2[0] = -1.0;
	coeffs2[1] = 1.0;
	coeffs2[2] = -1.0;
	coeffs2[3] = 1.0;
	coeffs2[4] = -1.0;

	// coeffs 3:
	coeffs3[0] = 1.0;
	coeffs3[1] = -2.0;
	coeffs3[2] = 3.0;
	coeffs3[3] = -4.0;
	coeffs3[4] = 5.0;

	Numb rhs1 = 0.0;
	Numb rhs2 = 0.0;
	Numb rhs3 = -1.0;

	ConstArray* cArray = newConstArray(maxDim, numberOfConstraints);

	setConstraint(cArray, newConstraint(maxDim, coeffs1, rhs1, LESS_EQUAL), 0);
	setConstraint(cArray, newConstraint(maxDim, coeffs2, rhs2, LESS_EQUAL), 1);
	setConstraint(cArray, newConstraint(maxDim, coeffs3, rhs3, LESS_EQUAL), 2);

	return cArray;
}
