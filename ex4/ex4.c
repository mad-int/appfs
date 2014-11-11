#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define BUF_SIZE 512
#define USAGE "file must consist of: \n#rows #columns constraints, e.g.:\n\n\
3 # columns (variables)\n5 # rows (constraints)\n2 3 -5 4 5 <= 8\n3 6 0 8 6 <= 10\
\n0 0 1 1 7 <= 1\n"

int fillRow(int* row, char* inputString, char* delimiter);
void printMatrix(int** matrix, int rows, int columns);
unsigned char* giveFeasibles(int** matrix, int rows, int columns, int* rhs);
void printBitRepresentation(unsigned char* vectors, int vector_size, int vector_dim);
static unsigned int powers[32] = {1,2,4,8,16,32,64,128,256,512,1024,
								2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 
								2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 
								134217728, 268435456, 536870912, 1073741824, 2147483648};

// This method reads from filename the constraints of a LP
// The format of the file has to be similar to the following:
/*
								
								
3 # columns (variables)
5 # rows (constraints)
2 3 -5 4 5 <= 8
3 6 0 8 6 <= 10
0 0 1 1 7 <= 1
								
								
*/					
int readConstraints(const char* filename) {
	
	assert(NULL != filename);
	FILE* fp;
	char buffer[BUF_SIZE];
	char* s;
	int lines = 0;
	int dimension[2];
	int** matrix;
	int* b;
	//printf("test");
	
	
	if (NULL == (fp = fopen(filename, "r"))) {
		fprintf(stderr, "Can't open file %s\n",filename);
		printf(USAGE);
		return -1;
	}
	
	while(NULL != (s = fgets(buffer, sizeof(buffer), fp))) {
		char* end_pos = strpbrk(s, "#\n\r");
		*end_pos = '\0'; // terminate string here, now we can process this line
		
		/* Skip over leading space
		*/
		while(isspace(*s)) 
			s++;
		
		/* Skip over empty lines
		*/
		if (!*s)
			continue;
		
		if (lines < 2) {
			if (!(dimension[lines] = atoi(s))) {
				fprintf(stderr, "Can't parse %d-th matrix-dimension!\n",lines+1);
				printf(USAGE);
				return -1;
			}
			if (lines == 1) {
				/* ready to initialize matrix and rhs-vector */
				matrix = calloc(dimension[1], sizeof(*matrix));
				
				for(int i = 0; i < dimension[1]; ++i)
				{
					matrix[i] = calloc(dimension[0], sizeof(*matrix[i]));
				}
				b = calloc(dimension[1], sizeof(*b));
			}
		}
		else {
			char* help_me = calloc(BUF_SIZE, sizeof(*help_me));
			strcpy(help_me, s);
			if (dimension[0] != fillRow(matrix[lines - 2], strtok(s,"<="), " ")) {
				// free pointer:
				for(int i = 0; i < dimension[1]; ++i) {
					free(matrix[i]);
				}
				free(matrix);
				free(b);
				printf(USAGE);
				return -1;
			}
			char* test = strtok(help_me, "<=");
			test = strtok(NULL, "<=");

			b[lines - 2] = atoi(test);
			free(help_me);
		}
		lines++;
	}
	
	//printMatrix(matrix, dimension[1], dimension[0]);
	unsigned char* feasibles = giveFeasibles(matrix, dimension[1], dimension[0], b);
	printBitRepresentation(feasibles, powers[dimension[0]],dimension[0]);
	
	// free pointer:
	for(int i = 0; i < dimension[1]; ++i) {
		free(matrix[i]);
	}
	free(matrix);
	free(b);
	// Not good to free feasibles here... sorry
	free(feasibles);
	
	fclose(fp);
	return lines;
}


int main(int argc, char** argv)
{
   if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }
	readConstraints(argv[1]);
   
   return EXIT_SUCCESS;
}

// test function for printing some 2D-array
void printMatrix(int** matrix, int rows, int columns) {
	for(int i = 0; i < rows; ++i) {
		for(int j = 0; j < columns; ++j)
		{
			printf("%d ", matrix[i][j]);
		}
		printf("\n");
	}
}

// @param vector_size number of different vectors
// @param vector_dim dimension of one vector (n) 
void printBitRepresentation(unsigned char* vectors, int vector_size, int vector_dim) {
	int counter = 0;
	for(unsigned int j = 0; j < vector_size; ++j) {
		if (*(vectors + j)) {
			counter++;
			for(int i = 0; i < vector_dim; ++i) {
				// Transform *vectors to int for bitwise operations
				if (j & powers[i]) {
					printf("x_%d = 1\n", i + 1);
				}
				else {
					printf("x_%d = 0\n", i + 1);
				}
			}
			printf("\n");
		}
	}
	printf("\nWe got %d feasible solutions in total!\n", counter); 
}

// gives back some char array where the i-th entry set to 1 indicates
// that his bit-representation is a feasible assignement to x
unsigned char* giveFeasibles(int** matrix, int rows, int columns, int* rhs) {
				
	char x_is_valid;
	int row_result;
	// initialize array of chars - we need one for each possible vector x
	unsigned char* index_vector = calloc(powers[columns],sizeof(*index_vector));

	//for each incidencevector
	for(unsigned int j = 0; j < powers[columns]; ++j) {
		x_is_valid = 1;
		// each row must be feasible...
		for(int i = 0; i < rows && x_is_valid; ++i) {
			row_result = 0;
			// so we add the element of the matrix belonging to row i and column k 
			// only if the 
			for(int k = 0; k < columns; ++k) {
				// if index vector j has a "1" at position "i"
				if (j & powers[k]) {
					row_result += matrix[i][k];
				}
			}
			if (row_result > rhs[i]) {
				// too bad
				x_is_valid = 0;
				// try next vector
			}
		}
		if (x_is_valid) {
			*(index_vector + j) = 1;
		}
	}
	return index_vector;
}

/**
Fills some array row with numbers contained in inputString.
The numbers must be seperated by a delimiter
RETURN: number of decimals read
**/
int fillRow(int* row, char* inputString, char* delimiter) {
	int i = 0;
	assert(NULL != inputString && NULL != row);
	// Seperate each number
	char* value = strtok(inputString, delimiter);
	while (NULL != value) {
		row[i] = atoi(value);
		i++;
		// Next number
		value = strtok(NULL, delimiter);
	}
	return i;
}