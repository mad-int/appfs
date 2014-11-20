#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h> // strpbrk
#include <ctype.h>  // isspace
// local
#include "allocate.h"

#define MAX_LINE_LEN 512 // Maximum input line length

typedef struct {
	int *coeffs;
	int rhs;
	int dim;
	int nonZeros;
} Constraint;

typedef struct {
	Constraint **constraints;
	int dim;
	int numberOfConstraints;
} ConstArray;

Constraint *newConstraint(int dim, int *coeffs, int rhs) {

	assert(dim > 0);

	unsigned int nonZeros = 0;
	unsigned int mask = 1;
	int i;

	for (i = 0; i < dim; i++) {

		if (coeffs[i] != 0) {

			nonZeros += mask;
		}
		mask = mask << 1;
	}

	Constraint *c = (Constraint *) allocate(1, sizeof(*c));
	assert(c != NULL);

	c->dim = dim;

	c->coeffs = coeffs;

	c->rhs = rhs;

	c->nonZeros = nonZeros;

	return c;
}

void freeConstraint(Constraint *c) {

	deallocate(c->coeffs);
	deallocate(c);
}

ConstArray *newConstArray(int dim, int numberOfConstraints) {

	assert(numberOfConstraints > 0);
	assert(dim > 0);

	ConstArray *c = (ConstArray *) allocate(1, sizeof(*c));
	assert(c != NULL);

	c->dim = dim;

	c->numberOfConstraints = numberOfConstraints;

	Constraint **constraints = (Constraint **) allocate(numberOfConstraints,
			sizeof(*constraints));
	assert(constraints != NULL);

	c->constraints = constraints;
	return c;
}

void freeConstArray(ConstArray *c) {

	int i;
	for (i = 0; i < c->numberOfConstraints; i++) {
		freeConstraint(c->constraints[i]);
	}
	deallocate(c);
}

void setConstraint(ConstArray *cA, Constraint *c, int index) {

	assert(index >= 0);
	assert(index < cA->numberOfConstraints);
	assert(cA->dim==c->dim);

	cA->constraints[index] = c;
}

int getCoefficient(Constraint *c, int index) {

	assert(index >= 0);
	assert(index < c->dim);

	return c->coeffs[index];
}

int checkForSingleConstraint(Constraint *c, int booleanAssignment) {

	int nonZeroAssignment = booleanAssignment & c->nonZeros;
	int mask = 1;
	int i;
	int lhs = 0;
	for (i = 0; i < c->dim; i++) {

		if ((mask & nonZeroAssignment) != 0) {
			lhs += getCoefficient(c, i);
		}
		mask = mask << 1;
	}
	return lhs <= c->rhs;
}

int checkForAllConstraints(ConstArray *c, int booleanAssignment) {

	int i;
	for (i = 0; i < c->numberOfConstraints; i++) {

		if (!(checkForSingleConstraint(c->constraints[i], booleanAssignment))) {
			return 0;
		}
	}
	return 1;
}

char *getBinaryFromInt(int binaryAssignment, int length) {

	char *string = allocate(length, sizeof(string));

	int mask = 1;
	int i;
	for (i = 0; i < length; i++) {
		string[i] = ((mask & binaryAssignment) == 0) ? '0' : '1';
		mask = mask << 1;
	}
	return string;
}

void printConstArray(ConstArray *c) {

	int i, j;

	printf("Printing constraint array:\n");
	printf("dimension: %i\n", c->dim);
	printf("number of constraints: %i\n", c->numberOfConstraints);
	printf("Constraints:\n");
	for (i = 0; i < c->numberOfConstraints; i++) {
		Constraint *constraint = c->constraints[i];
		for (j = 0; j < c->dim; j++) {
			printf(" + %i x%i", getCoefficient(constraint,j), j);
		}
		printf(" <= %i\n", constraint->rhs);
	}
}

void printAllFeasibleBinaryAssignments(ConstArray *c) {

	// assuming a 64bit system
	long testBinary;
	long upperBound = 1L << (c->dim);
	int feasSolutions = 0;

	for (testBinary = 0; testBinary < upperBound; testBinary++) {
		char* string;
		string = getBinaryFromInt(testBinary, c->dim);

		if (checkForAllConstraints(c, testBinary)) {
			printf("Binary assignment %s is valid!\n", string);
			feasSolutions = 1;
		}
		deallocate(string);
	}
	if (!feasSolutions) {
		printf("No feasible solution found!\n");
	}
}

ConstArray* createDummyConstArray() {

	int i;
	int maxDim = 5;
	int numberOfConstraints = 3;

	int* coeffs1 = (int *) allocate(maxDim , sizeof(coeffs1));
	int* coeffs2 = (int *) allocate(maxDim , sizeof(coeffs2));
	int* coeffs3 = (int *) allocate(maxDim , sizeof(coeffs3));
	// coeffs 1:
	coeffs1[0] = 1;
	coeffs1[1] = 2;
	coeffs1[2] = 0;
	coeffs1[3] = -3;
	coeffs1[4] = 0;

	// coeffs 2:
	coeffs2[0] = -1;
	coeffs2[1] = 1;
	coeffs2[2] = -1;
	coeffs2[3] = 1;
	coeffs2[4] = -1;

	// coeffs 3:
	coeffs3[0] = 1;
	coeffs3[1] = -2;
	coeffs3[2] = 3;
	coeffs3[3] = -4;
	coeffs3[4] = 5;

	int rhs1 = 0;
	int rhs2 = 0;
	int rhs3 = -1;

	ConstArray *cArray = newConstArray(maxDim, numberOfConstraints);

	setConstraint(cArray, newConstraint(maxDim, coeffs1, rhs1), 0);
	setConstraint(cArray, newConstraint(maxDim, coeffs2, rhs2), 1);
	setConstraint(cArray, newConstraint(maxDim, coeffs3, rhs3), 2);

	return cArray;
}

ConstArray* read_constArray_from_file(const char* filename) {
	assert(NULL != filename);
	assert(0 < strlen(filename));

	// given data for file reading
	FILE* fp;
	char buf[MAX_LINE_LEN];
	char* s;
	int lines = 0;

	// own data for constraint array bulding
	ConstArray *c;
	int dim = -1;
	int numberOfConstraints = -1;
	int constraintIndex = 0;
	int dimensionIndex;
	int *coeffs;
	int rhs;
	char *delim = " ";
	char *token;
	char *debug;

	int coefficient;

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

		if (dim == -1) {
			dim = strtol(s, &debug, 10);
			if (strcmp(debug, "\0") != 0 && strcmp(debug, " ") != 0) {
				fprintf(stderr,
						"Invalid data file %s: Invalid dimension definition! String after first number (which has to be '' or ' '): '%s'\n",
						filename, debug);
				return NULL;
			}
			if (dim > 0 && dim < 33) {
				continue;
			} else {
				fprintf(stderr,
						"Invalid data file %s: No positive number of variables less or equal to 33 is given!\n",
						filename);
				return NULL;
			}
		}
		if (numberOfConstraints == -1) {
			numberOfConstraints = strtol(s, &debug, 10);
			if (strcmp(debug, "\0") != 0 && strcmp(debug, " ") != 0) {
				fprintf(stderr,
						"Invalid data file %s: Invalid definition of the number of constraints! String after first number (which has to be '' or ' '): '%s'\n",
						filename, debug);
				return NULL;
			}
			if (numberOfConstraints > 0 && dim < 33) {
				c = newConstArray(dim, numberOfConstraints);
				continue;
			} else {
				fprintf(stderr,
						"Invalid data file %s: No positive number of constraints less or equal to 33 is given!\n",
						filename);
				return NULL;
			}
		}

		dimensionIndex = 0;
		coeffs = (int *) allocate(dim , sizeof(coeffs));

		if (constraintIndex >= numberOfConstraints) {
			fprintf(stderr, "Invalid data file %s: To many constraints!\n",
					filename);
			return NULL;
		}

		token = strtok(s, delim);
		if (strcmp(token, "<=") != 0) {
			coeffs[dimensionIndex] = strtol(token, NULL, 10);
			dimensionIndex++;
		} else {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: No variables given!\n",
					filename, constraintIndex);
			return NULL;
		}

		// while there are tokens and the token is not "<="
		while (token != NULL && strcmp(token, "<=") != 0) {
			// next token
			token = strtok(NULL, delim);
			if (strcmp(token, "<=") != 0) {
				coeffs[dimensionIndex] = strtol(token, NULL, 10);
				dimensionIndex++;
			}
		}
		/// token should be "<=" now
		if (dimensionIndex != dim) {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: Incorrect number of variables!\n",
					filename, constraintIndex);
			return NULL;
		}
		// rhs token
		token = strtok(NULL, delim);
		if (token != NULL) {
			rhs = strtol(token, NULL, 10);
		} else {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: No right-hand-side given!\n",
					filename, constraintIndex);
			return NULL;
		}
		if (strtok(NULL, delim) != NULL) {
			fprintf(stderr,
					"Invalid data file %s: Error creating constraint %i: Multiple right-hand-sides given!\n",
					filename, constraintIndex);
			return NULL;
		}
		setConstraint(c, newConstraint(dim, coeffs, rhs), constraintIndex);
		constraintIndex++;
	}
	fclose(fp);

	if (constraintIndex != numberOfConstraints) {
		fprintf(stderr, "Invalid data file %s: Not enough constraints!\n",
				filename);
		return NULL;
	}

	return c;
}

int main(int argc, char** argv) {
	ConstArray *c;
	if (argc < 2 || strlen(argv[1]) <= 0) {
		printf("usage: %s filename\n", argv[0]);
		printf("Reading dummy data instead!\n");
		c = createDummyConstArray();
		printConstArray(c);
	} else {
		c = read_constArray_from_file(argv[1]);
	}

	if (c == NULL) {
		return EXIT_FAILURE;
	}
	printAllFeasibleBinaryAssignments(c);
	freeConstArray(c);

	return EXIT_SUCCESS;
}
