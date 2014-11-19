#include "linearProgram.h"


//! Function prints the coefficient matrix of the linear program
/*
	\param lp the linearProgram which constraint matrix shall be printed
*/
void printMatrix(const struct linearProgram* lp) {
	for(int i = 0; i < lp->row; ++i) {
		for(int j = 0; j < lp->col; ++j)
		{
			printf("%f ", lp->coeffs[i][j]);
		}
		printf("\n");
	}
}

//! computes all feasible 0-1 solutions of a linear program
/*!
	sets the array feasibles at index i to 1 if 
	the binary representation of i seen as indexvector is a feasible solution
	\param ilp is some struct of linearProgram [READ-ONLY]
	\param feasibles the array where all incidence-vectors of feasible solutions are svaed
	\return the number of feasible solutions to the lienar program
*/
int giveFeasibles(struct linearProgram* binaryLP) {
				
	int counter = 0; // counts
	char x_is_valid;
	num row_result;
	// initialize array of chars - we need one for each possible vector x

	//for each incidencevector
	for(unsigned int j = 0; j < (1u << binaryLP->col); ++j) {
		x_is_valid = 1;
		// each row must be feasible...
		for(int i = 0; i < binaryLP->row && x_is_valid; ++i) {
			row_result = 0;
			// so we add the element of the binaryLP.coeffs belonging to row i and column k 
			// only if the 
			for(int k = 0; k < binaryLP->col; ++k) {
				// if index vector j has a "1" at position "i"
				if (j & (1u << k)) {
					row_result += binaryLP->coeffs[i][k];
				}
			}
			
			if (((row_result > binaryLP->rhs[i]) && ((binaryLP->types[i] == LESS)
				|| (binaryLP->types[i] == EQUAL)))
				 			|| ((row_result < binaryLP->rhs[i]) && ((binaryLP->types[i] == GREATER)
								|| (binaryLP->types[i] == EQUAL))))
			{
				// too bad
				x_is_valid = 0;
				// try next vector
			}
		}
		if (x_is_valid) {
			counter++;
			binaryLP->feasibles[j] = 1;
		}
	}
	return counter;
}

//! prints alle feasible vectors
/* prints all feasible vectors
	\param vectors 0-1-array, vectors[i] = 1 indicates 
		that the bit representation of i is some feasible solution
	\param vector_size number of different vectors
	\param vector_dim dimension of one vector (n) 
*/
void printBinaryVectors(const struct linearProgram* binaryLP) {
	int counter = 0;
	for(unsigned int j = 0; j < (1u << binaryLP->col); ++j) {
		//vectors[j] is set to one -> bit-representation of j is feasible vector
		if (binaryLP->feasibles[j]) {
			counter++;
			for(int i = 0; i < binaryLP->col; ++i) {
				// Transform *vectors to int for bitwise operations
				if (j & (1u << i)) {
					printf("x_%d = 1\n", i + 1);
				}
				else {
					printf("x_%d = 0\n", i + 1);
				}
			}
			printf("\n");
		}
	}
}

//! setter for some type in a constraintType array
/* 
	converts the sign <, = or > to a constraintType and saves it 
	at position constraintNumber in array cType
	\param type the sign corresponding to type of inequality
	\param constraintNumber the row corresponding to the inequality
	\param cType the array which shall - in the end - contain the constraintTypes of alle rows
*/

void setType(char type, int constraintNumber, enum constraintType* cType ) {
	switch(type) {
		case '<': cType[constraintNumber] = LESS; break;
		case '=': cType[constraintNumber] = EQUAL; break;
		case '>': cType[constraintNumber] = GREATER; break;
		default: assert(0!=0);
	}
}

//! allocates the necessary memory
/*
	initializes a new struct with all required memory according to number of columns/rows

	\param col the number of columns the new LP shall have
	\param row the number of rows the new LP shall have
	\return a pointer to the new initiliazed struct linearProgram
*/
struct linearProgram* initializeLP(int col, int row) {
	//Dimension should be checked previously
	if (!(col > 0 && col <=32 && row > 0 && row <= 32)) {
		return NULL;
	}
	struct linearProgram* binaryLP = allocate(1,sizeof(*binaryLP));
	binaryLP->col = col;
	binaryLP->row = row;
	binaryLP->coeffs = allocate(row, sizeof(*(binaryLP->coeffs)));
	binaryLP->feasibles = allocate(1u << binaryLP->col, sizeof(*(binaryLP->feasibles)));

	// allocate array for coefficient (2-D wow!)
	for(int i = 0; i < binaryLP->row; ++i)
	{
		binaryLP->coeffs[i] = allocate(col, sizeof(*(binaryLP->coeffs[i])));
	}
	binaryLP->rhs = allocate(row, sizeof(*(binaryLP->rhs)));
	binaryLP->types = allocate(row, sizeof(*(binaryLP->types)));
	
	return binaryLP;
	
}

//! frees all space allocated in initializeLP
/* Gives back the memory that was allocated during initialization of the linearProgram
	\param linearProgram* pointer to the LP that shall be deleted
*/

void deleteLinearProgram(struct linearProgram* lp) {
	assert(NULL != lp);
	// free pointer:
	for(int i = 0; i < lp->row; ++i) {
		free(lp->coeffs[i]);
	}
	deallocate(lp->coeffs);
	deallocate(lp->rhs);
	deallocate(lp->feasibles);
	deallocate(lp);
}