#ifndef _LINEARPROGRAM_H
#define _LINEARPROGRAM_H


#include "linearProgram.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "allocate.h"

#define DOUBLE
#if defined DOUBLE
#define num double
#else
#define num int
#endif

//! Some enum for the type of an (in)equality
enum constraintType {LESS, EQUAL, GREATER};

// This struct represents our linearProgram
/* We save #columns, #rows, coefficient-matrix, rhs, the type of each (in)equality and the feasible solutions

*/
struct linearProgram {
	int col;
	int row;
	num** coeffs;	
	num* rhs;	
	enum constraintType* types;
	unsigned char* feasibles;
};

void printMatrix(const struct linearProgram*);
int giveFeasibles(struct linearProgram*);
void printBinaryVectors(const struct linearProgram*);
void setType(char Type, int constraintNumber, enum constraintType*);
struct linearProgram* initializeLP(int col, int row);
void deleteLinearProgram(struct linearProgram*);

#endif