#include "constraint.h"

#ifndef _CONSTRAINTARRAY_H_
#define _CONSTRAINTARRAY_H_

typedef struct {
	Constraint** constraints;
	int dim;
	int numberOfConstraints;
} ConstArray;

ConstArray* newConstArray(int dim, int numberOfConstraints);

void freeConstArray(ConstArray* c);

void setConstraint(ConstArray* cA, Constraint* c, int index);

void printAllFeasibleBinaryAssignments(ConstArray* c);

ConstArray* read_constArray_from_file(const char* filename);

int printConstArray(ConstArray* c);

ConstArray* createDummyConstArray();

#endif /* _CONSTRAINTARRAY_H_ */
