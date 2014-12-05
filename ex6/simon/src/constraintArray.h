#include "numb.h"

#ifndef _CONSTRAINTARRAY_H_
#define _CONSTRAINTARRAY_H_

typedef enum constraint_type {
    LESS_EQUAL, EQUAL, GREATER_EQUAL
} constType;

typedef struct {
    Numb* coeffs;
    Numb rhs;
    constType type;
    int dim;
} Constraint;

typedef struct {
    Constraint** constraints;
    int dim;
    int numberOfConstraints;
} ConstArray;

Constraint* newConstraint(int dim, Numb* coeffs, Numb rhs, constType type);

void freeConstraint(Constraint* c);

ConstArray* newConstArray(int dim, int numberOfConstraints);

void freeConstArray(ConstArray* c);

void setNumberOfConstrains(ConstArray* c, int numberOfConstraints);

int setConstraint(ConstArray* cA, Constraint* c, int index);

void printNoFeasibleAssignment();

void printAllAssignmentsFeasible(int dimension);

int preprocessConstArray(ConstArray* cA);

int calculateMaxActivityForConstraint(Constraint* c, Numb* ptrToNumb);

int calculateMinActivityForConstraint(Constraint* c, Numb* ptrToNumb);

void printAllFeasibleBinaryAssignments(ConstArray* c);

void printConstArray(ConstArray* c);

void createDummyConstArray(ConstArray** cA);

#endif /* _CONSTRAINTARRAY_H_ */
