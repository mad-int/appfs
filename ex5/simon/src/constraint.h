#include "numb.h"

#ifndef _CONSTRAINT_H_
#define _CONSTRAINT_H_

typedef enum constraint_type {
	LESS_EQUAL, EQUAL, GREATER_EQUAL
} constType;

typedef struct {
	Numb* coeffs;
	Numb rhs;
	constType type;
	int dim;
	int nonZeros;
} Constraint;

Constraint* newConstraint(int dim, Numb* coeffs, Numb rhs, constType type);

void freeConstraint(Constraint* c);

Numb getCoefficient(Constraint* c, int index);

int checkForSingleConstraint(Constraint* c, int booleanAssignment);

#endif /* _CONSTRAINT_H_ */
