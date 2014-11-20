#include <stdlib.h>
#include <assert.h>
// local
#include "constraint.h"
#include "numb.h"
#include "allocate.h"

Constraint* newConstraint(int dim, Numb* coeffs, Numb rhs, constType type) {

	assert(dim > 0);

	unsigned int nonZeros = 0;
	unsigned int mask = 1;
	int i;

	for (i = 0; i < dim; i++) {

		if (0 != coeffs[i]) {

			nonZeros += mask;
		}
		mask = mask << 1;
	}

	Constraint* c = (Constraint*) allocate(1, sizeof(*c));
	assert(c != NULL);

	c->dim = dim;

	c->coeffs = coeffs;

	c->rhs = rhs;

	c->type = type;

	c->nonZeros = nonZeros;

	return c;
}

void freeConstraint(Constraint* c) {

	deallocate(c->coeffs);
	deallocate(c);
}

Numb getCoefficient(Constraint* c, int index) {

	assert(index >= 0);
	assert(index < c->dim);

	return c->coeffs[index];
}

int checkForSingleConstraint(Constraint* c, int booleanAssignment) {

	int nonZeroAssignment = booleanAssignment & c->nonZeros;
	int mask = 1;
	int i;
	Numb lhs = 0;
	for (i = 0; i < c->dim; i++) {

		if (0 != (mask & nonZeroAssignment)) {
			lhs += getCoefficient(c, i);
		}
		mask = mask << 1;
	}
	switch (c->type) {
	case LESS_EQUAL:
		return lhs <= c->rhs;
	case EQUAL:
		return lhs == c->rhs;
	default:
		return lhs >= c->rhs;
	}
	return lhs <= c->rhs;
}
