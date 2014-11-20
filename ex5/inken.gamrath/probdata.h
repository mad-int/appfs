#ifndef _PROBDATA_H_
#define _PROBDATA_H_

#include <stdbool.h>

#include "template.h"

typedef struct MATRIX
{
   int    size;
   int    redundant;
   int    m;
   int    n;
   TYPE* coefs;
   TYPE* rhs;
} Matrix;

/* creates a new matrix */
extern Matrix* matrix_new(int m, int n);
/* frees matrix data */
extern void matrix_free(Matrix* matrix);
/* adds a new row (constraint) */
extern bool matrix_put(Matrix* matrix, TYPE* coefs, TYPE rhs);
/* gets the number of rows (constraints) */
extern int matrix_getM(Matrix* matrix);
/* gets the number of columns (variables) */
extern int matrix_getN(Matrix* matrix);
/* gets entries of the matrix */
extern TYPE* matrix_getCoefs(Matrix* matrix);
/* gets right-hand sides of the constraints */
extern TYPE* matrix_getRhs(Matrix* matrix);
/* gets maximal number of rows (constraints) */
extern int matrix_getSize(Matrix* matrix);
/* gets redundant number of rows (constraints) */
extern int matrix_getRedundant(Matrix* matrix);
/* determines whether the matrix is empty */
extern bool matrix_is_empty(const Matrix* matrix);
/* solves the LP */
extern bool solveLP(Matrix* matrix, FILE* fp);

#endif /* _PROBDATA_H_ */