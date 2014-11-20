#ifndef _PROBDATA_H_
#define _PROBDATA_H_

#include <stdbool.h>

typedef struct matrix
{
   int    size;
   int    redundant;
   int    m;
   int    n;
   float* coefs;
   float* rhs;
} Matrix;

/* creates a new matrix */
extern Matrix* matrix_new(int m, int n);
/* frees matrix data */
extern void matrix_free(Matrix* matrix);
/* adds a new row (constraint) */
extern bool matrix_put(Matrix* matrix, float* coefs, float rhs);
/* gets the number of rows (constraints) */
extern int matrix_getM(Matrix* matrix);
/* gets the number of columns (variables) */
extern int matrix_getN(Matrix* matrix);
/* gets entries of the matrix */
extern float* matrix_getCoefs(Matrix* matrix);
/* gets right-hand sides of the constraints */
extern float* matrix_getRhs(Matrix* matrix);
/* gets maximal number of rows (constraints) */
extern int matrix_getSize(Matrix* matrix);
/* determines whether the matrix is empty */
extern bool matrix_is_empty(const Matrix* matrix);

#endif /* _PROBDATA_H_ */