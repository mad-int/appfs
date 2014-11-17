#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "allocate.h"
#include "probdata.h"

/* determines whether the matrix data is valid */
static bool matrix_is_valid(const Matrix* matrix)
{
   return matrix != NULL
      && matrix->n > 0 && matrix->m >= 0
      && matrix->size > 0 && matrix->m <= matrix->size
      && matrix->coefs != NULL && matrix->rhs != NULL;
}

/* creates a new matrix */
Matrix* matrix_new(int m, int n)
{
   Matrix* matrix;

   assert(m > 0);
   assert(n > 0);

   matrix = allocate(1, sizeof(*matrix));

   matrix->m = 0;
   matrix->n = n;
   matrix->size = m;
   matrix->redundant = 0;
   matrix->coefs = allocate(matrix->size * matrix->n, sizeof(*matrix->coefs));
   matrix->rhs = allocate(matrix->size, sizeof(*matrix->rhs));
   assert(matrix_is_valid(matrix));

   return matrix;
}

/* frees matrix data */
void matrix_free(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   deallocate(matrix->rhs);
   deallocate(matrix->coefs);
   deallocate(matrix);
}

/* adds a new row (constraint) */
bool matrix_put(Matrix* matrix, float* coefs, float rhs)
{
   assert(matrix_is_valid(matrix));

   /* abort if more rows (constraints) wants to be added than announced */
   if (matrix->m == matrix->size)
   {
      fprintf(stderr, "Matrix overflow size=%d\n", matrix->m);
      abort();
   }
   assert(matrix->m < matrix->size);

   /* compute maximal and minimal activity */
   float max = 0.0;
   float min = 0.0;
   for (int j = 0; j < matrix->n; j++)
   {
      if (coefs[j] > 0)
         max += coefs[j];
      else
      {
         if (coefs[j] < 0)
            min += coefs[j];
      }
   }

   /* constraint is redundant, when maximal activity is smaller or equal to rhight-hand side
    * => constraint is not added
    */
   if (max <= rhs)
   {
#ifdef debug
      printf("redundant constraint.\n");
#endif
      matrix->redundant++;
      return true;
   }

   /* constraint is infeasible, when minimal activity is greater than rhight-hand side
    * => return infeasible
    */
   if (min >= rhs)
   {
#ifdef debug
      printf("Reading detected infeasibility.\n");
#endif
      matrix->redundant++;
      return false;
   }

   /* check for redundancy (multiple of other constraint)
    * => constraint is not added, when it is redundant
    */
   for (int i = 0; i < matrix->m; i++)
   {
      if ((rhs == 0 && matrix->rhs[i] == 0) || (rhs != 0 && matrix->rhs[i] != 0))
      {
         float c = 0;
         bool set = false;

         if (matrix->rhs[i] != 0 && rhs !=0)
         {
            c = rhs / matrix->rhs[i];
            set = true;
         }
         int j;
         for (j = 0; j < matrix->n; j++)
         {
            if ((coefs[j] != 0 && matrix->coefs[i*matrix->n+j] == 0) || (coefs[j] == 0 && matrix->coefs[i*matrix->n+j] != 0))
               break;
            if (coefs[j] != 0 && matrix->coefs[i*matrix->n+j] != 0)
            {
               if (!set)
               {
                  c = coefs[j] / matrix->coefs[i*matrix->n+j];
                  set = true;
               }
               else
               {
                  if (c != coefs[j] / matrix->coefs[i*matrix->n+j])
                     break;
               }
            }
         }
         if (j == matrix->n)
         {
#ifdef debug
            printf("redundant constraint.\n");
#endif
            matrix->redundant++;
            return true;
         }
      }
   }

   /* add row (constraint) to matrix */
   for (int i = 0; i < matrix->n; i++)
      matrix->coefs[matrix->m * matrix->n + i] = coefs[i];
   matrix->rhs[matrix->m] = rhs;
   matrix->m++;

   assert(matrix_is_valid(matrix));
   return true;
}

/* gets the number of rows (constraints) */
int matrix_getM(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->m;
}

/* gets the number of columns (variables) */
int matrix_getN(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->n;
}

/* gets entries of the matrix */
float* matrix_getCoefs(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->coefs;
}

/* gets right-hand sides of the constraints */
float* matrix_getRhs(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->rhs;
}

/* gets maximal number of rows (constraints) */
int matrix_getSize(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->size;
}

/* determines whether the matrix is empty */
bool matrix_is_empty(const Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->m == 0;
}