#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h> // pow

#include "allocate.h"
#include "probdata.h"

// #define debug

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
   matrix->size = 2*m;
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
bool matrix_put(Matrix* matrix, TYPE* coefs, TYPE rhs)
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
   TYPE max = 0.0;
   TYPE min = 0.0;
#ifdef debug
   fprintf(stdout, "Add consttraint: ");
   char* str;
   str = allocate(MAX_STR_LEN, sizeof(*str));
#endif
   for (int j = 0; j < matrix->n; j++)
   {
      if (coefs[j] > 0)
         max += coefs[j];
      else
      {
         if (coefs[j] < 0)
            min += coefs[j];
      }
#ifdef debug
      vtostr(str, coefs[j]);
      fprintf(stdout, "%s ", str);
#endif
   }
#ifdef debug
   vtostr(str, rhs);
   fprintf(stdout, "<= %s\n", str);
   deallocate(str);
#endif

   /* constraint is redundant, when maximal activity is smaller or equal to rhight-hand side
    * => constraint is not added
    */
   if (max <= rhs)
   {
#ifdef debug
      printf("Constraint is redundant.\n");
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
         double c = 0;
         bool set = false;

         int j = 0;
         if (matrix->rhs[i] != 0 && rhs !=0)
         {
            c = rhs / matrix->rhs[i];
            set = true;
            if (c < 0)
            {
               set = false;
               j = matrix->n + 1;
            }
         }

         for (; j < matrix->n; j++)
         {
            if ((coefs[j] != 0 && matrix->coefs[i*matrix->n+j] == 0) || (coefs[j] == 0 && matrix->coefs[i*matrix->n+j] != 0))
               break;
            if (coefs[j] != 0 && matrix->coefs[i*matrix->n+j] != 0)
            {
               if (!set)
               {
                  c = coefs[j] / matrix->coefs[i*matrix->n+j];
                  set = true;
                  if (c < 0)
                  {
                     set = false;
                     break;
                  }
               }
               else
               {
                  if (c != coefs[j] / matrix->coefs[i*matrix->n+j])
                     break;
               }
            }
         }
         if (j == matrix->n && set)
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
TYPE* matrix_getCoefs(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->coefs;
}

/* gets right-hand sides of the constraints */
TYPE* matrix_getRhs(Matrix* matrix)
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

/* gets redundant number of rows (constraints) */
int matrix_getRedundant(Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->redundant;
}

/* determines whether the matrix is empty */
bool matrix_is_empty(const Matrix* matrix)
{
   assert(matrix_is_valid(matrix));

   return matrix->m == 0;
}

/* solves the LP
 * returns true, if a solution is found
 * returns false otherwise
 */
bool solveLP(Matrix* matrix, FILE* fp)
{
   int* values;
   bool valid;
   bool success;
   long int length;
   double sum;
   long int p;

   assert(matrix_is_valid(matrix));

   values = allocate(matrix->n, sizeof(*values));

   success = false;
   length = pow(2, matrix->n);
   clock_t start = clock();
   for (long int i = 0; i < length; i++)
   {
//       p = 1;
      p = i;
      for (int k = 0; k < matrix->n; k++)
      {
//          values[k] = (i / p) % 2;
//          p *= 2;
         values[k] = p % 2;
         p /= 2;
      }
      valid = true;
      for (int j = 0; j < matrix->m; j++)
      {
         sum = 0.0;
         for (int k = 0; k < matrix->n; k++)
         {
            sum += matrix->coefs[j*matrix->n+k] * values[k];
         }
         if (sum > matrix->rhs[j])
         {
            valid = false;
            break;
         }
      }
#ifndef debug
      if (valid)
      {
#endif
         success = true;
         for (int k = 0; k < matrix->n; k++)
         {
            fprintf(fp, "%i", values[k]);
            if (k < matrix->n-1)
            {
               fprintf(fp, " ");
            }
            else
            {
#ifndef debug
               fprintf(fp, "\n");
#else
               fprintf(fp, ": %i\n", valid);
#endif
            }
         }
#ifndef debug
      }
#endif
   }
   fprintf(stdout, "Enumeration time: %.2f seconds.\n", GET_SEC(start, clock()));

   deallocate(values);
   assert(matrix_is_valid(matrix));

   return success;
}
