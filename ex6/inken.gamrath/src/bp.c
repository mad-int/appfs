#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h> // pow
#include <string.h> //TODO kann wieder raus

#include "allocate.h"
#include "misc.h"
#include "bp.h"

// #define DEBUG
// #define MORE_DEBUG

/*
 * determines whether the binary program data is valid
 */
static bool bp_is_valid(
   const BinaryProgram* bp          /**< binary program */
)
{
   return bp != NULL
      && bp->n > 0 && bp->m >= 0
      && bp->size > 0 && bp->m <= bp->size
      && bp->coefs != NULL && bp->rhs != NULL;
}

/*
 * creates a new binary program
 */
BinaryProgram* bp_new(
   int m,                           /**< dimension of rows */
   int n                            /**< dimension of columns */
)
{
   BinaryProgram* bp;

   assert(m > 0);
   assert(n > 0);

   bp = allocate(1, sizeof(*bp));

   bp->m = 0;
   bp->n = n;
   bp->size = 2*m;
   bp->redundant = 0;
   bp->coefs = allocate(bp->size * bp->n, sizeof(*bp->coefs));
   bp->rhs = allocate(bp->size, sizeof(*bp->rhs));
   assert(bp_is_valid(bp));

   return bp;
}

/*
 * frees binary program data
 */
void bp_free(
   BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   deallocate(bp->rhs);
   deallocate(bp->coefs);
   deallocate(bp);
}

/*
 * adds a new row (constraint)
 */
BP_RETCODE bp_put(
   BinaryProgram* bp,          /**< binary program */
   TYPE* coefs,                /**< array of coefficients */
   TYPE rhs                    /**< right hand side */
)
{
   assert(bp_is_valid(bp));

   /* abort if more rows (constraints) wants to be added than announced */
   if (bp->m == bp->size)
   {
      fprintf(stderr, "BinaryProgram overflow size=%d\n", bp->m);
      abort();
   }
   assert(bp->m < bp->size);

   /* compute maximal and minimal activity */
   TYPE max = 0;
   TYPE min = 0;
#ifdef MORE_DEBUG
   fprintf(stdout, "Add consttraint: ");
   char* str;
   str = allocate(MAX_STR_LEN, sizeof(*str));
#endif
   for (int j = 0; j < bp->n; j++)
   {
      if (coefs[j] > 0)
      {
         max += coefs[j];
         if (max < coefs[j])
         {
            printf("Reading detected possible overflow.\n");
            return BP_ERROR;
         }
      }
      else
      {
         if (coefs[j] < 0)
         {
            min += coefs[j];
            if (min > coefs[j])
            {
               printf("Reading detected possible underflow.\n");
               return BP_ERROR;
            }
         }
      }
#ifdef MORE_DEBUG
      if (j > 0)
         fprintf(stdout, "+ ");
      vtostr(str, coefs[j]);
      fprintf(stdout, "%s x_%i ", str, j+1);
#endif
   }
#ifdef MORE_DEBUG
   vtostr(str, rhs);
   fprintf(stdout, "<= %s\n", str);
   deallocate(str);
#endif

   /* constraint is redundant, when maximal activity is smaller or equal to rhight-hand side
    * => constraint is not added
    */
   if (max <= rhs)
   {
#ifdef MORE_DEBUG
      printf("Constraint is redundant.\n");
#endif
      bp->redundant++;
      return BP_OKAY;
   }

   /* constraint is infeasible, when minimal activity is greater than rhight-hand side
    * => return infeasible
    */
   if (min > rhs)
      return BP_INFEASIBLE;

   /* check for redundancy (multiple of other constraint)
    * => constraint is not added, when it is redundant
    */
   for (int i = 0; i < bp->m; i++)
   {
      if ((rhs == 0 && bp->rhs[i] == 0) || (rhs != 0 && bp->rhs[i] != 0))
      {
         double quotposmax = 0;
         double quotposmin = 0;
         double quotnegmax = 0;
         double quotnegmin = 0;
         double quot = 0;
         int dom = 0;

         int j = 0;
         for (; j < bp->n; j++)
         {
            /* nothing to do, when both coefficients are zero */
            if (coefs[j] == 0 && bp->coefs[j] == 0)
               continue;
            /* break, when one coefficient is zero and the other is negative */
            else if ((coefs[j] == 0 && bp->coefs[j] < 0) || (coefs[j] < 0 && bp->coefs[j] == 0))
               break;
            /* only stored constraint can dominates new one, when coefficient of new one is zero */
            else if (coefs[j] == 0 && bp->coefs[j] > 0)
            {
               if (dom == -1 || quot != 0)
                  break;
               dom = 1;
            }
            /* only new constraint can dominates stored one, when coefficient of stored one is zero */
            else if (coefs[j] > 0 && bp->coefs[j] == 0)
            {
               if (dom == 1 || quot != 0)
                  break;
               dom = -1;
            }
            else
            {
               assert(coefs[j] != 0);
               assert(bp->coefs[j] != 0);
               double q = coefs[j] / bp->coefs[i*bp->n+j];

               /* compute quotient for positive coefficients */
               if (coefs[j] > 0 && bp->coefs[i*bp->n+j] > 0)
               {
                  if (quot != 0)
                     break;
                  if (quotposmax == 0)
                  {
                     assert(quotposmin == 0);
                     quotposmax = q;
                     quotposmin = q;
                  }
                  else
                  {
                     quotposmax = MAX(quotposmax, q);
                     quotposmin = MIN(quotposmin, q);
                  }
                  if (quotnegmax != 0 && ((dom == 1 && quotposmax > quotnegmin) || (dom == -1 && quotposmin > quotnegmax)))
                     break;
               }
               /* compute quotient for negative coefficients */
               else if (coefs[j] < 0 && bp->coefs[i*bp->n+j] < 0)
               {
                  if (quot != 0)
                     break;
                  if (quotnegmax == 0)
                  {
                     assert(quotnegmin == 0);
                     quotnegmax = q;
                     quotnegmin = q;
                  }
                  else
                  {
                     quotnegmax = MAX(quotnegmax, q);
                     quotnegmin = MIN(quotnegmin, q);
                  }
                  if (quotposmax != 0 && ((dom == 1 && quotposmax > quotnegmin) || (dom == -1 && quotposmin > quotnegmax)))
                     break;
               }
               /* compute quotient for coefficients with different sign */
               else
               {
                  if (quotnegmax != 0 || quotposmax != 0 || dom != 0)
                     break;
                  if (quot == 0)
                     quot = q;
                  else if (abs(quot) - abs(q) < EPSILON )
                     break;
               }
               if (quotposmax == 0 && quotnegmax == 0)
                  continue;
               if (quotposmax != 0 && quotnegmax != 0 && (quotposmax > quotnegmin || quotposmin < quotnegmax))
                  break;
               if (quotposmax != 0 || quotnegmax != 0)
               {
                  /* check whether stored constraint dominates new one */
                  if ((quotposmax > 0 && rhs >= bp->rhs[i] * quotposmax) || (quotposmax == 0 && rhs >= bp->rhs[i] * quotnegmin))
                  {
                     if (dom == -1)
                        break;
                     dom = 1;
                  }
                  /* check whether new constraint dominates stored one */
                  else if ((quotposmin > 0 && rhs <= bp->rhs[i] * quotposmin) || (quotposmin == 0 && rhs <= bp->rhs[i] * quotnegmax))
                  {
                     if (dom == 1)
                        break;
                     dom = -1;
                  }
                  /* none of the two constraints is dominating */
                  else
                     break;
               }
               /* should be dead code */
               if (dom != 0 && quot != 0)
                  break;
            }
         }
         if (j == bp->n)
         {
            assert( quotposmax != 0 || quotnegmax != 0 || quot != 0);
            if (quot != 0)
            {
               if (rhs / quot + bp->rhs[i] < -EPSILON && dom == 0)
                  return BP_INFEASIBLE;
            }
            else
            {
               if (quotposmax == quotposmin && quotnegmax == quotnegmin)
               {
                  assert(quotposmax != 0 || quotnegmax != 0);
                  assert(quotposmax == quotnegmax || quotposmax == 0 || quotnegmax == 0);
                  if (dom == 0)
                     bp->rhs[i] = MIN(bp->rhs[i], (TYPE)(rhs / quotposmax));
                  else if (dom == -1)
                  {
                     if ((quotposmin > 0 && rhs > bp->rhs[i] * quotposmin) || (quotposmin == 0 && rhs > bp->rhs[i] * quotnegmax))
                        continue;
                     for (int k= 0; k < bp->n; k++)
                        bp->coefs[i*bp->n+k] = coefs[k];
                     bp->rhs[i] = rhs;
                  }
                  else if ((quotposmax > 0 && rhs < bp->rhs[i] * quotposmax) || (quotposmax == 0 && rhs < bp->rhs[i] * quotnegmin))
                     continue;
               }
               else
               {
                  assert(dom != 0);
                  if (dom == -1)
                  {
                     if ((quotposmin > 0 && rhs > bp->rhs[i] * quotposmin) || (quotposmin == 0 && rhs > bp->rhs[i] * quotnegmax))
                        continue;
                     for (int k= 0; k < bp->n; k++)
                        bp->coefs[i*bp->n+k] = coefs[k];
                     bp->rhs[i] = rhs;
                  }
                  else if ((quotposmax > 0 && rhs < bp->rhs[i] * quotposmax) || (quotposmax == 0 && rhs < bp->rhs[i] * quotnegmin))
                     continue;
               }
#ifdef MORE_DEBUG
               printf("redundant constraint.\n");
#endif
               bp->redundant++;
               return BP_OKAY;
            }
         }
      }
   }

   /* add row (constraint) to binary program */
   for (int i = 0; i < bp->n; i++)
      bp->coefs[bp->m * bp->n + i] = coefs[i];
   bp->rhs[bp->m] = rhs;
   bp->m++;

   assert(bp_is_valid(bp));
   return BP_OKAY;
}

/*
 * gets the number of rows (constraints)
 */
int bp_getM(
   BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   return bp->m;
}

#if 0
/*
 * gets the number of columns (variables)
 */
int bp_getN(
   BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   return bp->n;
}
#endif

#if 0
/*
 * gets entries of the binary program
 */
TYPE* bp_getCoefs(
   BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   return bp->coefs;
}
#endif

#if 0
/*
 * gets right-hand sides of the constraints
 */
TYPE* bp_getRhs(
   BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   return bp->rhs;
}
#endif

#if 0
/*
 * gets maximal number of rows (constraints)
 */
int bp_getSize(
   BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   return bp->size;
}
#endif

/*
 * gets redundant number of rows (constraints)
 */
int bp_getRedundant(
   BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   return bp->redundant;
}

#if 0
/*
 * determines whether the binary program is empty
 */
bool bp_is_empty(
   const BinaryProgram* bp          /**< binary program */
)
{
   assert(bp_is_valid(bp));

   return bp->m == 0;
}
#endif

#ifndef BRANCHING
/*
 * solves the binary program
 * returns BP_OKAY, if a solution is found
 * returns BP_INFEASIBLE otherwise
 */
BP_RETCODE solveBP(
   BinaryProgram* bp,          /**< binary program */
   FILE* fp                    /**< output file */
)
{
   long int count = 0;
   int* values;
#ifdef DEBUG
   bool valid;
#endif
   long int length;
   double sum;
   long int p;

   assert(bp_is_valid(bp));

   values = allocate(bp->n, sizeof(*values));

   length = pow(2, bp->n);
   count = length;
   /* for all vectors */
   for (long int i = 0; i < length; i++)
   {
      p = i;
      /* create vector */
      for (int k = 0; k < bp->n; k++)
      {
         values[k] = p % 2;
         p /= 2;
      }
#ifdef DEBUG
      valid = true;
#endif
      /* for all constraints */
      for (int j = 0; j < bp->m; j++)
      {
         sum = 0.0;
         /* compute left-hand side of the constraint */
         for (int k = 0; k < bp->n; k++)
         {
            sum += bp->coefs[j*bp->n+k] * values[k];
         }
         /* check constraint for infeasibility */
         if (sum > bp->rhs[j])
         {
#ifdef DEBUG
            valid = false;
#endif
            /* decrease number of possible feasible solutions */
            count--;
            break;
         }
      }
#ifdef DEBUG
#ifndef MORE_DEBUG
      /* print solution */
      if (valid)
      {
#endif
         for (int k = 0; k < bp->n; k++)
         {
            fprintf(fp, "%i", values[k]);
            if (k < bp->n-1)
            {
               fprintf(fp, " ");
            }
            else
            {
#ifndef MORE_DEBUG
               fprintf(fp, "\n");
#else
               fprintf(fp, ": %i\n", valid);
#endif
            }
         }
#ifndef MORE_DEBUG
      }
#endif
#endif
   }
   /* free vector */
   deallocate(values);
   assert(bp_is_valid(bp));

   fprintf(fp, "%li solutions found\n", count);
   if (count > 0)
      return BP_OKAY;

   return BP_INFEASIBLE;
}

#else
/*
 * solves the BP with Branching
 * returns BP_OKAY, if a solution is found
 * returns BP_INFEASIBLE otherwise
 */
BP_RETCODE solveBT(
   BinaryProgram* bp,          /**< binary program */
   FILE* fp                    /**< output file */
)
{
   long int count = 0;

   /* check whether there are only redundant constraint in the bp */
   if (bp->m == 0)
   {
      /* set number of feasible solutions */
      count = pow(2, bp->n);
#ifdef DEBUG
      long int length = count;
      long int p;
      char* currsol;
      currsol = allocate(2*bp->n, sizeof(*currsol));
      /* print all solutions */
      for (long int i = 0; i < length; i++)
      {
         p = i;
         currsol[0] = '\0';
         for (int k = 0; k < bp->n; k++)
         {
            sprintf(currsol, "%s%s%li", currsol, (strlen(currsol) == 0) ? "" : " ", p % 2);
            p /= 2;
         }
         fprintf(fp, "%s\n", currsol);
      }
      deallocate(currsol);
#endif /* DEBUG */
      fprintf(fp, "%li solutions found.\n", count);
      assert(count > 0);
      return BP_OKAY;
   }

   /* allocate arrays */
   TYPE* lhs = allocate(bp->m, sizeof(*lhs));
#ifdef CUTOFF
   TYPE* min = allocate(bp->m, sizeof(*min));
   TYPE* max = allocate(bp->m, sizeof(*max));
#endif
   int depth = 0;

   /* initialize min, max and lhs */
   for (int i = 0; i < bp->m; i++)
   {
      lhs[i] = 0;
#ifdef CUTOFF
      max[i] = 0;
      min[i] = 0;
      for (int j = 1; j < bp->n; j++)
      {
         if (bp->coefs[i*bp->n+j] > 0)
            max[i] += bp->coefs[i*bp->n+j];
         else if (bp->coefs[i*bp->n+j] < 0)
            min[i] += bp->coefs[i*bp->n+j];
      }
#endif
   }

   char* sol = NULL;
#ifdef DEBUG
   sol = allocate(2*bp->n, sizeof(*sol));
#endif

   /* add coefficients of first variable to fixing */
   for (int i = 0; i < bp->m; i++)
      lhs[i] += bp->coefs[i*bp->n+0];

   /* branch on first variable => false */
#ifdef DEBUG
   sprintf(sol, "1");
#endif
#ifdef CUTOFF
   count += branch(fp, bp, lhs, min, max, depth+1, sol);
#else
   count += branch(fp, bp, lhs, NULL, NULL, depth+1, sol);
#endif

   /* remove coefficients of first variable to fixing */
   for (int i = 0; i < bp->m; i++)
      lhs[i] -= bp->coefs[i*bp->n+0];

   /* branch on first variable => true */
#ifdef DEBUG
   sprintf(sol, "0");
#endif
#ifdef CUTOFF
   count += branch(fp, bp, lhs, min, max, depth+1, sol);
#else
   count += branch(fp, bp, lhs, NULL, NULL, depth+1, sol);
#endif

   /* free arrays */
#ifdef DEBUG
   deallocate(sol);
#endif
#ifdef CUTOFF
   deallocate(max);
   deallocate(min);
#endif
   deallocate(lhs);

   fprintf(fp, "%li solutions found.\n", count);
   if (count > 0)
      return BP_OKAY;

   return BP_INFEASIBLE;
}

/*  */
int branch(
   FILE* fp,                   /**< output file */
   BinaryProgram* bp,          /**< binary program */
   TYPE* lhs,                  /**< value of left-hand side of the constraint */
   TYPE* min,                  /**< minimal activity of the constraint */
   TYPE* max,                  /**< maximal activity of the constraint */
   int depth,                  /**< depth in the tree */
   char* sol                   /**< fixed part of the solution */
)
{
   long int count = 0;
#ifdef CUTOFF
   bool unrestricted = true;
#endif /* CUTOFF */

#ifdef DEBUG
   assert(sol != NULL);
#else
   assert(sol == NULL);
#endif /* DEBUG */

#ifdef CUTOFF
   assert(min != NULL);
   assert(max != NULL);
#else /* CUTOFF */
   assert(min == NULL);
   assert(max == NULL);
#endif /* CUTOFF */

   int i;
#ifndef CUTOFF
   /* if CUTOFF not defined, check only leaves */
   if (depth == bp->n)
   {
#endif /* !CUTOFF */
      for (i = 0; i < bp->m; i++)
      {
#ifdef CUTOFF
         /* check activities */
         if (lhs[i] + min[i] > bp->rhs[i])
            break;
         if (lhs[i] + max[i] > bp->rhs[i])
            unrestricted = false;
#else /* CUTOFF */
         /* check feasibility of constraint */
         if (lhs[i] > bp->rhs[i])
            break;
#endif /* CUTOFF */
      }

      /* when all solutions were feasible */
      if (i == bp->m )
      {
#ifdef CUTOFF
         if (unrestricted)
         {
            if (depth == bp->n)
            {
#endif /* CUTOFF */
               /* add one feasible solution */
               count++;
#ifdef DEBUG
               fprintf(fp, "%s\n", sol);
#endif /* DEBUG */
#ifdef CUTOFF
            }
            else
            {
               /* when subtree was cut off, add all solutions of subtree */
#ifdef DEBUG
               long int length = pow(2, bp->n - depth);
               long int p;
               char* currsol;
               currsol = allocate(2*bp->n, sizeof(*currsol));
               for (long int i = 0; i < length; i++)
               {
                  p = i;
                  sprintf(currsol, "%s", sol);
                  for (int k = depth; k < bp->n; k++)
                  {
                     sprintf(currsol, "%s %li", currsol, p % 2);
                     p /= 2;
                  }
                  fprintf(fp, "%s\n", currsol);
               }
               deallocate(currsol);
#endif /* DEBUG */
               count += pow(2, bp->n - depth);
            }
            return count;
         }
#endif /* !CUTOFF */

      }
#ifdef CUTOFF
      /* when CUTOFF is defined, do not branch further, when subtree is infeasible */
      else
         return count;
#endif /* CUTOFF */
#ifndef CUTOFF
   }
   /* further branching */
   else
   {
#else /* !CUTOFF */
   if (depth < bp->n)
   {
#endif /* !CUTOFF */
      char* bsol = NULL;

      /* update min and max */
#ifdef CUTOFF
      for (i = 0; i < bp->m; i++)
      {
         max[i] -= MAX(bp->coefs[i*bp->n+depth], 0.0);
         min[i] -= MIN(bp->coefs[i*bp->n+depth], 0.0);
      }
#endif /* CUTOFF */

#ifdef DEBUG
      bsol = allocate(2*bp->n, sizeof(*bsol));
#endif /* DEBUG */
      /* add coefficients of first variable to fixing */
      for (int i = 0; i < bp->m; i++)
         lhs[i] += bp->coefs[i*bp->n+depth];

      /* branch on next variable => false */
#ifdef DEBUG
      i = sprintf(bsol, "%s 1", sol);
      assert(i < 2*bp->n);
#endif /* DEBUG */
#ifdef CUTOFF
      count += branch(fp, bp, lhs, min, max, depth+1, bsol);
#else /* CUTOFF */
      count += branch(fp, bp, lhs, NULL, NULL, depth+1, bsol);
#endif /* CUTOFF */

      /* remove coefficients of first variable to fixing */
      for (int i = 0; i < bp->m; i++)
         lhs[i] -= bp->coefs[i*bp->n+depth];

   /* branch on next variable => true */
#ifdef DEBUG
      i = sprintf(bsol, "%s 0", sol);
      assert(i < 2*bp->n);
#endif /* DEBUG */
#ifdef CUTOFF
      count += branch(fp, bp, lhs, min, max, depth+1, bsol);
#else /* CUTOFF */
      count += branch(fp, bp, lhs, NULL, NULL, depth+1, bsol);
#endif /* CUTOFF */

      /* update min and max */
#ifdef CUTOFF
      for (i = 0; i < bp->m; i++)
      {
         max[i] += MAX(bp->coefs[i*bp->n+depth], 0.0);
         min[i] += MIN(bp->coefs[i*bp->n+depth], 0.0);
      }
#endif /* CUTOFF */

#ifdef DEBUG
      deallocate(bsol);
#endif /* DEBUG */
   }

   return count;
}
#endif /* ! BRANCHING */
