#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h> // pow
#include <string.h> //TODO kann wieder raus

#include "allocate.h"
#include "bp.h"

#define EPSILON 0.000000001

#define CUTOFF

// #define DEBUG
// #define MORE_DEBUG

/* determines whether the binary program data is valid */
static bool bp_is_valid(const BinaryProgram* bp)
{
   return bp != NULL
      && bp->n > 0 && bp->m >= 0
      && bp->size > 0 && bp->m <= bp->size
      && bp->coefs != NULL && bp->rhs != NULL;
}

/* creates a new binary program */
BinaryProgram* bp_new(int m, int n)
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

/* frees binary program data */
void bp_free(BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   deallocate(bp->rhs);
   deallocate(bp->coefs);
   deallocate(bp);
}

/* adds a new row (constraint) */
BP_RETCODE bp_put(BinaryProgram* bp, TYPE* coefs, TYPE rhs)
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
         double c = 0;
         int dom = 0;
         bool set = false;

         int j = 0;
         for (; j < bp->n; j++)
         {
//             if ((coefs[j] != 0 && bp->coefs[i*bp->n+j] == 0) || (coefs[j] == 0 && bp->coefs[i*bp->n+j] != 0))
//                break;
            if (coefs[j] != 0 && bp->coefs[i*bp->n+j] == 0)
            {
               if (dom == 1)
                  break;
               dom = -1;
               continue;
            }
            if (coefs[j] == 0 && bp->coefs[i*bp->n+j] != 0)
            {
               if (dom == -1)
                  break;
               dom = 1;
               continue;
            }
            if (coefs[j] != 0 && bp->coefs[i*bp->n+j] != 0)
            {
               if (!set)
               {

//                   if (coefs[j] < 0)
//                      c = bp->coefs[i*bp->n+j] / coefs[j];
//                   else
                     c = coefs[j] / bp->coefs[i*bp->n+j];
                  if (c < 0 && dom != 0)
                     break;
                  set = true;
               }
               else
               {
                  if (c < 0)
                  {
                     if (dom != 0 || abs(c - coefs[j] / bp->coefs[i*bp->n+j]) > EPSILON)
                        break;
                  }
                  else
                  {
                     double quot;
//                      if(coefs[j] < 0)
//                         quot = bp->coefs[i*bp->n+j] / coefs[j];
//                      else
                        quot = coefs[j] / bp->coefs[i*bp->n+j];

                     if (quot < 0)
                        break;
//                      printf("dom: %i, quot: %f, c: %f\n", dom, quot, c);
                     /* quot greater than c */
                     if (quot - c > EPSILON)
                     {
                        if (rhs == 0 || bp->rhs[i] == 0)
                           break;
                        /* new constraint dominates stored constraint */
                        if (rhs / c <= bp->rhs[i])
                        {
                           if (dom == 1)
                              break;
                           dom = -1;
                           if( rhs < 0 && bp->rhs[i] < 0 )
                              c = quot;
                        }
                        /* stored constraint dominates new constraint */
                        else if (rhs / quot >= bp->rhs[i])
                        {
                           if (dom == -1)
                              break;
                           dom = 1;
                           if( rhs > 0 || bp->rhs[i] > 0 )
                              c = quot;
                        }
                        else
                           break;
                     }
                     /* c greater than quot */
                     else if (quot - c < -EPSILON)
                     {
                        if (rhs == 0 || bp->rhs[i] == 0)
                           break;
                        /* new constraint dominates stored constraint */
                        if (rhs / quot <= bp->rhs[i])
                        {
                           if (dom == 1)
                              break;
                           dom = -1;
                           if( rhs > 0 || bp->rhs[i] > 0 )
                              c = quot;
                        }
                        /* stored constraint dominates new constraint */
                        else if (rhs  / c >= bp->rhs[i])
                        {
                           if (dom == -1)
                              break;
                           dom = 1;
                           if( rhs < 0 && bp->rhs[i] < 0 )
                              c = quot;
                        }
                        else
                           break;
                     }
//                      if (abs(c - coefs[j] / bp->coefs[i*bp->n+j]) > EPSILON)
//                         break;
                  }
               }
            }
         }

         if (j == bp->n && set)
         {
            if (c < 0)
            {
               if (rhs / c + bp->rhs[i] < -EPSILON)
                  return BP_INFEASIBLE;
            }
            else
            {
               assert(c > 0);
               if (dom == 0)
               {
//                   printf("rhs(old): %f, rhs(new): %f\n", bp->rhs[i], (TYPE)(rhs / c));
                  bp->rhs[i] = MIN(bp->rhs[i], (TYPE)(rhs / c));
//                   printf("new rhs (%f)\n", (double)(bp->rhs[i]));
               }
               /* stored constraint dominates new constraint */
               else if (dom == 1)
               {
                  if (bp->rhs[i] > rhs / c)
                     continue;
//                   printf("old(%i) |> new (%f)\n", i, c);

               }
               /* new constraint dominates stored constraint */
               else if (dom == -1)
               {
                  if (bp->rhs[i] < rhs / c)
                     continue;
//                   printf("new |> old(%i) (%f)\n", i, c);
                  for (int k= 0; k < bp->n; k++)
                     bp->coefs[i*bp->n+k] = coefs[k];
                  bp->rhs[i] = rhs;
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

/* gets the number of rows (constraints) */
int bp_getM(BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   return bp->m;
}

#if 0
/* gets the number of columns (variables) */
int bp_getN(BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   return bp->n;
}
#endif

#if 0
/* gets entries of the binary program */
TYPE* bp_getCoefs(BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   return bp->coefs;
}
#endif

#if 0
/* gets right-hand sides of the constraints */
TYPE* bp_getRhs(BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   return bp->rhs;
}
#endif

#if 0
/* gets maximal number of rows (constraints) */
int bp_getSize(BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   return bp->size;
}
#endif

/* gets redundant number of rows (constraints) */
int bp_getRedundant(BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   return bp->redundant;
}

#if 0
/* determines whether the binary program is empty */
bool bp_is_empty(const BinaryProgram* bp)
{
   assert(bp_is_valid(bp));

   return bp->m == 0;
}
#endif

/* solves the LP
 * returns BP_OKAY, if a solution is found
 * returns BP_INFEASIBLE otherwise
 */
BP_RETCODE solveBP(BinaryProgram* bp, FILE* fp)
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
   for (long int i = 0; i < length; i++)
   {
      p = i;
      for (int k = 0; k < bp->n; k++)
      {
         values[k] = p % 2;
         p /= 2;
      }
#ifdef DEBUG
      valid = true;
#endif
      for (int j = 0; j < bp->m; j++)
      {
         sum = 0.0;
         for (int k = 0; k < bp->n; k++)
         {
            sum += bp->coefs[j*bp->n+k] * values[k];
         }
         if (sum > bp->rhs[j])
         {
#ifdef DEBUG
            valid = false;
#endif
            count--;
            break;
         }
      }
#ifdef DEBUG
#ifndef MORE_DEBUG
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
   deallocate(values);
   assert(bp_is_valid(bp));

   fprintf(fp, "%li solutions found\n", count);
   if (count > 0)
      return BP_OKAY;

   return BP_INFEASIBLE;
}

/* solves the BP with Branching
 * returns BP_OKAY, if a solution is found
 * returns BP_INFEASIBLE otherwise
 */
BP_RETCODE solveBT(BinaryProgram* bp, FILE* fp)
{
   long int count = 0;

   /* check whether there are only redundant constraint in the bp */
   if (bp->m == 0)
   {
      count = pow(2, bp->n);
#ifdef DEBUG
      long int length = pow(2, bp->n);
      long int p;
      char* currsol;
      currsol = allocate(2*bp->n, sizeof(*currsol));
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

   /* branch on first variable => false */
#ifdef DEBUG
   sprintf(sol, "0");
#endif
#ifdef CUTOFF
   count += branch(fp, bp, lhs, min, max, depth+1, sol);
//    count += branchCutoff(fp, bp, lhs, min, max, depth+1, sol);
#else
   count += branch(fp, bp, lhs, NULL, NULL, depth+1, sol);
#endif

   /* add coefficients of first variable to fixing */
   for (int i = 0; i < bp->m; i++)
      lhs[i] += bp->coefs[i*bp->n+0];

   /* branch on first variable => true */
#ifdef DEBUG
   sprintf(sol, "1");
#endif
#ifdef CUTOFF
   count += branch(fp, bp, lhs, min, max, depth+1, sol);
//    count += branchCutoff(fp, bp, lhs, min, max, depth+1, sol);
#else
   count += branch(fp, bp, lhs, NULL, NULL, depth+1, sol);
#endif

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

// int branchCutoff(FILE* fp, BinaryProgram* bp, TYPE* lhs, TYPE* min, TYPE* max, int depth, char* sol)
// {
//    bool unrestricted = true;
//    long int count = 0;
//
// #ifdef DEBUG
//    assert(sol != NULL);
// #else
//    assert(sol == NULL);
// #endif
//
//    int i;
//    for (i = 0; i < bp->m; i++)
//    {
//       if (lhs[i] + min[i] > bp->rhs[i])
//       {
//          break;
//       }
//       if (lhs[i] + max[i] > bp->rhs[i])
//       {
//          unrestricted = false;
//       }
//    }
//
//    if (i < bp->m)
//       return count;
//
//
//    if (unrestricted && i == bp->m)
//    {
//       if (depth == bp->n)
//       {
//          count++;
// #ifdef DEBUG
//          fprintf(fp, "%s\n", sol);
// #endif
//       }
//       else
//       {
// #ifdef DEBUG
//          long int length = pow(2, bp->n - depth);
//          long int p;
//          char* currsol;
//          currsol = allocate(2*bp->n, sizeof(*currsol));
//          for (long int i = 0; i < length; i++)
//          {
//             p = i;
//             sprintf(currsol, "%s", sol);
//             for (int k = depth; k < bp->n; k++)
//                sprintf(currsol, "%s %li", currsol, p % 2);
//             fprintf(fp, "%s\n", currsol);
//          }
//          deallocate(currsol);
// #endif
//          count += pow(2, bp->n - depth);
//       }
//       return count;
//    }
//
//    if (depth < bp->n)
//    {
//       TYPE* blhs = allocate(bp->m, sizeof(*blhs));
//       TYPE* bmax = allocate(bp->m, sizeof(*bmax));
//       TYPE* bmin = allocate(bp->m, sizeof(*bmin));
//
//       for (i = 0; i < bp->n; i++)
//       {
//          bmax[i] = max[i] + MAX(bp->coefs[i*bp->n+depth-1], 0.0);
//          bmin[i] = min[i] + MIN(bp->coefs[i*bp->n+depth-1], 0.0);
//       }
//       char* bsol = NULL;
// #ifdef DEBUG
//       bsol = allocate(2*bp->n, sizeof(*bsol));
//       i = sprintf(bsol, "%s 0", sol);
//       assert(i < 2*bp->n);
// #endif
//       count += branchCutoff(fp, bp, lhs, bmin, bmax, depth+1, bsol);
//       for (i = 0; i < bp->n; i++)
//          blhs[i] = lhs[i] + bp->coefs[i*bp->n+depth];
// #ifdef DEBUG
//       i = sprintf(bsol, "%s 1", sol);
//       assert(i < 2*bp->n);
// #endif
//       count += branchCutoff(fp, bp, blhs, bmin, bmax, depth+1, bsol);
//
// #ifdef DEBUG
//       deallocate(bsol);
// #endif
//       deallocate(bmax);
//       deallocate(bmin);
//       deallocate(blhs);
//    }
//
//    return count;
// }

/*  */
int branch(FILE* fp, BinaryProgram* bp, TYPE* lhs, TYPE* min, TYPE* max, int depth, char* sol)
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
   if (depth == bp->n)
   {
#endif /* !CUTOFF */
      for (i = 0; i < bp->m; i++)
      {
#ifdef CUTOFF
         if (lhs[i] + min[i] > bp->rhs[i])
            break;
         if (lhs[i] + max[i] > bp->rhs[i])
            unrestricted = false;
#else /* CUTOFF */
         if (lhs[i] > bp->rhs[i])
            break;
#endif /* CUTOFF */
      }

      if (i == bp->m )
      {
#ifdef CUTOFF
         if (unrestricted)
         {
            if (depth == bp->n)
            {
#endif /* CUTOFF */
               count++;
#ifdef DEBUG
               fprintf(fp, "%s\n", sol);
#endif /* DEBUG */
#ifdef CUTOFF
            }
            else
            {
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
      else
         return count;
#endif /* CUTOFF */
#ifndef CUTOFF
   }
   else
   {
#else /* !CUTOFF */
   if (depth < bp->n)
   {
#endif /* !CUTOFF */
      char* bsol = NULL;

      /* update min and max */
#ifdef CUTOFF
      TYPE* bmax = allocate(bp->m, sizeof(*bmax));
      TYPE* bmin = allocate(bp->m, sizeof(*bmin));
      for (i = 0; i < bp->m; i++)
      {
         bmax[i] = max[i] - MAX(bp->coefs[i*bp->n+depth], 0.0);
         bmin[i] = min[i] - MIN(bp->coefs[i*bp->n+depth], 0.0);
      }
#endif /* CUTOFF */

      /* branch on variable => false */
#ifdef DEBUG
      bsol = allocate(2*bp->n, sizeof(*bsol));
      i = sprintf(bsol, "%s 0", sol);
      assert(i < 2*bp->n);
#endif /* DEBUG */
#ifdef CUTOFF
      count += branch(fp, bp, lhs, bmin, bmax, depth+1, bsol);
#else /* CUTOFF */
      count += branch(fp, bp, lhs, NULL, NULL, depth+1, bsol);
#endif /* CUTOFF */
      /* update fixations */
      TYPE* blhs = allocate(bp->m, sizeof(*blhs));
      for (i = 0; i < bp->m; i++)
         blhs[i] = lhs[i] + bp->coefs[i*bp->n+depth];

      /* branch on variable => true */
#ifdef DEBUG
      i = sprintf(bsol, "%s 1", sol);
      assert(i < 2*bp->n);
#endif /* DEBUG */
#ifdef CUTOFF
      count += branch(fp, bp, blhs, bmin, bmax, depth+1, bsol);
#else /* CUTOFF */
      count += branch(fp, bp, blhs, NULL, NULL, depth+1, bsol);
#endif /* CUTOFF */
      deallocate(blhs);
#ifdef DEBUG
      deallocate(bsol);
#endif /* DEBUG */

      /* free cut-off variables */
#ifdef CUTOFF
      deallocate(bmin);
      deallocate(bmax);
#endif /* CUTOFF */
   }

   return count;
}
