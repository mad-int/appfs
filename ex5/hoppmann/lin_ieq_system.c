/*
 * ieq_system.c
 *
 *  Created on: 12.11.2014
 *      Author: bzfhoppm
 */

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>   // fopen
#include <time.h>

#include "lin_ieq_system.h"

#define GET_SEC(a, b)  ((b - a) / (double)CLOCKS_PER_SEC)


static int n_sol = 0;

struct linear_inequality_system
{
  int n_vars;
  int n_cons;
  int size;
  number* cons_lhs;
  number* cons_rhs;
  ineq_t* cons_type;
};

int getInt(number n)
{
   return n.i;
}

double getDouble(number n)
{
   return n.d;
}

static bool linIeqSys_is_valid(const LinIeqSys* linIeqSys)
{
    return linIeqSys != NULL
         && linIeqSys->size > 0
         && linIeqSys->size >= linIeqSys->n_vars * linIeqSys->n_cons
         && linIeqSys->cons_lhs != NULL
         && linIeqSys->cons_rhs != NULL
         && linIeqSys->cons_type != NULL;
}

static int position(const LinIeqSys* linIeqSys, int row_i, int col_j)
{
   assert(linIeqSys_is_valid(linIeqSys));

   int pos = row_i * linIeqSys->n_vars + col_j;

   assert(pos < linIeqSys->size);
   assert(pos >= 0);

   return pos;
}

LinIeqSys* linIeqSys_new(int num_cons, int num_vars)
{
   LinIeqSys* linIeqSys;

   assert(num_vars > 0);
   assert(num_cons > 0);

   linIeqSys = allocate(1, sizeof(*linIeqSys));


   linIeqSys->n_vars = num_vars;
   linIeqSys->n_cons = num_cons;
   linIeqSys->size = linIeqSys->n_vars * linIeqSys->n_cons;

   linIeqSys->cons_lhs = allocate(linIeqSys->size, sizeof(*linIeqSys->cons_lhs));
   linIeqSys->cons_rhs = allocate(linIeqSys->n_cons, sizeof(*linIeqSys->cons_rhs));
   linIeqSys->cons_type = allocate(linIeqSys->n_cons, sizeof(*linIeqSys->cons_type));

   assert(linIeqSys_is_valid(linIeqSys));

   return linIeqSys;
}

void linIeqSys_free(LinIeqSys* linIeqSys)
{
   assert(linIeqSys_is_valid(linIeqSys));

   deallocate(linIeqSys->cons_lhs);
   deallocate(linIeqSys->cons_rhs);
   deallocate(linIeqSys->cons_type);
   deallocate(linIeqSys);
}

void linIeqSys_put(LinIeqSys* linIeqSys, int row_i, int col_j, number a)
{
   assert(linIeqSys_is_valid(linIeqSys));

   assert(row_i >= 0);
   assert(col_j >= 0);
   assert(row_i < linIeqSys->n_cons);
   assert(col_j < linIeqSys->n_vars);

   int pos = position(linIeqSys, row_i, col_j);

   linIeqSys->cons_lhs[pos] = a;

   assert(linIeqSys_is_valid(linIeqSys));

}

void linIeqSys_setType(LinIeqSys* linIeqSys, int row_i, ineq_t type)
{
   assert(linIeqSys_is_valid(linIeqSys));

   assert(row_i >= 0);
   assert(row_i < linIeqSys->n_cons);

   linIeqSys->cons_type[row_i] = type;

   assert(linIeqSys_is_valid(linIeqSys));

}


void linIeqSys_putRHS(LinIeqSys* linIeqSys, int row_i, number b)
{
   assert(linIeqSys_is_valid(linIeqSys));

   assert(row_i >= 0);
   assert(row_i < linIeqSys->n_cons);

   linIeqSys->cons_rhs[row_i] = b;

   assert(linIeqSys_is_valid(linIeqSys));
}

int linIeqSys_nCons(LinIeqSys* linIeqSys)
{
   assert(linIeqSys_is_valid(linIeqSys));

   return linIeqSys->n_cons;
}

int linIeqSys_nVars(LinIeqSys* linIeqSys)
{
   assert(linIeqSys_is_valid(linIeqSys));

   return linIeqSys->n_vars;
}

void linIeqSys_print(LinIeqSys* linIeqSys)
{
   assert(linIeqSys_is_valid(linIeqSys));

   printf("\n");
   printf("Linear inequality system with %d variables and %d constraints:\n\n", linIeqSys_nVars(linIeqSys), linIeqSys_nCons(linIeqSys));

   int i;
   ineq_t type;
   int row_i;
   int col_j;

   row_i = 0;
   col_j = 0;

   for(i = 0; i < linIeqSys->size; i++)
   {
      col_j++;

   #ifdef INT
      printf("%d x_%d", getInt(linIeqSys->cons_lhs[i]), col_j);
   #else
      printf("%.1f x_%d", getDouble(linIeqSys->cons_lhs[i]), col_j);
   #endif

      if(col_j < linIeqSys->n_vars)
         printf(" + ");

      int offset = (i + 1) % linIeqSys->n_vars;
      if(offset == 0)
      {
         assert( row_i < linIeqSys->n_cons);

         type = linIeqSys->cons_type[row_i];
         switch(type){
            case leq  :
               printf(" <=");
               break;
            case eq  :
               printf(" =");
               break;
            case geq  :
               printf(" >=");
               break;
         }
#ifdef INT
         printf(" %d", getInt(linIeqSys->cons_rhs[row_i]));
#else
         printf(" %.1f", getDouble(linIeqSys->cons_rhs[row_i]));
#endif

         printf("\n");
         col_j = 0;
         row_i++;
      }
   }

   assert(linIeqSys_is_valid(linIeqSys));
}

static void printSolution(char* currentSolution, int n_vars) {

   int i;

   n_sol++;

   printf("x_%d = ", n_sol);
   for (i = 0; i < n_vars; i++) {
      printf("%d ", currentSolution[i]);
   }

   printf("\n");
}

static bool checkFeasibility(const LinIeqSys* linIeqSys, char* solution, int n_vars) {

      assert(linIeqSys_is_valid(linIeqSys));

      double lhs;
      double rhs;
      double a;

      int pos;
      int row_i = 0;
      int col_j = 0;

      ineq_t type;

      /** check for each constraint if it is satisfied */
      for (row_i = 0; row_i < linIeqSys->n_cons; row_i++)
      {
         type = linIeqSys->cons_type[row_i];

#ifdef INT
         rhs  = getInt(linIeqSys->cons_rhs[row_i]);
#else
         rhs  = getDouble(linIeqSys->cons_rhs[row_i]);
#endif

         lhs  = 0.0;
         for (col_j = 0; col_j < linIeqSys->n_vars; col_j++)
         {
            pos = position(linIeqSys, row_i, col_j);
#ifdef INT
         a = getInt(linIeqSys->cons_lhs[pos]);
#else
         a = getDouble(linIeqSys->cons_lhs[pos]);
#endif
         lhs += solution[col_j] * a;
         }
         if( ((leq == type) && (lhs > rhs) )
               || ((eq == type) && (lhs != rhs))
               || ((geq == type) && (lhs < rhs)))
            return false;
      }

      assert(linIeqSys_is_valid(linIeqSys));

   return true;
}

static void solveRecursively(LinIeqSys* linIeqSys, int var, char* currentSolution) {

   assert(var >= 0);
   assert(var <= linIeqSys->n_vars);

   if (var == (linIeqSys->n_vars)) {
      if (checkFeasibility(linIeqSys, currentSolution, linIeqSys->n_vars)) {
         printSolution(currentSolution, linIeqSys->n_vars);
      }
      return;
   }

   currentSolution[var] = 0;
   solveRecursively(linIeqSys, var + 1,
         currentSolution);

   currentSolution[var] = 1;
   solveRecursively(linIeqSys, var + 1,
         currentSolution);

}

void linIeqSys_solve(LinIeqSys* linIeqSys)
{
   int i;

   printf("\n");
   printf("Solve linear inequality system.\n\n");

   char currentSolution[linIeqSys->n_vars];

   for (i = 0; i < linIeqSys->n_vars; i++) {
      currentSolution[i] = 0;
   }

   clock_t start = clock();

   solveRecursively(linIeqSys, 0,  currentSolution);

   double elapsed = GET_SEC(start, clock());

   printf("\n");
   printf("Solved in %.3f s. \n", elapsed);

   if(0 == n_sol)
   {
      printf("Linear inequality system is infeasible.\n");
   }
   else
   {
      printf("\n");
      printf("Found in total %d solutions.\n", n_sol);
   }
}
