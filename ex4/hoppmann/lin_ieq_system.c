/*
 * ieq_system.c
 *
 *  Created on: 12.11.2014
 *      Author: bzfhoppm
 */

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>   // fopen

#include "lin_ieq_system.h"

static int n_sol = 0;


struct linear_inequality_system
{
  int n_vars;
  int n_cons;
  int size;
  double* entries;
};

static bool linIeqSys_is_valid(const LinIeqSys* linIeqSys)
{
    return linIeqSys != NULL
         && linIeqSys->size > 0
         && linIeqSys->size >= (linIeqSys->n_vars + 1) * linIeqSys->n_cons
         && linIeqSys->entries != NULL;
}

static int position(const LinIeqSys* linIeqSys, int row_i, int col_j)
{
   assert(linIeqSys_is_valid(linIeqSys));

   int pos = row_i * (linIeqSys->n_vars + 1) + col_j;

   assert(pos < linIeqSys->size);
   assert(pos >= 0);

   return pos;
}

static int positionRHS(const LinIeqSys* linIeqSys, int row_i)
{
   assert(linIeqSys_is_valid(linIeqSys));

   int pos = row_i * (linIeqSys->n_vars + 1) + linIeqSys->n_vars;

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
   linIeqSys->size = (linIeqSys->n_vars + 1) * linIeqSys->n_cons;

   linIeqSys->entries = allocate(linIeqSys->size, sizeof(*linIeqSys->entries));

   assert(linIeqSys_is_valid(linIeqSys));

   return linIeqSys;
}

void linIeqSys_free(LinIeqSys* linIeqSys)
{
   assert(linIeqSys_is_valid(linIeqSys));

   deallocate(linIeqSys->entries);
   deallocate(linIeqSys);
}

int linIeqSys_put(LinIeqSys* linIeqSys, int row_i, int col_j, double a)
{
   assert(linIeqSys_is_valid(linIeqSys));

   assert(row_i >= 0);
   assert(col_j >= 0);
   assert(row_i < linIeqSys->n_cons);
   assert(col_j < linIeqSys->n_vars);

   int pos = position(linIeqSys, row_i, col_j);

   linIeqSys->entries[pos] = a;

   assert(linIeqSys_is_valid(linIeqSys));

   return pos;
}

void linIeqSys_putRHS(LinIeqSys* linIeqSys, int row_i, double b)
{
   assert(linIeqSys_is_valid(linIeqSys));

   assert(row_i >= 0);
   assert(row_i < linIeqSys->n_cons);

   int pos = positionRHS(linIeqSys, row_i);

   linIeqSys->entries[pos] = b;

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

   for(i = 0; i < linIeqSys->size; i++)
   {
      if(i > 0)
      {
         int offset = i % (linIeqSys->n_vars + 1);
         if(offset == 0)
            printf("\n");
         if(offset == linIeqSys->n_vars)
            printf("<=\t");
      }

      printf("%.1f\t", linIeqSys->entries[i]);
   }
   printf("\n");

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

      int pos;
      int row_i = 0;
      int col_j = 0;

      /** check for each constraint if it is satisfied */
      for (row_i = 0; row_i < linIeqSys->n_cons; row_i++)
      {
         lhs = 0.0;
         for (col_j = 0; col_j < linIeqSys->n_vars; col_j++)
         {
            pos = position(linIeqSys, row_i, col_j);
            lhs += solution[col_j] * linIeqSys->entries[pos];
         }
         pos = positionRHS(linIeqSys, row_i);
         if (lhs > linIeqSys->entries[pos])
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
   solveRecursively(linIeqSys, 0,  currentSolution);

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
