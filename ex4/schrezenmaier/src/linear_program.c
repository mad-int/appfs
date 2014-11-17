#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "linear_program.h"
#include "allocate.h"

#define MAX_LINE_LEN   512  // Maximum input line length

/* represents a linear program Ax<=b */
struct linear_program
{
   int var; /* the number of variables */
   int constr; /* the number of set constraints */
   int max_constr;
   int** matrix; /* the matrix A */
   int* rhs; /* the right hand side vector b */
};

enum comp_sign
{
   LEQ,
   GEQ,
   EQ
};

LinearProgram* new_lp(int var, int constr)
{
   /* since == constraints are replaced by two <= constraints,
      we have to reserve space for twice the given number of constraints*/
   LinearProgram* lp = allocate(1, sizeof(*lp));
   lp->var = var;
   lp->constr = 0;
   lp->max_constr = 2 * constr;
   lp->matrix = allocate(lp->max_constr, sizeof(*lp->matrix));
   int i;
   for (i = 0; i < var; ++i)
   {
      lp->matrix[i] = allocate(var, sizeof(*lp->matrix[0]));
   }
   lp->rhs = allocate(lp->max_constr, sizeof(*lp->rhs));
   
   return lp;
}

void add_constraint_lp(LinearProgram* lp, int* row, enum comp_sign sign, int rhs)
{
   assert(lp);
   assert(lp->constr + 2 < lp->max_constr);
   assert(row);
   assert(0 <= sign);
   assert(sign < 3);
   
   int i;
   switch(sign)
   {
      case LEQ:
         for(i = 0; i < lp->var; ++i)
            lp->matrix[lp->constr][i] = row[i];
         lp->rhs[lp->constr] = rhs;
         lp->constr++;
         break;
      case GEQ:
         for(i = 0; i < lp->var; ++i)
            lp->matrix[lp->constr][i] = - row[i];
         lp->rhs[lp->constr] = - rhs;
         lp->constr++;
         break;
      case EQ:
         for(i = 0; i < lp->var; ++i)
         {
            lp->matrix[lp->constr][i] = row[i];
            lp->matrix[lp->constr + 1][i] = - row[i];
         }
         lp->rhs[lp->constr] = rhs;
         lp->rhs[lp->constr + 1] = - rhs;
         lp->constr += 2;
         break;
   }
}

void print_lp(LinearProgram* lp)
{
   assert(lp);
   
   int i, j;
   for(i = 0; i < lp->constr; ++i)
   {
      for(j = 0; j < lp->var; ++j)
         printf("%i ", lp->matrix[i][j]);
      printf("<= %i\n", lp->rhs[i]);
   }
}

LinearProgram* read_from_file_lp(const char* file_name)
{
   assert(file_name);
   assert(0 < strlen(file_name));
   
   enum parser_state
   {
      READ_VAR,
      READ_CONSTR,
      READ_MATRIX
   };

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int lines = 0;
   
   enum parser_state state = READ_VAR;
   
   int var = 0, constr = 0;
   LinearProgram* lp;
   
   if (NULL == (fp = fopen(file_name, "r")))
      fprintf(stderr, "Can't open file %s\n", file_name);
   while(NULL != (s = fgets(buf, sizeof(buf), fp)))
   {
      char* t = strpbrk(s, "#\n\r");
      
      lines++;

      if (NULL != t) /* else line is not terminated or too long */
         *t = '\0';  /* clip comment or newline */
         
      /* Skip over leading space
       */
      while(isspace(*s))
         s++;

      /* Skip over empty lines
       */
      if (!*s)  /* <=> (*s == '\0') */
         continue;

      switch(state)
      {
         case READ_VAR:
            var = atoi(s);
            state = READ_CONSTR;
            break;
         case READ_CONSTR:
            constr = atoi(s);
            lp = new_lp(var, constr);
            state = READ_MATRIX;
            break;
         case READ_MATRIX:
         {
            int row[constr];
            enum comp_sign sign;
            int rhs;
            char *ptr;
            ptr = strtok(s, " ");
            int i = 0;
            while(ptr != NULL)
            {
               if(i < var)
                  row[i] = atoi(ptr);
               else if(i == var)
               {
                  if(0 == strcmp(ptr, "<="))
                     sign = LEQ;
                  else if(0 == strcmp(ptr, ">="))
                     sign = GEQ;
                  else if(0 == strcmp(ptr, "=="))
                     sign = EQ;
               }
               else if(i == var + 1)
                  rhs = atoi(ptr);
               ptr = strtok(NULL, " ");
               ++i;
            }
            add_constraint_lp(lp, row, sign, rhs);
         }
      }
         
   }
   fclose(fp);
   
   return lp;
}

void print_feasible_binary_lp(LinearProgram* lp, char* file_name)
{
   assert(lp);
   assert(file_name);
   assert(0 < strlen(file_name));
   
   FILE *output;
   if(NULL == (output = fopen(file_name, "w")))
   {
      fprintf(stderr, "Can't open file %s\n", file_name);
   }
   
   long x = 0l;
   long x_count = 0l;
   int eval[lp->constr];
   int i;
   for(i=0; i < lp->constr; ++i)
      eval[i] = 0;
   int solutions = 0;
   while(1)
   {
      /* check current vector for feasibility */
      int feasible = 1;
      for(i = 0; i < lp->constr; ++i)
      {
         if(eval[i] > lp->rhs[i])
         {
            feasible = 0;
            break;
         }
      }
       
      /* print feasible solution to file */
      if (feasible)
      {
         for(i = 0; i < lp->var; ++i)
         {
            fprintf(output, "%ld",((x >> i) & 1l));
         }
         fprintf(output, "\n");
         ++solutions;
      }
       
      /* calculate new vector in gray code */
      ++x_count;
      long x_new = x_count ^ (x_count >> 1);
      long diff = x_new ^ x;
      int changed_var = 0;
      while((1ul & diff) == 0)
      {
         ++changed_var;
         diff >>= 1;
      }
      if(changed_var >= lp->var)
         break;
      x = x_new;
      
      /* calculate new evaluation */
      int sign = -1;
      if(x & (1ul << changed_var))
         sign = 1;
      for(i = 0; i < lp->constr; ++i)
         eval[i] += sign * lp->matrix[i][changed_var];
   }
   fclose(output);
   printf("Printed %i solutions to file %s\n", solutions, file_name);
}