#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h> // isspace
#include <time.h>
#include "linear_program.h"
#include "allocate.h"

#define MAX_LINE_LEN   512  // Maximum input line length
#define EPS 1e-10
#define MAX_CONSTR 32
#define MAX_VAR 32

#ifdef DOUBLE
#define NUMBER double
#define NUMBER_FORMAT "%lf"
#else
#define NUMBER int
#define NUMBER_FORMAT "%d"
#endif

/* represents a linear program Ax<=b */
struct linear_program
{
   int var; /* the number of variables */
   int constr; /* the number of set constraints */
   int max_constr;
   NUMBER** matrix; /* the matrix A */
   NUMBER* rhs; /* the right hand side vector b */
};

enum comp_sign
{
   LEQ, /* <= */
   GEQ, /* >= */
   EQ   /* == */
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
   for (i = 0; i < lp->max_constr; ++i)
   {
      lp->matrix[i] = allocate(var, sizeof(*lp->matrix[0]));
   }
   lp->rhs = allocate(lp->max_constr, sizeof(*lp->rhs));
   
   return lp;
}

void add_constraint_lp(LinearProgram* lp, NUMBER* row, enum comp_sign sign, NUMBER rhs)
{
   assert(lp);
   assert(lp->constr + 2 <= lp->max_constr);
   assert(row);
   assert(0 <= sign && sign < 3);
   
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
   char s[MAX_LINE_LEN];
   for(i = 0; i < lp->constr; ++i)
   {
      for(j = 0; j < lp->var; ++j)
      {
         sprintf(s, "%s ", NUMBER_FORMAT);
         printf(s, lp->matrix[i][j]);
      }
      sprintf(s, "<= %s\n", NUMBER_FORMAT);
      printf(s, lp->rhs[i]);
   }
}

void parse_error(const char* file_name, int line, const char* message)
{
   assert(file_name);
   assert(message);
   assert(line >= 0);
   
   fprintf(stderr, "Parsing error in %s (line %d): %s\n", file_name, line, message);
   exit(EXIT_FAILURE);
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
   
   int added_rows = 0;
   
   enum parser_state state = READ_VAR;
   
   int var = 0, constr = 0;
   LinearProgram* lp = NULL;
   
   if (NULL == (fp = fopen(file_name, "r")))
   {
      fprintf(stderr, "Can't open file %s\n", file_name);
      exit(EXIT_FAILURE);
   }
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
      
      int i = 0;
      char* ptr;
      ptr = strtok(s, " ");
      
      switch(state)
      {
         case READ_VAR:
            while(ptr != NULL)
            {
               if(i > 0)
                  parse_error(file_name, lines, "too many entries in line");
               char ending;
               if(1 != sscanf(ptr, "%d%s", &var, &ending))
                  parse_error(file_name, lines, "wrong format for the number of variables");
               if(var <= 0)
                  parse_error(file_name, lines, "non-positive number of variables");
               ptr = strtok(NULL, " ");
               ++i;
            }
            state = READ_CONSTR;
            break;
         case READ_CONSTR:
            while(ptr != NULL)
            {
               if(i > 0)
                  parse_error(file_name, lines, "too many entries in line");
               char ending;
               if(1 != sscanf(s, "%d%s", &constr, &ending))
                  parse_error(file_name, lines, "wrong format for the numbe of constraints");
               if(constr <= 0)
                  parse_error(file_name, lines, "non-positive number of constraints");
               lp = new_lp(var, constr);
               ptr = strtok(NULL, " ");
               ++i;
            }
            state = READ_MATRIX;
            break;
         case READ_MATRIX:
         {
            if(added_rows >= constr)
               parse_error(file_name, lines, "too many matrix rows");
            NUMBER row[var];
            enum comp_sign sign = -1;
            NUMBER rhs;
            while(ptr != NULL)
            {
               if(i < var)
               {
                  char ending;
                  char argument[MAX_LINE_LEN];
                  sprintf(argument, "%s%s", NUMBER_FORMAT, "%s");
                  if(1 != sscanf(ptr, argument, &row[i], &ending))
                     parse_error(file_name, lines, "wrong format for a matrix entry");
               }
               else if(i == var)
               {
                  if(0 == strcmp(ptr, "<="))
                     sign = LEQ;
                  else if(0 == strcmp(ptr, ">="))
                     sign = GEQ;
                  else if(0 == strcmp(ptr, "=="))
                     sign = EQ;
                  else
                  {
                     parse_error(file_name, lines, "no valid comparison sign");
                  }
               }
               else if(i == var + 1)
               {
                  char ending;
                  char argument[MAX_LINE_LEN];
                  sprintf(argument, "%s%s", NUMBER_FORMAT, "%s");
                  if(1 != sscanf(ptr, argument, &rhs, &ending))
                     parse_error(file_name, lines, "wrong format for the rhs");
               }
               else
                  parse_error(file_name, lines, "too many entries in line");
               ptr = strtok(NULL, " ");
               ++i;
            }
            if(i < var + 2)
               parse_error(file_name, lines, "too few entries in line");
            add_constraint_lp(lp, row, sign, rhs);
            ++added_rows;
         }
      }
         
   }
   
   if(added_rows < constr)
      parse_error(file_name, lines, "too few constraints specified");
   
   fclose(fp);
   
   return lp;
}

/* Enumerates all feasible solutions of the given LP and writes them to the given file.
   For faster evaluation gray codes are used. This might lead to numerical problems
   when floating point arithmetic is used.*/
void print_feasible_binary_lp(LinearProgram* lp, char* file_name)
{
   assert(lp);
   assert(file_name);
   assert(0 < strlen(file_name));
   
   FILE *output;
   if(NULL == (output = fopen(file_name, "w")))
   {
      fprintf(stderr, "Can't open file %s\n", file_name);
      exit(EXIT_FAILURE);
   }
   
   /*clock_t t1, t2;
   t1 = clock();*/
   
   double time = - (double) clock();
   
   int i;
   
   long positions[MAX_CONSTR + 1];
   positions[0] = 1l;
   for(i = 1; i <= MAX_CONSTR; ++i)
      positions[i] = positions[i-1] << 1;
   
   long x_count = 0l;
   NUMBER eval[lp->constr];
   for(i=0; i < lp->constr; ++i)
      eval[i] = 0;
   int solutions = 0;
   while(1)
   {
      /* check current vector for feasibility */
      int feasible = 1;
      for(i = 0; i < lp->constr; ++i)
      {
         if(eval[i] > lp->rhs[i] + EPS)
         {
            feasible = 0;
            break;
         }
      }
       
      /* print feasible solution to file */
      if (feasible)
      {
         /* calculate current vector in gray code */
         long x = x_count ^ (x_count >> 1);

         time += (double) clock();
         
         for(i = 0; i < lp->var; ++i)
         {
            fprintf(output, "%ld",((x >> i) & 1l));
         }
         fprintf(output, "\n");
         
         time -= (double) clock();
         
         ++solutions;
      }
       
      /* calculate switching position */
      ++x_count;
      int changed_var = __builtin_ffsl(x_count) - 1;
      
      /*int changed_var = 0;
      while((positions[changed_var] & x_count) == 0l)
         ++changed_var;*/
      
      if(changed_var >= lp->var)
         break;
      
      /* calculate new evaluation */
      NUMBER sign = -1;
      if((positions[changed_var + 1] & x_count) == 0l)
         sign = 1;
      for(i = 0; i < lp->constr; ++i)
         eval[i] += sign * lp->matrix[i][changed_var];
   }
   
   
   /*t2 = clock();
   double time = ((double)t2 - (double)t1) / CLOCKS_PER_SEC;*/
   
   time += (double) clock();
   time /= CLOCKS_PER_SEC;
   
   fclose(output);
   printf("Printed %i solutions to file %s\n", solutions, file_name);
   printf("Time for enumeration: %f seconds\n", time);
}