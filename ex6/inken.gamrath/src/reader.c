#include <string.h> // strpbrk
#include <ctype.h> // isspace
#include <stdio.h> // FILE, fopen
#include <assert.h> // assert
#include <stdlib.h> // strtol

#include "allocate.h"
#include "bp.h"
#include "misc.h"

/* read problem data from file
 * returns amount of lines
 * returns BP_INVALIDDATA when there was a reading error
 * returns BP_INFEASIBLE when the problem is infeasible
 */
BP_RETCODE process_file(const char* filename, BinaryProgram** bp)
{
   assert(NULL != filename);
   assert(0 < strlen(filename));

   FILE* fp;
   char buf[MAX_LINE_LEN];
   char* s;
   char* tok;
   char delimiter[2] = " ";
   char* test;
   char relation[3];
   int lines = 0;
   int m = -1;
   int n = -1;
   TYPE* coefs = NULL;
   TYPE rhs;
   int i = 0;
   int j = 0;

   if (NULL == (fp = fopen(filename, "r")))
   {
      fprintf(stderr, "Can't open file %s\n", filename);
      return BP_NOFILE;
   }

   while(NULL != (s = fgets(buf, sizeof(buf), fp)))
   {
      char* t = strpbrk(s, "#\n\r");

      lines++;

      if (NULL != t) /* else line is not terminated or too long */
         *t = '\0'; /* clip comment or newline */

      /* Skip over leading space
       */
      while(isspace(*s))
         s++;

      /* Skip over empty lines
       */
      if (!*s) /* <=> (*s == '\0') */
         continue;

      /* read lines */
      i++;
      j = 0;
      tok = strtok(s, delimiter);
      switch(i)
      {
         /* first line contains amount of columns (variables) */
         case 1:
         {
            n = strtol(tok, &test, 10);
            if ((int)strlen(test) != 0)
            {
               fprintf(stderr, "Wrong input data for number of variables.\n");
               fclose(fp);
               return BP_INVALIDDATA;
            }
            if (n <= 0 || n > MAX_MATRIX_SIZE)
            {
               fprintf(stderr, "Dimension of columns is not right. (%i not in {1, ..., %i})\n", n, MAX_MATRIX_SIZE);
               fclose(fp);
               return BP_INVALIDDATA;
            }
            tok = strtok(NULL, delimiter);
            if (tok != NULL)
            {
               fprintf(stderr, "Too many arguments for number of columns.\n");
               fclose(fp);
               return BP_INVALIDDATA;
            }
            coefs = allocate(n, sizeof(*coefs));
            assert(coefs != NULL);
         break;
         }

         /* second line contains amount of rows (constraints) */
         case 2:
         {
            m = strtol(tok, &test, 10);
            if ((int)strlen(test) != 0)
            {
               fprintf(stderr, "Wrong input data for number of constraints.\n");
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }
            if (m <= 0 || m > MAX_MATRIX_SIZE)
            {
               fprintf(stderr, "Dimension of rows is not right. (%i not in {1, ..., %i})\n", m, MAX_MATRIX_SIZE);
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }
            tok = strtok(NULL, delimiter);
            if (tok != NULL)
            {
               fprintf(stderr, "Too many arguments for number of rows.\n");
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }
            *bp = bp_new(m, n);
         break;
         }

         /* other lines contain rows (constraints) of binary program and right-hand sides */
         default:
         {
            assert(m != -1 || n != -1 ||  coefs != NULL);

            while(tok != NULL
               && strcmp(tok, "<=") != 0 && strcmp(tok, "=<") != 0
               && strcmp(tok, ">=") != 0 && strcmp(tok, "=>") != 0
               && strcmp(tok, "==") != 0 && strcmp(tok, "=") != 0)
            {
               if (j >= n)
               {
                  fprintf(stderr, "Too many coefficients or no relation sign.\n");
                  deallocate(coefs);
                  fclose(fp);
                  return BP_INVALIDDATA;
               }
               coefs[j] = strtov(tok, &test);
               if ((int)strlen(test) != 0)
               {
                  fprintf(stderr, "Wrong input data in line %i. Wrong type of coefficient (%s) of variable %i.\n", lines, tok, j+1);
                  deallocate(coefs);
                  fclose(fp);
                  return BP_INVALIDDATA;
               }
               tok = strtok(NULL, delimiter);
               j++;
            }

            /* when j < n, then we have not enough coefficient entries in this line */
            if (j < n )
            {
               fprintf(stderr, "Not enough entries in line %i.\n", lines);
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }

            /* when tok is NULL, then there is no relation operator given in this line */
            if (tok == NULL)
            {
               fprintf(stderr, "No relation operator in line %i.\n", lines);
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }

            /* copy relation operator */
            assert(strlen(tok) <= 2);
            strncpy(relation, tok, sizeof(relation));

            /* get next token, which is right-hand side */
            tok = strtok(NULL, delimiter);

            /* when tok is NULL, then there is no right-hand side given in this line */
            if (tok == NULL)
            {
               fprintf(stderr, "Wrong input data in line %i. (no right-hand side)\n", lines);
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }

            /* convert right-hand side and check whether the format is correct */
            rhs = strtov(tok, &test);
            if ((int)strlen(test) != 0)
            {
               fprintf(stderr, "Wrong input data in line %i. (wrong type of right-hand side)\n", lines);
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }

            /* check for additional information after right-hand side */
            tok = strtok(NULL, delimiter);
            if (tok != NULL)
            {
               fprintf(stderr, "Too many information after relation sign of line %i.\n", lines);
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;
            }

            /* add constraint(s) to the problem */
            if (strcmp(relation, "<=") == 0 || strcmp(relation, "=<") == 0 || strcmp(relation, "=") == 0 || strcmp(relation, "==") == 0)
            {
               BP_RETCODE retcode = bp_put(*bp, coefs, rhs);
               if (retcode == BP_INFEASIBLE)
               {
                  deallocate(coefs);
                  fclose(fp);
                  return BP_INFEASIBLE;
               }
               assert(retcode == BP_OKAY);
            }
            if (strcmp(relation, ">=") == 0 || strcmp(relation, "=>") == 0 || strcmp(relation, "=") == 0 || strcmp(relation, "==") == 0)
            {
               for (int k = 0; k < n; k++)
                  coefs[k] *= -1;
               rhs *= -1;
               BP_RETCODE retcode = bp_put(*bp, coefs, rhs);
               if (retcode == BP_INFEASIBLE)
               {
                  deallocate(coefs);
                  fclose(fp);
                  return retcode;
               }
               assert(retcode == BP_OKAY);
            }
            if (strcmp(relation, "=") == 0 || strcmp(relation, "==") == 0)
               m++;

            /* check whether we have too many rows */
            if (bp_getM(*bp) + bp_getRedundant(*bp) > m)
            {
               fprintf(stderr, "Too many rows.\n");
               deallocate(coefs);
               fclose(fp);
               return BP_INVALIDDATA;

            }
         }
      }
   }
   deallocate(coefs);

   if (bp_getM(*bp) + bp_getRedundant(*bp) < m)
   {
      fprintf(stderr, "Not enough rows.\n");
      fclose(fp);
      return BP_INVALIDDATA;
   }

   fclose(fp);

   return BP_OKAY;
}