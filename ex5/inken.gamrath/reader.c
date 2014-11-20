#include <string.h> // strpbrk
#include <ctype.h> // isspace
#include <stdio.h> // FILE, fopen
#include <assert.h> // assert
#include <stdlib.h> // strtol

#include "allocate.h"
#include "probdata.h"

// #define debug

/* read problem data from file
 * returns amount of lines
 * returns -1 when there was a reading error
 * returns -2 when the problem is infeasible
 */
int process_file(const char* filename, Matrix** matrix)
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
      return -1;
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
#ifdef debug
      fprintf(stdout, "line %i: %s\n", lines, s);
#endif
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
               return -1;
            }
            if (n <= 0 || n > MAX_MATRIX_SIZE)
            {
               fprintf(stderr, "Dimension of columns is not right. (%i not in {1, ..., %i})\n", n, MAX_MATRIX_SIZE);
               return -1;
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
               return -1;
            }
            if (m <= 0 || m > MAX_MATRIX_SIZE)
            {
               fprintf(stderr, "Dimension of rows is not right. (%i not in {1, ..., %i})\n", m, MAX_MATRIX_SIZE);
               deallocate(coefs);
               return -1;
            }
            *matrix = matrix_new(m, n);
         break;
         }

         /* other lines contain rows (constraints) of matrix and right-hand sides */
         default:
         {
            if (m == -1 || n == -1 ||  coefs == NULL)
            {
               fprintf(stderr, "Dimension not set or coefficient array not allocated.\n");
               deallocate(coefs);
               return -1;
            }

            while(tok != NULL
               && strcmp(tok, "<=") != 0 && strcmp(tok, "=<") != 0
               && strcmp(tok, ">=") != 0 && strcmp(tok, "=>") != 0
               && strcmp(tok, "==") != 0 && strcmp(tok, "=") != 0)
            {
               if (j >= n)
               {
                  fprintf(stderr, "Too many coefficients or no relation sign.\n");
                  deallocate(coefs);
                  return -1;
               }
               coefs[j] = strtov(tok, &test);
               if ((int)strlen(test) != 0)
               {
                  fprintf(stderr, "Wrong input data in line %i. Wrong type of coefficient (%s) of variable %i.\n", lines, tok, j+1);
                  deallocate(coefs);
                  return -1;
               }
               tok = strtok(NULL, delimiter);
               j++;
            }

            /* when j < n, then we have not enough coefficient entries in this line */
            if (j < n )
            {
               fprintf(stderr, "Not enough entries in line %i.\n", lines);
               deallocate(coefs);
               return -1;
            }

            /* when tok is NULL, then there is no relation operator given in this line */
            if (tok == NULL)
            {
               fprintf(stderr, "No relation operator in line %i.\n", lines);
               deallocate(coefs);
               return -1;
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
               return -1;
            }

            /* convert right-hand side and check whether the format is correct */
            rhs = strtov(tok, &test);
            if ((int)strlen(test) != 0)
            {
               fprintf(stderr, "Wrong input data in line %i. (wrong type of right-hand side)\n", lines);
               deallocate(coefs);
               return -1;
            }

            /* check for additional information after right-hand side */
            tok = strtok(NULL, delimiter);
            if (tok != NULL)
            {
               fprintf(stderr, "Too many information after relation sign of line %i.\n", lines);
               deallocate(coefs);
               return -1;
            }

            /* add constraint(s) to the problem */
            if (strcmp(relation, "<=") == 0 || strcmp(relation, "=<") == 0 || strcmp(relation, "=") == 0 || strcmp(relation, "==") == 0)
            {
               if (!matrix_put(*matrix, coefs, rhs))
               {
                  fprintf(stderr, "Problem is infeasible.\n");
                  deallocate(coefs);
                  return -2;
               }
            }
            if (strcmp(relation, ">=") == 0 || strcmp(relation, "=>") == 0 || strcmp(relation, "=") == 0 || strcmp(relation, "==") == 0)
            {
               for (int k = 0; k < n; k++)
                  coefs[k] *= -1;
               rhs *= -1;
               if (!matrix_put(*matrix, coefs, rhs))
               {
                  fprintf(stderr, "Problem is infeasible.\n");
                  deallocate(coefs);
                  return -2;
               }
            }
            if (strcmp(relation, "=") == 0 || strcmp(relation, "==") == 0)
               m++;

            /* check whether we have too many rows */
            if (matrix_getM(*matrix) + matrix_getRedundant(*matrix) > m)
            {
               fprintf(stderr, "Too many rows.\n");
               deallocate(coefs);
               return -1;

            }
         }
      }
   }
   deallocate(coefs);

   if (matrix_getM(*matrix) + matrix_getRedundant(*matrix) < m)
   {
      fprintf(stderr, "Not enough rows.\n");
      return -1;
   }

   fclose(fp);

   return lines;
}