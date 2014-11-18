#include <string.h> // strpbrk
#include <ctype.h> // isspace
#include <stdio.h> // FILE, fopen
#include <assert.h> // assert
#include <stdlib.h> // strtol

#include "allocate.h"
#include "probdata.h"

#define MAX_LINE_LEN 512 // Maximum input line length
#define MAX_STR_LEN 64 // Maximum string length
#define MAX_MATRIX_SIZE 32 // Maximum colums and rows of matrix

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
   char relation[2];
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
      printf("%s\n", s);
      tok = strtok(s, delimiter);
      switch(i)
      {
         /* first line contains amount of columns (variables) */
         case 1:
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

         /* second line contains amount of rows (constraints) */
         case 2:
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

         /* other lines contain rows (constraints) of matrix and right-hand sides */
         default:
            if (m == -1 || n == -1 ||  coefs == NULL || s == NULL)
            {
               fprintf(stderr, "Dimension not set or coefficient array not allocated or line is NULL.\n");
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
#if defined INT
               coefs[j] = strtol(tok, &test, 10);
#elif defined DOUBLE
               coefs[j] = strtod(tok, &test);
#else
               fprintf(stderr, "Wrong type defined.\n");
               deallocate(coefs);
               return -1;
#endif /* INT */
               if ((int)strlen(test) != 0)
               {
                  fprintf(stderr, "Wrong input data in line %i. Wrong type of coefficient (%s) of variable %i.\n", lines, tok, j+1);
                  deallocate(coefs);
                  return -1;
               }
               tok = strtok(NULL, delimiter);
               j++;
            }
            if (j < n )
            {
               fprintf(stderr, "Not enough entries in line %i.\n", lines);
               deallocate(coefs);
               return -1;
            }
            if (tok == NULL)
            {
               fprintf(stderr, "No relation operator in line %i.\n", lines);
               deallocate(coefs);
               return -1;
            }
            /* copy relation operator */
            strcpy(relation, tok);
            tok = strtok(NULL, delimiter);
            if (tok == NULL)
            {
               fprintf(stderr, "Wrong input data in line %i. (no right-hand side)\n", lines);
               deallocate(coefs);
               return -1;
            }
#if defined INT
            rhs = strtol(tok, &test, 10);
#elif defined DOUBLE
            rhs = strtod(tok, &test);
#else
            fprintf(stderr, "Wrong type defined.\n");
            deallocate(coefs);
            return -1;
#endif /* INT */
            if ((int)strlen(test) != 0)
            {
               fprintf(stderr, "Wrong input data in line %i. (wrong type of right-hand side)\n", lines);
               deallocate(coefs);
               return -1;
            }

            tok = strtok(NULL, delimiter);
            if (tok != NULL)
            {
               fprintf(stderr, "Too many information after right-hand side of line %i.\n", lines);
               deallocate(coefs);
               return -1;
            }

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
            if (matrix_getM(*matrix) + matrix_getRedundant(*matrix) > m)
            {
               fprintf(stderr, "Too many rows.\n");
               deallocate(coefs);
               return -1;

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