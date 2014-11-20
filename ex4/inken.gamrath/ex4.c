#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <ctype.h> // isspace
#include <math.h> // pow
#include <time.h> // clock

#include "allocate.h"
#include "probdata.h"
#include "queue.h"

#define MAX_LINE_LEN 512 // Maximum input line length
#define MAX_STR_LEN 64 // Maximum string length
#define MAX_MATRIX_SIZE 32 // Maximum colums and rows of matrix

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC) // returns time in seconds

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
   char delimiter[2] = " \0";
   int lines = 0;
   int m = -1;
   int n = -1;
   float* coefs = NULL;
   float rhs;
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
      switch(i)
      {
         /* first line contains amount of columns (variables) */
         case 1:
            n = strtol(s, NULL, 10);
            assert(n > 0);
            assert(n <= MAX_MATRIX_SIZE);
            coefs = allocate(n, sizeof(*coefs));
            assert(coefs != NULL);
         break;

         /* second line contains amount of rows (constraints) */
         case 2:
            m = strtol(s, NULL, 10);
            assert(m > 0);
            assert(m <= MAX_MATRIX_SIZE);
            *matrix = matrix_new(m, n);
         break;

         /* other lines contain rows (constraints) of matrix and right-hand sides */
         default:
            assert(m != -1);
            assert(n != -1);
            assert(coefs != NULL);
            assert(s != NULL);
            tok = strtok(s, delimiter);
            while(strcmp(tok, "<=") != 0 && tok != NULL)
            {
               assert(j < n);
               coefs[j] = strtof(tok, NULL);
               tok = strtok(NULL, delimiter);
               j++;
            }
            if (j != n || tok == NULL)
               return -1;
            tok = strtok(NULL, delimiter);
            assert(tok != NULL);
            rhs = strtof(tok, NULL);
            if( !matrix_put(*matrix, coefs, rhs) )
            {
               fprintf(fp, "Problem is infeasible.\n");
               return -2;
            }
      }
   }
   deallocate(coefs);
   assert(m >= matrix_getM(*matrix));
   assert(m == matrix_getSize(*matrix));
   fclose(fp);

   assert(i <= MAX_MATRIX_SIZE + 2);

   return lines;
}

bool solveLP(Matrix* matrix, FILE* fp)
{
   int* values;
   bool valid;
   bool success;
   int m;
   int n;
   long int length;
   float* coefs;
   float* rhs;
   float sum;
   long int p;

   m = matrix_getM(matrix);
   n = matrix_getN(matrix);
   values = allocate(n, sizeof(*values));
   coefs = matrix_getCoefs(matrix);
   rhs = matrix_getRhs(matrix);

   success = false;
   length = pow(2, n);
   for (long int i = 0; i < length; i++)
   {
//       printf("[%li]\n", i);
      valid = true;
      for (int j = 0; j < m; j++)
      {
         sum = 0.0;
         p = 1;
         for (int k = 0; k < n; k++)
         {
//             values[k] = (int)(floor(i / pow(2.0, k))) % 2;
            values[k] = (int)(floor(i / p)) % 2;
            sum += coefs[j*n+k] * values[k];
            p *= 2;
         }
         if (sum > rhs[j])
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
         for (int k = 0; k < n; k++)
         {
            fprintf(fp, "%i", values[k]);
            if (k < n-1)
               fprintf(fp, " ");
            else
#ifndef debug
               fprintf(fp, "\n");
#else
               fprintf(fp, ": %i\n", valid);
#endif
         }
#ifndef debug
      }
#endif
   }
   return success;
}

int main(int argc, char** argv)
{
   Matrix* matrix = NULL;          /* problem data */
   int lines = -1;                 /* line number of input file */
   FILE* fp;                       /* output file */
//    char filename[MAX_STR_LEN];     /* name of output file */
   clock_t start;                  /* clock to meassure running time */

   /* start clock */
   start = clock();

   /* check whether there are enough arguments given */
   if (argc < 2 || strlen(argv[0]) <= 0)
   {
      fprintf(stderr, "usage: %s filename\n", argv[0]);
      return EXIT_FAILURE;
   }

//    /* creates name of output file
//     * <filename>.<extension> => <filename>.sol
//     */
//    sscanf(argv[1], "%[^.].%*[^.]", filename);
//    sprintf(filename, "%s.sol", filename);
//    fp = fopen(filename , "w");
   fp = stdout;

   /* read problem data from file and creates problem data
    * returns amount of lines
    * returns -1 when there was a reading error
    * returns -2 when the problem is infeasible
    */
   lines = process_file(argv[1], &matrix);
   if (lines == -1)
      return EXIT_FAILURE;
   if (lines == -2)
   {
      fprintf(fp, "Problem is infeasible.\n");
      return EXIT_SUCCESS;
   }

#ifdef debug
   printf("m: %i, n: %i\n", matrix_getM(matrix), matrix_getN(matrix));
#endif

   /* solve the problem */
   if (!solveLP(matrix, fp))
      fprintf(fp, "Problem is infeasible.\n");

   /* free problem data */
   matrix_free(matrix);

   fprintf(stdout, "Running time: %.2f seconds.\n", GET_SEC(start, clock()));

   /* close output file */
   if (fp != stdout)
      fclose(fp);

   return EXIT_SUCCESS;
}