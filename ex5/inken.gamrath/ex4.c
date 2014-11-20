#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <math.h> // pow

#include "allocate.h"
#include "probdata.h"
#include "reader.h"

// #define debug

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