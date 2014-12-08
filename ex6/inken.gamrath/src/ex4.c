#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <math.h> // pow
#include <time.h> // clock_t

#include "allocate.h"
#include "bp.h"
#include "reader.h"


int main(
   int argc,               /**< number of arguments */
   char** argv             /**< argument array */
)
{
   BinaryProgram* bp = NULL;       /* problem data */
   FILE* fp;                       /* output file */
   clock_t start;                  /* clock to meassure running time */

   /* start clock */
   start = clock();

   /* check whether there are enough arguments given */
   if (argc < 2 || strlen(argv[0]) <= 0)
   {
      fprintf(stderr, "usage: %s filename\n", argv[0]);
      return EXIT_FAILURE;
   }

#ifdef OUTFILE
   /* creates name of output file
    * <filename>.<extension> => <filename>.sol
    */
   char filename[MAX_STR_LEN];     /* name of output file */
   sscanf(argv[1], "%[^.].%*[^.]", filename);
   sprintf(filename, "%s.sol", filename);
   fp = fopen(filename , "w");
#else /* OUTFILE */
   fp = stdout;
#endif /* OUTFILE */

   /* read problem data from file and creates problem data
    * returns amount of lines
    * returns -1 when there was a reading error
    * returns -2 when the problem is infeasible
    */
   BP_RETCODE retcode =  process_file(argv[1], &bp);
   if (retcode == BP_INFEASIBLE)
   {
      if (bp != NULL)
         bp_free(bp);
      fprintf(fp, "0 solutions found.\n");
      fprintf(fp, "Problem is infeasible.\n");
      fprintf(stdout, "Running time: %.2f seconds.\n", GET_SEC(start, clock()));
      return EXIT_SUCCESS;
   }
   if (retcode == BP_INVALIDDATA || retcode == BP_NOFILE || retcode == BP_ERROR || retcode == BP_READERROR)
   {
      if (bp != NULL)
         bp_free(bp);
      fprintf(stdout, "Running time: %.2f seconds.\n", GET_SEC(start, clock()));
      return EXIT_FAILURE;
   }
   assert(retcode == BP_OKAY);

   clock_t enumeration = clock();

   /* solve the problem */
#ifdef BRANCHING
   retcode = solveBT(bp, fp);
#else
   retcode = solveBP(bp, fp);
#endif
   assert(retcode == BP_OKAY || retcode == BP_INFEASIBLE);
   if ( retcode == BP_INFEASIBLE)
      fprintf(fp, "Problem is infeasible.\n");
   fprintf(stdout, "Enumeration time: %.2f seconds.\n", GET_SEC(enumeration, clock()));

   /* free problem data */
   bp_free(bp);

#ifdef OUTFILE
   /* close output file */
   if (fp != stdout)
      fclose(fp);
#endif /* OUTFILE */

   fprintf(stdout, "Running time: %.2f seconds.\n", GET_SEC(start, clock()));

   return EXIT_SUCCESS;
}
