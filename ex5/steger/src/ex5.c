#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "allocate.h"
#include "ilpParser.h"
#include "linearProgram.h"
#include <time.h>



	
// This method reads from filename the constraints of a LP
// The format of the file has to be similar to the following:
/*
								
								
3 # columns (variables)
5 # rows (constraints)
2 3 -5 4 5 <= 8
3 6 0 8 6 <= 10
0 0 1 1 7 <= 1
								
								
*/

int main(int argc, char** argv)
{
   if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }
	struct linearProgram* ilp = createLPFromFile(argv[1]);
	// compute feasible vectors from ILP
	clock_t enumeration_start=clock();
	int numberOfFeasibles = giveFeasibles(ilp);
	clock_t enumeration_end = clock();
	
	// print feasible vectors
	printBinaryVectors(ilp);
	printf("\nWe got %d feasible solutions in total!\n", numberOfFeasibles); 
	printf("Laufzeit der Enumeration: %.2f Sekunden\n",(float)(enumeration_end-enumeration_start) / CLOCKS_PER_SEC);
	// free all allocated memory
	deleteLinearProgram(ilp);
   
   return EXIT_SUCCESS;
}