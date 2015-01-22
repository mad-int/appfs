#include <assert.h>
#include <stdio.h>
#include <time.h>

#include "ineq.h"

// http://en.wikipedia.org/wiki/De_Bruijn_sequence
const unsigned int index32[32] =
{
0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
}; 
#define DEBRUIJN32 0x077CB531U

void solutionFoundCallback(const struct BinaryVector *solution)
{
	PrintSolution(solution);
}

int main(int argc, char **argv)
{
	assert(MAX_VARIABLES <= 32);
	// we store all x's as bits in an uint32_t, don't forget that	

	if (argc < 2)
	{
		printf("Usage: %s filename\n", argv[0]);
		return 0;
	}

	FILE *file = fopen(argv[1], "rb");	
	if (!file)
	{
		printf("Could not open the file %s\n", argv[1]);
		return 0;
	}
	struct IneqSystem *system = NULL; 
	int r = ReadIneqSystemData(file, &system);
	fclose(file);
	if (r)
	{				
		printf("Input data is erroneous.\n");
		return 0;
	}
	clock_t start = clock();
	struct BinaryVector solution;	
	unsigned int solutionsCount = 0;	
	solution.variablesCount = system->variablesCount;

	long double workingSums[system->constraintsCount];
	for (int i = 0; i < system->constraintsCount; i++)
		workingSums[i] = 0.0L;
		
	unsigned int n = 1;// node number
 	unsigned int negn = 1u << (system->variablesCount - 1);
	negn += (negn - 1);

	uint32_t value = 0;
	solution.bitmap = value;
	TestSolutionForSystem(system, &solution, solutionFoundCallback, &solutionsCount, UINT_MAX, workingSums);

	while (negn != 0)
	{
		unsigned int updateMask = n & negn;
		unsigned int columnChanged = index32[(uint32_t)(updateMask * DEBRUIJN32) >> 27];
		/* ((n & negn) == (n & -n)) */
		assert(((n + negn) & ((1u << (system->variablesCount - 1)) + ((1u << (system->variablesCount - 1)) - 1))) == 0);

		++n;
		--negn;
		value ^= updateMask;

		solution.bitmap = value;
		TestSolutionForSystem(system, &solution, solutionFoundCallback, &solutionsCount, columnChanged, workingSums);
	} 

	clock_t end = clock();	
	printf("Solutions count: %d\n", solutionsCount);
	printf("Solving took %6.5lf seconds.\n", (double)(end - start) / (double)CLOCKS_PER_SEC);

	DeallocateIneqSystem(system);

	return 0;
}
