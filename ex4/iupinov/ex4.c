#include <assert.h>
#include <stdio.h>

#include "ineq.h"

int main(int argc, char **argv)
{
	assert(MAX_VARIABLES <= 32);
	// we store all x's as bits in an uint32_t, don't forget that	

	if (argc < 2)
	{
		printf("Usage: ex4 filename\n");
		return 1;
	}

	FILE *file = fopen(argv[1], "rb");	
	if (!file)
	{
		printf("Could not open the file %s\n", argv[1]);
		return 1;
	}
	struct IneqSystem *system = ReadIneqSystemData(file);
	fclose(file);

	
	struct BinaryVector solution;	
	int solutionsCount = 0;	
	solution.variablesCount = system->variablesCount;
	uint32_t max = (1 << system->variablesCount);
	if (system->variablesCount == MAX_VARIABLES)
		max = 0;
	uint32_t value = 0;
	do
	{
		solution.bitmap = value;
		int res = TestSolutionForSystem(system, &solution);
		if (!res)
		{
			solutionsCount++;
			PrintSolution(&solution);
		}
		++value;
	} 
	while (value != max);
	printf("Solutions count: %d\n", solutionsCount);

	DeallocateIneqSystem(system);

	return 0;
}
