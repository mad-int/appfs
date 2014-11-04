#include <stdio.h>
#include <stdlib.h>

int intSortCompare(const void *a, const void *b)
{
	int la = *(int *)a;
	int lb = *(int *)b;	
	if (la > lb)
		return 1;
	else if (la < lb)
		return -1;
	else 
		return 0;
}

int main(int argc, char **argv)
{
	if (argc < 2)
	{
		printf("Usage: ex1 filename\n");
		return 1;
	}

   	FILE *file = fopen(argv[1], "rb");
    	if (!file)
	{
		printf("Could not open the file %s\n", argv[1]);
		return 1;
	}

	fseek(file, 0, SEEK_END);
	const int fileSize = ftell(file);
	fseek(file, 0L, SEEK_SET);

	int *numbers = (int *)malloc(fileSize);
	if (!numbers)
	{
		printf("Could not allocate memory.\n");
		fclose(file);
		return 1;		
	}
	int r = fread(numbers, 1, fileSize, file);
	fclose(file);
	if (r != fileSize)
	{
		printf("Could not read the file contents.\n");
		free(numbers);
		return 1;		
	}
	const int numbersCount = fileSize / sizeof(int); 

	qsort(numbers, numbersCount, sizeof(int), intSortCompare);

	int i;
		
	for (i = 0; i < numbersCount; i++)
	{	
		if ((numbers[i] < 0) || ((i > 0) && (numbers[i] == numbers[i - 1])))
			continue;
		
		printf("%d\n", numbers[i]); 
	}

	free(numbers);

	return 0;
}
