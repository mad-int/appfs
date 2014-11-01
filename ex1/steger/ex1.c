#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define INT_SIZE 4
#define MAX_VALUE 2147483648
int main(int argc, char *argv[]) {
	
	
  	// Usage check
	if (argc != 2) {
		printf("Usage: ex1 [Testfile]");
		return EXIT_FAILURE;
	}
	
	// An array with an entry for every possible value (0 up to MAX_VALUE)
	char *isInFile = malloc(sizeof(char) * MAX_VALUE);
	if(isInFile != NULL) {
    	//printf("Allokation erfolgreich ... \n");
		// Initialize array
		for(unsigned int i = 0; i < MAX_VALUE; i++)
			*(isInFile++) = 0;
		//Reset
		isInFile -= MAX_VALUE;
	}
	else {
    printf("Kein virtueller RAM mehr verfügbar ...\n");
    return EXIT_FAILURE;
	}
	//open file
	FILE *fp;
	fp=fopen(argv[1], "r");
	// could file be opened?
	if (fp==NULL) perror ("Error opening file");
	else {
		// we need a counter for testing purposes only
    	int counter = 0;
    	unsigned char c[4];
    	int sum = 0;
		//printf("Lese Datei...\n");
		// read 4 Bytes at once and transform into (signed) integer-value
		while (fread(c, 1, 4, fp) == INT_SIZE) {
      	  	sum = c[0] | c[1] << 8 | c[2] << 16 | c[3] << 24;
			// if there are negative values in ndata.dat, skip them
			if (sum >= 0) {
				*(isInFile + sum) = 1;
			}
      	  	counter++;
    	}
		// Print elements which occur in file
		for (unsigned int i = 0; i < MAX_VALUE; i++) {
			if(*isInFile == 1) {
				printf("%d\n", i);
			}
			isInFile++;
		}
	isInFile -= MAX_VALUE;
	}
	free(isInFile);
	//Heureka!
	return EXIT_SUCCESS;
}