#include <stdio.h>
#include <stdlib.h>

// local
#include "constraintArray.h"

int main(int argc, char** argv) {
	ConstArray* c;
	if (argc < 2) {
		printf("usage: %s filename\n", argv[0]);
		printf("Reading dummy data instead!\n");
		c = createDummyConstArray();
		if (EXIT_FAILURE == printConstArray(c)) {
			return EXIT_FAILURE;
		}
	} else {
		c = read_constArray_from_file(argv[1]);
	}

	if (NULL == c) {
		return EXIT_FAILURE;
	}
	printAllFeasibleBinaryAssignments(c);
	freeConstArray(c);

	return EXIT_SUCCESS;
}
