#include <stdlib.h>
#include <stdio.h>
#include <string.h> // strcmp
#include <ctype.h>  // isspace

#include "numb.h"
int parseStringToNumb(char* string, Numb* ptrToNumb) {

	char* debug;
	Numb value;
#ifdef INT_COEFFS
	value = strtol(string, &debug, 10);
#else
	value = strtod(string, &debug);
#endif
	// whitespaces after the number are okay
	while (isspace(*debug)) {
		debug++;
	}
	if (0 != strcmp(debug, "\0")) {
		return EXIT_FAILURE;
	} else {
		*ptrToNumb = value;
		return EXIT_SUCCESS;
	}
}

int parseNumbToString(char** ptrToString, Numb number) {
	int returnValue;
#ifdef INT_COEFFS
	returnValue=sprintf(*ptrToString,"%i",number);
#else
	returnValue = sprintf(*ptrToString, "%f", number);
#endif
	//printf("number: %f, string: %s",number,*ptrToString);
	if (0 > returnValue) {
		return EXIT_FAILURE;
	} else {
		return EXIT_SUCCESS;
	}
}
