#ifndef _TEMPLATE_H_
#define _TEMPLATE_H_

#include <time.h> // clock

#define MAX_LINE_LEN 512 // Maximum input line length
#define MAX_STR_LEN 64 // Maximum string length
#define MAX_MATRIX_SIZE 32 // Maximum colums and rows of matrix

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC) // returns time in seconds

#if !defined(DOUBLE) && defined(INT)
#define TYPE int
#else /* if !defined DOUBLE && defined INT */
#define TYPE double

#ifndef DOUBLE
#define DOUBLE
#endif /* #ifndef DOUBLE */

#ifdef INT
#undef INT
#endif /* #ifdef INT */

#endif /* if !defined DOUBLE && defined INT */


#include "probdata.h"

/* convert string to TYPE */
extern TYPE strtov(char* str, char** test);
/* print value to stdout */
extern int vtostr(char* str, TYPE value);

#endif /* _TEMPLATE_H_ */