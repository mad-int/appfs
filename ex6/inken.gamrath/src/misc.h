#ifndef _MISC_H_
#define _MISC_H_

#define MAX_LINE_LEN 512 // Maximum input line length
#define MAX_STR_LEN 512 // Maximum string length
#define MAX_MATRIX_SIZE 32 // Maximum colums and rows of matrix

#define EPSILON 0.000000001

#define MAX(x,y) ((x) >= (y) ? (x) : (y)) /**< returns maximum of x and y */
#define MIN(x,y) ((x) <= (y) ? (x) : (y)) /**< returns minimum of x and y */

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC) // returns time in seconds

/* define TYPE with int, when INT is defined and DOUBLE is not defined, and with double otherwise */
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

/* convert string to TYPE */
extern TYPE strtov(char* str, char** test);
/* print value to stdout */
extern int vtostr(char* str, TYPE value);

#endif /* _MISC_H_ */
