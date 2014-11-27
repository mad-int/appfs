#ifndef _DEF_H_
#define _DEF_H_

#define MAX_LINE_LEN 512 // Maximum input line length
#define MAX_STR_LEN 512 // Maximum string length
#define MAX_MATRIX_SIZE 32 // Maximum colums and rows of matrix

#define MAX(x,y)      ((x) >= (y) ? (x) : (y))     /**< returns maximum of x and y */
#define MIN(x,y)      ((x) <= (y) ? (x) : (y))     /**< returns minimum of x and y */

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

#endif /* _DEF_H_ */