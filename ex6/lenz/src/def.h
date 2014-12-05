#ifndef _DEF_H_
#define _DEF_H_

#ifdef USE_DOUBLE
#define Value double
#define tostr(X, Y) strtod((X), (Y))
#define BPFORMAT "g"
#else
#define Value int
#define tostr(X, Y) strtol((X), (Y), 10)
#define BPFORMAT "d"
#endif // USE_DOUBLE

#ifndef MAX
#define MAX(x,y)   ((x) >= (y) ? (x) : (y))    /**< returns maximum of x and y */
#endif

#endif // _DEF_H_
