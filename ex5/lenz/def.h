#ifndef _DEF_H_
#define _DEF_H_

#ifdef USE_DOUBLE
#define Value double
#define tostr(X, Y) strtol((X), (Y), 10)
#define BPFORMAT "g"
#else
#define Value int
#define tostr(X, Y) strtod((X), (Y))
#define BPFORMAT "d"
#endif

#endif // _DEF_H_
