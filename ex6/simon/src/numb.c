#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h> // strcmp
#include <ctype.h>  // isspace
#include <limits.h> // max/min values for int
#include <math.h>   // isinf for double overflow test
// local
#include "numb.h"

int parseStringToNumb(char* string, Numb* ptrToNumb) {
//TODO: maybe check if single values to not exit min/max values and min precision in case of double
    char* debug;
    Numb value;
#ifdef INT_COEFFS
    value = (int)strtol(string, &debug, 10);
#else
    value = strtod(string, &debug);
#endif
    // whitespaces after the number are okay
    while (isspace(*debug)) {
        debug++;
    }
    if (0 != strcmp(debug, "\0")) {
        return -1;
    } else {
        *ptrToNumb = value;
        return 1;
    }
}

void parseNumbToString(char** ptrToString, Numb number) {
#ifdef INT_COEFFS
    sprintf(*ptrToString,"%i",number);
#else
    // uses the shortest representation of a float: floating pojnt number or scientific format
    sprintf(*ptrToString, "%G", number);
#endif
}

int numbsEqual(Numb x, Numb y){
#ifdef INT_COEFFS
    return x == y;
#else
    return doublesEqual((double)x, (double)y);
#endif
}

int numbSmallerNumb(Numb x, Numb y){
#ifdef INT_COEFFS
    return x < y;
#else
    return doubleSmallerDouble((double)x, (double)y);
#endif
}

int numbGreaterNumb(Numb x, Numb y){
#ifdef INT_COEFFS
    return x > y;
#else
    return doubleGreaterDouble((double)x, (double)y);
#endif
}

int doublesEqual(double x, double y){
    return EPSILON >= fabs(x-y);
}

int doubleSmallerDouble(double x, double y){
    return EPSILON < (y-x);
}

int doubleGreaterDouble(double x, double y){
    return EPSILON < (x-y);
}

int sumHasPositiveOverflow(Numb x, Numb y){
#ifdef INT_COEFFS
    assert(y>0);
    return INT_MAX - y < x;
#else
    return isinf(x+y);
#endif
}

int sumHasNegativeOverflow(Numb x, Numb y){
#ifdef INT_COEFFS
    assert(y<0);
    return INT_MIN - y > x;
#else
    return isinf(x+y);
#endif
}

