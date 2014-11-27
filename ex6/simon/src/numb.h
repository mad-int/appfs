#ifndef _NUMB_H_
#define _NUMB_H_

// precision for double comparison
#define EPSILON 0.000001 // 10 ^ -6

#ifdef INT_COEFFS

typedef int Numb;
#define number_type "integer"

#else

typedef double Numb;
#define number_type "double"

#endif

int parseStringToNumb(char* string, Numb* ptrToNumb);

void parseNumbToString(char** ptrToString, Numb number);

int numbsEqual(Numb x, Numb y);

int numbSmallerNumb(Numb x, Numb y);

int numbGreaterNumb(Numb x, Numb y);

int doublesEqual(double x, double y);

int doubleSmallerDouble(double x, double y);

int doubleGreaterDouble(double x, double y);

int sumHasPositiveOverflow(Numb x, Numb y);

int sumHasNegativeOverflow(Numb x, Numb y);
#endif /* _NUMB_H_ */
