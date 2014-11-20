#ifndef _NUMB_H_
#define _NUMB_H_

#ifdef INT_COEFFS

typedef int Numb;
#define number_type "integer"

#else

typedef double Numb;
#define number_type "double"

#endif

int parseStringToNumb(char* string, Numb* ptrToNumb);

int parseNumbToString(char** ptrToString, Numb number);

#endif /* _NUMB_H_ */
