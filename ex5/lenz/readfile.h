#ifndef _READFILE_H_
#define _READFILE_H_

#include <stdbool.h> // bool

#define INT

#ifdef DOUBLE
typedef struct binaryProgram
{
    double** conss;
    double*  rhs;
    char* eq_type;
    int   nconss;
    int   nvars;
} BP;
#else
typedef struct binaryProgram
{
    int** conss;
    int*  rhs;
    char* eq_type;
    int   nconss;
    int   nvars;
} BP;
#endif // DOUBLE

extern int process_file( const char* filename, BP* prob );

/* check input data for constraint matrix and rhs */
void checkInputData( char* s, int i, int j, bool rhsIndicator );

/* prints optimizaton problem */
void print_problem( BP* prob );

#endif /* _READFILE_H_ */
