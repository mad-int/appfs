#ifndef _READFILE_H_
#define _READFILE_H_

#include <stdio.h> // fopen
#include <stdbool.h> // bool

#include "def.h"
#include "bp.h"

/* reads model from file */
extern int process_file( const char* filename, BP* prob );

/* check input data of constraint matrix and rhs */
void checkInputData( char* s, int i, int j, bool rhsIndicator, BP* prob, FILE* fp );

#endif /* _READFILE_H_ */
