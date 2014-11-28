#ifndef _READFILE_H_
#define _READFILE_H_

#include <stdbool.h> // bool

#include "def.h"
#include "bp.h"

extern int process_file( const char* filename, BP* prob );

/* check input data for constraint matrix and rhs */
void checkInputData( char* s, int i, int j, bool rhsIndicator, BP* prob );

#endif /* _READFILE_H_ */
