#ifndef _SOLUTIONS_H_
#define _SOLUTIONS_H_

#include "bp.h"

int next_sol(int* sol, int nvars );

int test_sol( int* sol, BP* prob );

void find_binary_solutions( BP* prob );

#endif //  _SOLUTIONS_H_
