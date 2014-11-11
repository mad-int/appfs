#ifndef _LINEAR_PROGRAM_H_
#define _LINEAR_PROGRAM_H_

#include <stdio.h>

typedef struct linear_program LinearProgram;

LinearProgram *lp_new(int rows, int cols);

void lp_free(LinearProgram* lp);

LinearProgram *parse_lp(char *filename, LinearProgram *lp);

void print_bin_solutions_lp(LinearProgram *lp);

void fprint_bin_solutions_lp(FILE *stream, LinearProgram *lp);

#endif
