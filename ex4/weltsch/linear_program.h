#ifndef _LINEAR_PROGRAM_H_
#define _LINEAR_PROGRAM_H_

#include <stdio.h>
#include <stdbool.h>

typedef struct linear_program LinearProgram;

extern LinearProgram *lp_new(int rows, int cols);

extern void lp_free(LinearProgram* lp);

extern bool parse_lp(const char *filename, LinearProgram *lp);

extern LinearProgram *new_lp_from_file(const char *filename);

extern void print_bin_solutions_lp(LinearProgram *lp);

extern void fprint_bin_solutions_lp(FILE *stream, LinearProgram *lp);

#endif
