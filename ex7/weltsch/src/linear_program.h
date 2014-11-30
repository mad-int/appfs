#ifndef _LINEAR_PROGRAM_H
#define _LINEAR_PROGRAM_H

#include <stdbool.h>

/* describes the "format" of a constraint:
 * <= -> LEQ
 * = -> EQ
 * >= -> GEQ
 */
enum constraint_type {LEQ, EQ, GEQ};

typedef struct linear_program LinearProgram;

extern LinearProgram* lp_new(int rows, int cols);

extern void lp_free(LinearProgram* lp);

extern void print_bin_solutions_lp(LinearProgram* lp);

#endif /* _LINEAR_PROGRAM_H */
