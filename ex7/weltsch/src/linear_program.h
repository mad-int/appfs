#ifndef _LINEAR_PROGRAM_H
#define _LINEAR_PROGRAM_H

#include <stdbool.h>
#include <stdint.h>

#include "num_type.h"

/* describes the "format" of a constraint:
 * <= -> LEQ
 * = -> EQ
 * >= -> GEQ
 */
enum constraint_type {LEQ, EQ, GEQ};

typedef struct linear_program LinearProgram;

extern LinearProgram* lp_new(int rows, int cols);

extern void lp_free(LinearProgram* lp);

extern uint64_t get_bin_solutions_lp(LinearProgram* lp);

extern void print_matrix(LinearProgram* lp);

extern int get_rows(LinearProgram* lp);

extern int get_cols(LinearProgram* lp);

extern void set_coef(LinearProgram *lp, int row, int col, num_t val);

extern void set_rhs(LinearProgram *lp, int row, num_t val);

extern void set_constraint_type(LinearProgram *lp, int row, int type);

extern bool can_overflow(LinearProgram* lp);

#endif /* _LINEAR_PROGRAM_H */
