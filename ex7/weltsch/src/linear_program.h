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

extern bool lp_is_valid(LinearProgram* lp);

extern uint64_t lp_get_bin_solutions(LinearProgram* lp);

extern void lp_print_matrix(LinearProgram* lp);

extern int lp_get_rows(LinearProgram* lp);

extern int lp_get_cols(LinearProgram* lp);

extern void lp_set_coef(LinearProgram *lp, int row, int col, num_t val);

extern void lp_set_rhs(LinearProgram *lp, int row, num_t val);

extern void lp_set_constraint_type(LinearProgram *lp, int row, int type);

extern bool lp_can_overflow(LinearProgram* lp);

#endif /* _LINEAR_PROGRAM_H */
