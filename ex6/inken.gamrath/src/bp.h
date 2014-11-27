#ifndef _PROBDATA_H_
#define _PROBDATA_H_

#include <stdbool.h>

#include "def.h"
#include "type_bp.h"
#include "type_retcode.h"

/* creates a new binary program */
extern BinaryProgram* bp_new(int m, int n);
/* frees binary program data */
extern void bp_free(BinaryProgram* bp);
/* adds a new row (constraint) */
extern BP_RETCODE bp_put(BinaryProgram* bp, TYPE* coefs, TYPE rhs);
/* gets the number of rows (constraints) */
extern int bp_getM(BinaryProgram* bp);
#if 0
/* gets the number of columns (variables) */
extern int bp_getN(BinaryProgram* bp);
#endif
#if 0
/* gets entries of the binary program */
extern TYPE* bp_getCoefs(BinaryProgram* bp);
#endif
#if 0
/* gets right-hand sides of the constraints */
extern TYPE* bp_getRhs(BinaryProgram* bp);
#endif
#if 0
/* gets maximal number of rows (constraints) */
extern int bp_getSize(BinaryProgram* bp);
#endif
/* gets redundant number of rows (constraints) */
extern int bp_getRedundant(BinaryProgram* bp);
#if 0
/* determines whether the binary program is empty */
extern bool bp_is_empty(const BinaryProgram* bp);
#endif
/* solves the BP */
extern BP_RETCODE solveBP(BinaryProgram* bp, FILE* fp);
/* solves the BP with branching tree */
extern BP_RETCODE solveBT(BinaryProgram* bp, FILE* fp);
/* branches in the tree */
int branch(FILE* fp, BinaryProgram* bp, TYPE* lhs, TYPE* min, TYPE* max, int depth, char* sol);

#endif /* _PROBDATA_H_ */