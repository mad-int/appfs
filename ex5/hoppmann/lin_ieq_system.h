/*
 * ieq_system.h
 *
 *  Created on: 12.11.2014
 *      Author: bzfhoppm
 */

#ifndef LIN_IEQ_SYSTEM_H_
#define LIN_IEQ_SYSTEM_H_

#include <stdbool.h>
#include "allocate.h"


/**
 * Representing a system of linear inequalities
 *
 * e.g. the system
 *
 * 2 3 5 4 <= 8
 * 3 6 0 8 >= 10
 * 0 0 1 1 <= 1
 * 0 0 1 3 = 1
 *
 *
 */
typedef struct linear_inequality_system LinIeqSys;

typedef union Number { int i; double d;} number;

enum ineq_type { leq, eq, geq };

typedef enum ineq_type ineq_t;

extern int getInt(number n);

extern double getDouble(number n);

/** creates a linIeqSys represented by a matrix*/
extern LinIeqSys* linIeqSys_new(int num_cons, int num_vars);

/** frees a linIeqSys */
extern void linIeqSys_free(LinIeqSys* linIeqSys);

/** set the number of variables*/
extern void linIeqSys_setNVars(LinIeqSys* linIeqSys, int n_vars);

/** set the number of constraints*/
extern void linIeqSys_setNCons(LinIeqSys* linIeqSys, int n_cons);

/** puts a at row i, column j of the matrix representing the linIeqSys */
extern void linIeqSys_put(LinIeqSys* linIeqSys, int row_i, int col_j, number a);

/** puts a at row i, column j of the matrix representing the linIeqSys */
extern void linIeqSys_setType(LinIeqSys* linIeqSys, int row_i, ineq_t type);

/** puts b at RHS of row i */
extern void linIeqSys_putRHS(LinIeqSys* linIeqSys, int row_i, number b);

/** get the number of constraints of the linIeqSys */
extern int linIeqSys_nCons(LinIeqSys* linIeqSys);

/** get the number of variables of the linIeqSys */
extern int linIeqSys_nVars(LinIeqSys* linIeqSys);

/** prints linIeqSys to standard output */
extern void linIeqSys_print(LinIeqSys* linIeqSys);

/** solve the system of linear inequalities by total enumeration */
extern void linIeqSys_solve(LinIeqSys* linIeqSys);

#endif /* LIN_IEQ_SYSTEM_H_ */
