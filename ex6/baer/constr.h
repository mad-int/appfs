#include <stdbool.h> // bool
#include <stdint.h>  // uint32_t

#ifndef CONSTR_H
#define CONSTR_H

#define MAX_LINE_LEN  512 // Maximum input line length
#define MAX_DIM       32  // Maximum number of dimensions

/**
 * The union of my 32-Bit binary-0-1-vector to enumerate over.
 *
 * This is a union of an uint32 and a bitfield struct
 * for accessing the single bits somewhat directly
 * without bit-operation obfuscation (shifting etc.).
 */
typedef union
{
  uint32_t data;
  struct
  {
    unsigned int _0 : 1; unsigned int _1 : 1; unsigned int _2 : 1; unsigned int _3 : 1;
    unsigned int _4 : 1; unsigned int _5 : 1; unsigned int _6 : 1; unsigned int _7 : 1;

    unsigned int  _8 : 1; unsigned int  _9 : 1; unsigned int _10 : 1; unsigned int _11 : 1;
    unsigned int _12 : 1; unsigned int _13 : 1; unsigned int _14 : 1; unsigned int _15 : 1;

    unsigned int _16 : 1; unsigned int _17 : 1; unsigned int _18 : 1; unsigned int _19 : 1;
    unsigned int _20 : 1; unsigned int _21 : 1; unsigned int _22 : 1; unsigned int _23 : 1;

    unsigned int _24 : 1; unsigned int _25 : 1; unsigned int _26 : 1; unsigned int _27 : 1;
    unsigned int _28 : 1; unsigned int _29 : 1; unsigned int _30 : 1; unsigned int _31 : 1;
  } bits;
} binvec_t;

typedef enum { LESSEQ, GREATEQ, EQUAL } cmp_t;

/**
 * This structs represents the inequalities A and b
 * in A x <= b.
 *
 * The void* is either double* or int* depending
 * if constr.c was compiled with USE_INT (-DUSE_INT) or not.
 */
typedef struct
{
  int rows;
  int cols;
  void* matrix;
  cmp_t* cmp;
  void* rhs; // righthand-side
} constraints_t;

//

/**
 * Get the index-th component of the binary-vector x.
 */
unsigned int get(const binvec_t x, const int index);

/**
 * Check if Ax <= b (x is a feasible solution to the constraints).
 */
bool is_feasible(const constraints_t cs, const binvec_t x);

/**
 * Check if row r2 is reduntant with respect to row r1.
 */
bool is_redunant(const constraints_t cs, const int r1, const int r2);

void swap_rows(constraints_t* cs, const int r1, const int r2);

void delete_row(constraints_t* cs, const int row);

void sort_rows_failfirst(constraints_t* cs);

/**
 * Print the binary-vector x to the stdout.
 *
 * maxdim is the maximal dimension to print
 * e.g. for maxdim 4 a possible output is "(0, 1, 1, 0)".
 */
void print_binvec(const binvec_t x, const int maxdim);

//

/**
 * Allocate memory for the constraints-struct
 * according to n and m dimension.
 */
constraints_t malloc_constraints(const int n, const int m);

/**
 * Read constraints-matrix and vector (A[x] <= b) from a textfile.
 *
 * @param filename name of file to read
 * @return -1 on error and 0 on no error.
 */
constraints_t malloc_constraints_from_file(const char* filename);

/**
 * Free the constraints-struct.
 */
void free_constraints(constraints_t cs);

/**
 * Check if the constraints-struct is valid.
 */
bool is_valid(const constraints_t cs);

/**
 * Check if the constraints-struct is null.
 */
bool is_null(const constraints_t cs);

/**
 * Print a constraints-struct to stdout.
 *
 * This is kind of the reverse operation to
 * malloc_constraints_from_file(), if one pipes
 * the output to a file, it can later loaded
 * with this function again.
 */
void print_constraints(const constraints_t cs);

#endif

