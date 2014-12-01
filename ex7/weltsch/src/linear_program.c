#include <assert.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "allocate.h"
#include "linear_program.h"

#define GET_SEC(a, b) ((double) (b - a) / (double)CLOCKS_PER_SEC)

struct linear_program {
    int rows;
    int cols;
    num_t** matrix;
    num_t* vector;
    int* constraint_types;
};

bool can_overflow(LinearProgram* lp) {
    assert(lp_is_valid(lp));

    for(int r = 0; r < lp->rows; r++)
    {
        num_t row_max = 0;
        num_t row_min = 0;

        for(int c = 0; c < lp->cols; c++)
        {
            num_t val = lp->matrix[r][c];

            if (val > 0)
            {
                if (row_max < MAX_COEF_VAL - val) {
                    row_max += val;
                } else {
                    fprintf(stderr, "Error: row %d numerical overflow\n", r);
                    return true;
                }
            } else if (val < 0) {
                if (row_min > MIN_COEF_VAL - val) {
                    row_min += val;
                } else {
                    fprintf(stderr, "Error: row %d numerical negative overflow\n", r);
                    return true;
                }
            } else {
                assert(val == 0);
            }
        }
    }

    return false;
}

bool lp_is_valid(LinearProgram* lp) {
    if (NULL == lp) {
        return false;
    }

    if (lp->rows <= 0 || lp->cols <= 0) {
        return false;
    }

    return lp &&
        lp->cols &&
        lp->rows &&
        lp->matrix &&
        lp->vector &&
        lp->constraint_types;
}

LinearProgram* lp_new(int rows, int cols) {
    assert(rows > 0);
    assert(cols > 0);

    LinearProgram* lp = allocate(1, sizeof(*lp));
    lp->rows = rows;
    lp->cols = cols;

    num_t** matrix = allocate(rows, sizeof(*matrix));

    int i;
    // initialize every row with 0s
    for (i = 0; i < rows; i++) {
        matrix[i] = allocate(cols, sizeof(*matrix[i]));
    }

    num_t* vector = allocate(rows, sizeof(*vector));
    int* constraint_types = allocate(rows, sizeof(*constraint_types));

    lp->matrix = matrix;
    lp->vector = vector;
    lp->constraint_types = constraint_types;

    assert(lp_is_valid(lp));
    return lp;
}

void lp_free(LinearProgram* lp) {
    assert(lp_is_valid(lp));
    int i;
    for (i = 0; i < lp->rows; i++) {
        deallocate(lp->matrix[i]);
    }

    deallocate(lp->matrix);
    deallocate(lp->vector);
    deallocate(lp->constraint_types);
    deallocate(lp);
}

int get_rows(LinearProgram* lp) {
    assert(lp_is_valid(lp));
    return lp->rows;
}

int get_cols(LinearProgram* lp) {
    assert(lp_is_valid(lp));
    return lp->cols;
}

void set_coef(LinearProgram *lp, int row, int col, num_t val) {
    assert(lp_is_valid_lp);
    assert(row >= 0 && row < lp->rows);
    assert(col >= 0 && col < lp->cols);
    assert(val <= MAX_COEF_VAL && val >= MIN_COEF_VAL);

    lp->matrix[row][col] = val;
}

void set_rhs(LinearProgram *lp, int row, num_t val) {
    assert(lp_is_valid_lp);
    assert(row >= 0 && row < lp->rows);
    assert(val <= MAX_COEF_VAL && val >= MIN_COEF_VAL);

    lp->vector[row] = val;
}

void set_constraint_type(LinearProgram *lp, int row, int type) {
    assert(lp_is_valid_lp);
    assert(row >= 0 && row < lp->rows);
    assert(val <= MAX_COEF_VAL && val >= MIN_COEF_VAL);

    lp->constraint_types[row] = type;
}

/* return nth greycode */
uint64_t next_vars(uint64_t n) {
    return n ^ n >> 1;
}

bool __get_nth_bit(uint64_t bits, int n) {
    assert(n >= 0);
    assert(n < 64);

    return 0 != (1u & bits >> n);
}

/* print a solution vector */
void __print_vars(uint64_t vars, int len) {
    assert(0 < len);
    assert(len < 64);

    int j;
    for (j = 0; j < len; j++) {
        printf("%d ", __get_nth_bit(vars, j));
    }

    printf("\n");
}

bool __is_feasible_sum(num_t sum, int row, LinearProgram* lp) {
    assert(lp_is_valid(lp));
    assert(row >= 0);
    assert(row < lp->rows);

    if (lp->constraint_types[row] == LEQ && lp->vector[row] < sum) {
        return false;
    } else if (lp->constraint_types[row] == GEQ && lp->vector[row] > sum) {
        return false;
    } else if (lp->constraint_types[row] == EQ && lp->vector[row] != sum) {
        return false;
    }
    return true;
}

/* check if a vector is a feasible solution to the lp */
bool is_feasible(uint64_t vars, LinearProgram* lp) {
    assert(lp_is_valid(lp));

    int i, j;
    for (i = 0; i < lp->rows; i++) {
        num_t sum = 0;
        for (j = 0; j < lp->cols; j++) {
            if (__get_nth_bit(vars, j)) {
                sum += lp->matrix[i][j];
            }
        }

        if (!__is_feasible_sum(sum, i, lp)) {
            return false;
        }
    }
    return true;
}

void __print_constraint_type(int row, LinearProgram* lp) {
    assert(lp_is_valid(lp));
    assert(row >= 0);
    assert(row < lp->rows);

    if (lp->constraint_types[row] == LEQ) {
        printf("<= ");
    } else if (lp->constraint_types[row] == GEQ) {
        printf(">= ");
    } else if (lp->constraint_types[row] == EQ) {
        printf("= ");
    } else {
        /* should never happen */
        printf("\nassigned unknown constraint type, fatal error\n");
        abort();
    }
}

void print_matrix(LinearProgram* lp) {
    assert(lp_is_valid(lp));

    printf("nvars: %d\n", lp->cols);
    printf("nconss: %d\n", lp->rows);

    int i, j;
    for (i = 0; i < lp->rows; i++) {
        for (j = 0; j < lp->cols; j++) {
            print_num(lp->matrix[i][j]);
        }

        __print_constraint_type(i, lp);

        print_num(lp->vector[i]);
        printf("\n");
    }
}

/* print all 0-1 solutions to the lp into the outstream */
void print_bin_solutions_lp(LinearProgram* lp) {
    uint64_t vars = 0;
    uint64_t count = 1UL << lp->cols;
    uint64_t feasible_solutions = 0;

    print_matrix(lp);
    printf("\n");

    clock_t start = clock();

    uint64_t i;
    for (i = 0; i < count; i++) {
        vars = next_vars(i);
        if (is_feasible(vars, lp)) {
            __print_vars(vars, lp->cols);
            feasible_solutions++;
        }
    }

    double elapsed = GET_SEC(start, clock());
    printf("Checked %lu vectors in %.3f s = %.3f kvecs/s\n",
            count, elapsed, (double) count / elapsed / 1000.0);

    printf("found %lu feasible solutions\n", feasible_solutions);
}
