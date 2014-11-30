#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "allocate.h"
#include "linear_program.h"
#include "num_type.h"

#define GET_SEC(a, b) ((double) (b - a) / (double)CLOCKS_PER_SEC)

struct linear_program {
    int rows;
    int cols;
    num_t** matrix;
    num_t* vector;
    int* constraint_types;
};

/* FIXME too simple, are there any other properties to check? */
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

/* print a solution vector */
void __print_config(num_t* configuration, int len) {
    assert(0 < len);

    int j;
    for (j = 0; j < len; j++) {
        print_num(configuration[j]);
    }

    printf("\n");
}

/* return the lexicographically next 0-1 vector */
void next_configuration(num_t* configuration, int len) {
    assert(0 < len);

    int i;
    for (i = 0; i < len; i++) {
        if (configuration[i]) {
            configuration[i] = 0;
        } else {
            configuration[i] = 1;
            break;
        }
    }
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
bool is_feasible(num_t* configuration, LinearProgram* lp) {
    assert(lp_is_valid(lp));

    int i, j;
    for (i = 0; i < lp->rows; i++) {
        num_t sum = 0;
        for (j = 0; j < lp->cols; j++) {
            sum += configuration[j] * lp->matrix[i][j];
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
    num_t* configuration = allocate(lp->cols, sizeof(*configuration));
    unsigned long count = 1UL << lp->cols;
    unsigned int feasible_solutions = 0;

    print_matrix(lp);
    printf("\n");

    clock_t start = clock();

    unsigned int i;
    for (i = 0; i < count; i++) {
        if (is_feasible(configuration, lp)) {
            __print_config(configuration, lp->cols);
            feasible_solutions++;
        }
        next_configuration(configuration, lp->cols);
    }

    double elapsed = GET_SEC(start, clock());
    printf("Checked %lu vectors in %.3f s = %.3f kvecs/s\n",
            count, elapsed, (double) count / elapsed / 1000.0);

    deallocate(configuration);
    printf("found %u feasible solutions\n", feasible_solutions);
}
