#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "allocate.h"
#include "linear_program.h"

struct linear_program {
    int rows;
    int cols;
    int **matrix;
    int *vector;
};

bool lp_is_valid(LinearProgram *lp) {
    return lp->cols &&
        lp->rows &&
        lp->matrix &&
        lp->vector;
}

LinearProgram *lp_new(int rows, int cols) {
    LinearProgram *lp = allocate(1, sizeof(*lp));
    lp->rows = rows;
    lp->cols = cols;

    int** matrix = allocate(cols, sizeof(*matrix));

    int i;
    // initialize every row with 0s
    for (i = 0; i < cols; i++) {
        matrix[i] = allocate(rows, sizeof(*matrix[i]));
    }

    int* vector = allocate(rows, sizeof(*vector));

    lp->matrix = matrix;
    lp->vector = vector;

    assert(lp_is_valid(lp));
    return lp;
}

void lp_free(LinearProgram* lp) {
    int i;
    for (i = 0; i < lp->cols; i++) {
        deallocate(lp->matrix[i]);
    }

    deallocate(lp->vector);
    deallocate(lp);
}

bool parse_lp(const char *filename, LinearProgram* lp) {
    return 0;
}

LinearProgram *new_lp_from_file(const char* filename) {
    assert(NULL != filename);
    assert(0 < strlen(filename));

    // read first 2 lines -> rows & cols
    int rows, cols;
    LinearProgram *lp = lp_new(rows, cols);
    parse_lp(filename, lp);

    assert(lp_is_valid(lp));
    return lp;
}

void print_bin_solutions_lp(LinearProgram* lp) {
    fprint_bin_solutions_lp(stdout, lp);
}

void __fprint_config(FILE* stream, int *configuration, int len) {
    int j;
    for (j = 0; j < len; j++) {
        fprintf(stream, "%d", configuration[j]);
    }
    fprintf(stream, "\n");
}

void next_configuration(int *configuration, int len) {
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

bool is_feasible(int *configuration, LinearProgram *lp) {
    int i, j;
    for (i = 0; i < lp->rows; i++) {
        int sum = 0;
        for (j = 0; j < lp->cols; j++) {
            sum += configuration[j] * lp->matrix[i][j];
        }
        if (lp->vector[i] > sum) {
            return false;
        }
    }
    return true;
}

void fprint_bin_solutions_lp(FILE* stream, LinearProgram* lp) {
    int* configuration = allocate(lp->cols, sizeof(*configuration));
    unsigned long solutions = 1UL << lp->cols;

    int i;
    for (i = 0; i < solutions; i++) {
        if (is_feasible(configuration, lp)) {
            __fprint_config(stream, configuration, lp->cols);
        }
        next_configuration(configuration, lp->cols);
    }
}
