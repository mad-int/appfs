#include <assert.h>
#include <ctype.h>   // isspace
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocate.h"
#include "linear_program.h"

#define MAX_LINE_LEN   512  // Maximum input line length

#ifdef USE_DOUBLE

typedef double num_t;
num_t parse_num(char* num_str, char** end_ptr) {
    return strtod(num_str, end_ptr);
}

bool is_num_valid(num_t num, char* num_str, char* end_str) {
}

#else

typedef int num_t;
num_t parse_num(char* num_str, char** end_ptr) {
    return strtol(num_str, end_ptr, 10);
}

#endif

/* the file parser can have 3 different states, reading #rows, #cols, or
 * parsing a constraint
 */
enum parser_state {READ_ROWS, READ_COLS, READ_CONSTRAINTS};

struct linear_program {
    int rows;
    int cols;
    int** matrix;
    int* vector;
};

/* FIXME too simple, are there any other properties to check? */
bool lp_is_valid(LinearProgram* lp) {
    return lp->cols &&
        lp->rows &&
        lp->matrix &&
        lp->vector;
}

LinearProgram* lp_new(int rows, int cols) {
    LinearProgram* lp = allocate(1, sizeof(*lp));
    lp->rows = rows;
    lp->cols = cols;

    int** matrix = allocate(rows, sizeof(*matrix));

    int i;
    // initialize every row with 0s
    for (i = 0; i < rows; i++) {
        matrix[i] = allocate(cols, sizeof(*matrix[i]));
    }

    int* vector = allocate(rows, sizeof(*vector));

    lp->matrix = matrix;
    lp->vector = vector;

    assert(lp_is_valid(lp));
    return lp;
}

char* skip_spaces(char* s) {
    while(isspace(*s)) {
        s++;
    }

    return s;
}

bool is_num_valid(long num, char* s, char* end_ptr) {
    if (num >= INT_MAX || num <= INT_MIN) {
        fprintf(stderr, "number %ld is to big for an int\n", num);
        return false;
    }

    if (s == end_ptr) {
        fprintf(stderr, "not a valid integer %s\n", s);
        return false;
    }
    return true;
}

/* parses a line of the file
 * tries to set the corresponding row in the matrix
 * returns false on error
 */
bool parse_row(char* s, int row, LinearProgram* lp) {
    assert(row < lp->rows);

    int i;
    char* end_ptr;
    for (i = 0; i < lp->cols; i++) {
        long num = strtol(s, &end_ptr, 10);

        if (!is_num_valid(num, s, end_ptr)) {
            return false;
        }

        lp->matrix[row][i] = (int) num;
        s = end_ptr;
    }

    s = skip_spaces(s);
    if (*s == '\0' || *s != '<' || *(s+1) != '=') {
        return false;
    }
    s+=2;


    long num = strtol(s, &end_ptr, 10);
    if (!is_num_valid(num, s, end_ptr)) {
        return false;
    }
    s = end_ptr;

    lp->vector[row] = num;
    return true;
}

void lp_free(LinearProgram* lp) {
    assert(lp_is_valid(lp));
    int i;
    for (i = 0; i < lp->rows; i++) {
        deallocate(lp->matrix[i]);
    }

    deallocate(lp->matrix);
    deallocate(lp->vector);
    deallocate(lp);
}

// taken from ex4_readline.c
// adapted to special needs
LinearProgram *new_lp_from_file(const char* filename) {
    assert(NULL != filename);
    assert(0 < strlen(filename));

    int rows = 0;
    int cols;
    LinearProgram* lp = NULL;

    /* counts the constraint that were read from file
     * if constraints < rows -> error
     * */
    int constraints = 0;

    /* used to mark distinguish the current state of the parser */
    int parser_state = READ_COLS;

    FILE* fp;
    char  buf[MAX_LINE_LEN];
    char* s;
    int   lines = 0;

    if (NULL == (fp = fopen(filename, "r")))
    {
        fprintf(stderr, "Can't open file %s\n", filename);
        return NULL;
    }

    while(NULL != (s = fgets(buf, sizeof(buf), fp)))
    {
        char* t = strpbrk(s, "#\n\r");

        lines++;

        if (NULL != t) /* else line is not terminated or too long */
            *t = '\0';  /* clip comment or newline */

        /* Skip over leading space
        */
        s = skip_spaces(s);

        /* Skip over empty lines
         */
        if (!*s) { /* <=> (*s == '\0') */
            continue;
        }

        /* line is nonempty, so try to parse data
         */
        if (parser_state == READ_COLS) {
            cols = atoi(s);
            parser_state = READ_ROWS;
        } else if (parser_state == READ_ROWS) {
            /* FIXME don't use atoi */
            rows = atoi(s);
            lp = lp_new(rows, cols);
            parser_state = READ_CONSTRAINTS;
        } else {
            /* stop if a row does not match the format */
            bool valid_format = parse_row(s, constraints, lp);
            if (!valid_format) {
                fprintf(stderr, "line %d does not match the required format\n", lines);
                break;
            }
            constraints++;
        }

    }
    fclose(fp);

    if (constraints != rows) {
        fprintf(stderr, "speciefied #(rows) does not match: %d expected, %d found\n", rows, constraints);
        return NULL;
    }

    printf("%d lines\n", lines);
    return lp;
}

/* print a solution vector */
void __print_config(int* configuration, int len) {
    assert(0 < len);
    int j;
    for (j = 0; j < len; j++) {
        printf("%d ", configuration[j]);
    }
    printf("\n");
}

/* return the lexicographically next 0-1 vector */
void next_configuration(int* configuration, int len) {
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

/* check if a vector is a feasible solution to the lp */
bool is_feasible(int* configuration, LinearProgram* lp) {
    int i, j;
    for (i = 0; i < lp->rows; i++) {
        int sum = 0;
        for (j = 0; j < lp->cols; j++) {
            sum += configuration[j] * lp->matrix[i][j];
        }

        if (lp->vector[i] < sum) {
            return false;
        }
    }
    return true;
}

void print_matrix(LinearProgram* lp) {
    printf("nvars: %d\n", lp->cols);
    printf("nconss: %d\n", lp->rows);
    int i, j;
    for (i = 0; i < lp->rows; i++) {
        for (j = 0; j < lp->cols; j++) {
            printf("%d ", lp->matrix[i][j]);
        }
        printf("<= ");
        printf("%d\n", lp->vector[i]);
    }
}

/* print all 0-1 solutions to the lp into the outstream */
void print_bin_solutions_lp(LinearProgram* lp) {
    int* configuration = allocate(lp->cols, sizeof(*configuration));
    long solutions = 1UL << lp->cols;
    int feasible_solutions = 0;

    print_matrix(lp);
    printf("\n");

    int i;
    for (i = 0; i < solutions; i++) {
        if (is_feasible(configuration, lp)) {
            __print_config(configuration, lp->cols);
            feasible_solutions++;
        }
        next_configuration(configuration, lp->cols);
    }
    deallocate(configuration);
    printf("found %d feasible solutions\n", feasible_solutions);
}
