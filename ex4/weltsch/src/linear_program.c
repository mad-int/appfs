#include <assert.h>
#include <ctype.h>   // isspace
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocate.h"
#include "linear_program.h"

#define MAX_LINE_LEN   512  // Maximum input line length

struct linear_program {
    int rows;
    int cols;
    int** matrix;
    int* vector;
};

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

char* skip_spaces(char* s) {
    while(isspace(*s)) {
        s++;
    }
    return s;
}

char* skip_number(char* s) {
    // a number can start with -
    if (*s == '-') {
        s++;
    }
    while(isdigit(*s)) {
        s++;
    }
    return s;
}

bool parse_row(char* s, int row, LinearProgram* lp) {
    int i;
    char* end_ptr;
    for (i = 0; i < lp->cols; i++) {
        long num = strtol(s, &end_ptr, 10);
        s = end_ptr;
        
        // check that the long num fits into int
        if (num >= INT_MAX || num <= INT_MIN) {
            return -1;
            fprintf(stderr, "number %ld is to big for an int\n", num);
        }

        lp->matrix[row][i] = (int) num;
    }

    s = skip_spaces(s);
    if (*s != '<' || *(s+1) != '=') {
        return false;
    }
    s+=2;


    long num = strtol(s, &end_ptr, 10);
    s = end_ptr;
    
    // check that the long num fits into int
    if (num >= INT_MAX || num <= INT_MIN) {
        return -1;
        fprintf(stderr, "number %ld is to big for an int\n", num);
    }

    lp->vector[row] = num;
    return true;
}

void lp_free(LinearProgram* lp) {
    assert(lp_is_valid(lp));
    int i;
    for (i = 0; i < lp->cols; i++) {
        deallocate(lp->matrix[i]);
    }

    deallocate(lp->vector);
    deallocate(lp);
}

// taken from ex4_readline.c
// adapted to special needs
LinearProgram *new_lp_from_file(const char* filename) {
    assert(NULL != filename);
    assert(0 < strlen(filename));

    bool rows_read = false;
    int rows = 0;
    bool cols_read = false;
    int cols = 0;
    LinearProgram* lp = NULL;
    int real_lines = 0;

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
        while(isspace(*s)) {
            s++;
        }
 
        /* Skip over empty lines
         */
        if (!*s) { /* <=> (*s == '\0') */
            continue;
        }

        /* line is nonempty, so try to parse data
         */
        if (!rows_read) {
            /* FIXME don't use atoi */
            rows = atoi(s);
            rows_read = true;
        } else if (!cols_read) {
            cols = atoi(s);
            cols_read = true;
            lp = lp_new(rows, cols);
        } else {
            bool valid_format = parse_row(s, real_lines, lp);
            if (!valid_format) {
                fprintf(stderr, "line %d does not match the required format\n", lines);
                break;
            }
            real_lines++;
        }
 
    }
    fclose(fp);
 
    /* FIXME lines != rows for lp */
    if (real_lines != rows) {
        fprintf(stderr, "speciefied #(rows) does not match: %d expected, %d found\n", rows, real_lines);
        return NULL;
    }

    return lp;
}

void print_bin_solutions_lp(LinearProgram* lp) {
    assert(lp_is_valid(lp));
    fprint_bin_solutions_lp(stdout, lp);
    assert(lp_is_valid(lp));
}

void __fprint_config(FILE* stream, int* configuration, int len) {
    assert(0 < len);
    int j;
    for (j = 0; j < len; j++) {
        fprintf(stream, "%d\t", configuration[j]);
    }
    fprintf(stream, "\n");
}

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
