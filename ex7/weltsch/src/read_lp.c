#include <assert.h>
#include <ctype.h>   // isspace
#include <stdio.h>
#include <string.h>

#include "linear_program.h"
#include "num_type.h"
#include "read_lp.h"

#define MAX_LINE_LEN   512  // Maximum input line length
/* the file parser can have 3 different states, reading #rows, #cols, or
 * parsing a constraint
 */
enum parser_state {READ_ROWS, READ_COLS, READ_CONSTRAINTS};

char* skip_spaces(char* s) {
    while(isspace(*s)) {
        s++;
    }

    return s;
}

bool parse_coef(char **row_ptr, int row, int col, LinearProgram* lp) {
    if (col >= get_cols(lp)) {
        return false;
    }

    char* end = NULL;
    num_t num = parse_num(*row_ptr, &end);

    if (!is_num_valid(num, *row_ptr, end)) {
        return false;
    }

    set_coef(lp, row, col, num);
    *row_ptr = end;
    return true;
}

bool parse_type(char** row_ptr, int row, LinearProgram* lp) {
    *row_ptr = skip_spaces(*row_ptr);
    if (!**row_ptr) {
        return false;
    }

    if (**row_ptr == '<') {
        if ('=' != *(*row_ptr+1)) {
            return false;
        }
        set_constraint_type(lp, row, LEQ);
        (*row_ptr)+=2;
        return true;
    }

    if (**row_ptr == '>') {
        if ('=' != *(*row_ptr+1)) {
            return false;
        }
        set_constraint_type(lp, row, GEQ);
        (*row_ptr)+=2;
        return true;
    }

    if (**row_ptr == '=') {
        if ('=' == *(*row_ptr+1)) {
            (*row_ptr)++;
        }
        set_constraint_type(lp, row, EQ);
        (*row_ptr)++;
        return true;
    }

    fprintf(stderr, "invalid constraint\n");
    return false;
}

bool parse_rhs(char** row_ptr, int row, LinearProgram* lp) {
    char* end = NULL;
    num_t num = parse_num(*row_ptr, &end);

    if (!is_num_valid(num, *row_ptr, end)) {
        return false;
    }

    set_rhs(lp, row, num);
    *row_ptr = end;
    return true;
}

/* parses a line of the file
 * tries to set the corresponding row in the matrix
 * returns false on error
 */
bool validate_and_parse_row(char* s, int row, LinearProgram* lp) {
    assert(lp_is_valid(lp));
    assert(row >= 0);
    assert(row < get_rows(lp));

    int vars = get_cols(lp);
    int coefs = 0;

    /* read coefficients */
    while (parse_coef(&s, row, coefs, lp)) {
        coefs++;
    }

    /* not at end of string */
    if (!*s) {
        return false;
    }

    if (coefs != vars) {
        fprintf(stderr, "#cols does not match #vars, expected: %d, found %d",vars, coefs);
        return false;
    }

    if (!parse_type(&s, row, lp)) {
        fprintf(stderr, "invalid type + %s", s);
        return false;
    }

    /* still not at end of string */
    if (!*s) {
        fprintf(stderr, "missing rhs + %s", s);
        return false;
    }

    if (!parse_rhs(&s, row, lp)) {
        fprintf(stderr, "invalid rhs + %s", s);
        return false;
    }

    s = skip_spaces(s);
    /* this time make sure we really ARE at end of string */
    if (*s) {
        fprintf(stderr, "trailing garbage + %s", s);
        return false;
    }

    assert(lp_is_valid(lp));
    return true;
}

LinearProgram *new_lp_from_file(const char* filename) {
    assert(NULL != filename);
    assert(0 < strlen(filename));

    int rows = 0;
    int cols;
    char* end_ptr = NULL;
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
        if (READ_COLS == parser_state) {
            cols = (int) strtol(s, &end_ptr, 10);

            if (cols <= 0) {
                fprintf(stderr, "please specify a positive number of cols.\n");
                goto read_error;
            }

            parser_state = READ_ROWS;
        } else if (READ_ROWS == parser_state) {
            rows = (int) strtol(s, &end_ptr, 10);

            if (rows <= 0) {
                fprintf(stderr, "please specify a positive number of rows.\n");
                goto read_error;
            }

            lp = lp_new(rows, cols);
            parser_state = READ_CONSTRAINTS;
        } else {
            /* stop if a row does not match the format */
            if (constraints >= rows) {
                fprintf(stderr, "too many constraints\n");
                goto read_error;
            }

            if (!validate_and_parse_row(s, constraints, lp)) {
                fprintf(stderr, "row format is invalid\n");
                goto read_error;
            }

            constraints++;
        }

    }
    fclose(fp);

    if (constraints != rows) {
        fprintf(stderr, "speciefied #(rows) does not match: %d expected, %d found\n", rows, constraints);
        goto read_error;
    }

    if (can_overflow(lp)) {
        fprintf(stderr, "the lp can overflow when using the current datatype for the coefficients\n");
        goto read_error;
    }

    printf("%d lines\n", lines);

    assert(lp_is_valid(lp));
    return lp;

read_error:
    if (NULL != lp) {
        lp_free(lp);
    }

    fprintf(stderr, "error in line %d\n", lines);
    fclose(fp);
    return NULL;
}
