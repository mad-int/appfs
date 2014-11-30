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

char* parse_type(char* s, int row, LinearProgram* lp) {
    s = skip_spaces(s);
    if (!*s) {
        return NULL;
    }
    if (*s == '<') {
        if ('=' != *(s+1)) {
            return NULL;
        }
        lp->constraint_types[row] = LEQ;
        s+=2;
        return s;
    }

    if (*s == '>') {
        if ('=' != *(s+1)) {
            return NULL;
        }
        lp->constraint_types[row] = GEQ;
        s+=2;
        return s;
    }

    if (*s == '=') {
        if ('=' == *(s+1)) {
            s++;
        }
        lp->constraint_types[row] = EQ;
        s++;
        return s;
    }

    fprintf(stderr, "invalid constraint\n");
    return NULL;
}

/* parses a line of the file
 * tries to set the corresponding row in the matrix
 * returns false on error
 */
bool parse_row(char* s, int row, LinearProgram* lp) {
    assert(lp_is_valid(lp));
    assert(row >= 0);
    assert(row < lp->rows);

    int i;
    char* end_ptr;
    for (i = 0; i < lp->cols; i++) {
        num_t num = parse_num(s, &end_ptr);

        if (!is_num_valid(num, s, end_ptr)) {
            return false;
        }

        lp->matrix[row][i] = num;
        s = end_ptr;
    }


    s = parse_type(s, row, lp);

    if (NULL == s) {
        return false;
    }

    num_t num = parse_num(s, &end_ptr);
    if (!is_num_valid(num, s, end_ptr)) {
        return false;
    }
    s = end_ptr;

    s = skip_spaces(s);

    if ('\0' != *s) {
        return false;
    }

    lp->vector[row] = num;

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
        if (parser_state == READ_COLS) {
            cols = (int) strtol(s, &end_ptr, 10);

            if (cols <= 0) {
                fprintf(stderr, "please specify a positive number of cols.\n");
                goto read_error;
            }

            parser_state = READ_ROWS;
        } else if (parser_state == READ_ROWS) {
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
                lp_free(lp);
                fprintf(stderr, "too many constraints");
                goto read_error;
            }

            bool valid_format = parse_row(s, constraints, lp);

            if (!valid_format) {
                lp_free(lp);
                goto read_error;
            }
            constraints++;
        }

    }
    fclose(fp);

    if (constraints != rows) {
        fprintf(stderr, "speciefied #(rows) does not match: %d expected, %d found\n", rows, constraints);
        return NULL;
    }

    if (can_overflow(lp)) {
        return NULL;
    }

    printf("%d lines\n", lines);
    return lp;

read_error:
    if (lp) {
        lp_free(lp);
    }

    printf("error in line %d\n", lines);
    fclose(fp);
    return NULL;
}
