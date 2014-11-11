#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include "ex4_readline.c"
#include "linear_program.h"

/** Read textfile, textfile should describe an lp in (FIXME format)
 * the specified format. Prints the integer 0-1 solutions to stdout
 * @param filename name of file to read
 * @return EXIT_FAILURE if file in wrong format
 *         EXIT_SUCCESS if solved
 */
int solve_lp(const char *filename) {
    assert(NULL != filename);
    assert(0 < strlen(filename));

    int rows, cols;
    LinearPorgram *lp = lp_new(rows, cols);
    parse_lp(filename, lp);
    print_bin_solutions_lp(lp);
    lp_free(lp);

    return EXIT_SUCCESS;
}

int main(int argc, char** argv) {
    if (argc < 2 || strlen(argv[1]) <= 0)
    {
       fprintf(stderr, "usage: %s filename", argv[0]);
       return EXIT_FAILURE;
    }
    // FIXME if argc >= 3 -> fprint_bin_solutions_lp

    return solve_lp(argv[1]);
}
