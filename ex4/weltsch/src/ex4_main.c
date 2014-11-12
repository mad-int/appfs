#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>
#include <assert.h>
#include "linear_program.h"

/** Read textfile, textfile should describe an lp in (FIXME format)
 * the specified format. Prints the integer 0-1 solutions to stdout
 * @param filename name of file to read
 * @return EXIT_FAILURE if file in wrong format
 *         EXIT_SUCCESS if solved
 */
int solve_lp(const char *filename, const char* outfile) {
    int rows, cols;
    LinearProgram *lp = new_lp_from_file(filename);

    if (NULL == lp) { /* NULL if not enough memory or wrong format */
        return EXIT_FAILURE;
    }

    if (strlen(outfile) > 0) {
        FILE *stream = fopen(outfile, "w");

        if (NULL == stream) {
            fprintf(stderr, "can't write to outfile %s\n", outfile);
            return EXIT_FAILURE;
        }

        fprint_bin_solutions_lp(stream, lp);
        fclose(stream);
    } else {
        print_bin_solutions_lp(lp);
    }

    lp_free(lp);

    return EXIT_SUCCESS;
}

int main(int argc, char** argv) {
    if (argc < 2 || strlen(argv[1]) <= 0)
    {
       fprintf(stderr, "usage: %s filename\n", argv[0]);
       fprintf(stderr, "alternative usage: %s filename outfile\n", argv[0]);
       return EXIT_FAILURE;
    }

    if (argc >= 3 && strlen(argv[2]) > 0) {
        return solve_lp(argv[1], argv[2]);
    }

    /* FIXME maybe add a wrapper for this, even though it's not
     * necessary */
    return solve_lp(argv[1], "");
}
