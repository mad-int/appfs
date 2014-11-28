#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <time.h> // clock

#include "allocate.h"
#include "readfile.h"
#include "solutions.h"
#include "bp.h"

int main(int argc, char** argv)
{
    if (argc < 2 || strlen(argv[1]) <= 0)
    {
        fprintf(stderr, "usage: %s filename", argv[0]);
        return EXIT_FAILURE;
    }
    /* create problem pointer */
    BP* prob;
    prob = allocate(1, sizeof(*prob));

    process_file( argv[1], prob );
//     print_problem( prob );
    find_binary_solutions( prob );

    /* free memory */
    free_problem( prob );

    return EXIT_SUCCESS;
}