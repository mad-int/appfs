#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert

#include "allocate.h"
#include "readfile.h"
#include "bp.h"
#include "enumeration.h"

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

    /* read model */
    process_file( argv[1], prob );

    /* simple preprocessing: remove redundant constraints */
    preprocessing( prob );

    /* generate Gray Code and print feas sols */
    genGrayCode( prob );

    /* free memory */
    free_problem( prob );

    return EXIT_SUCCESS;
}
