#include <stdio.h>
#include <stdlib.h>
// local
#include "constraintArray.h"
#include "readConstArray.h"

int main(int argc, char** argv) {
    ConstArray* c;
    if (argc < 2) {
        printf("usage: %s filename\n", argv[0]);
        printf("Reading dummy data instead!\n");
        createDummyConstArray(&c);
        printConstArray(c);
    } else {
        int r;
        switch(r=read_constArray_from_file(argv[1], &c)){
        case -1:
            return EXIT_FAILURE;
        case 1:
            return EXIT_SUCCESS;
        case 0:
            printf("Reading successful! Starting enumeration now.\n");
            break;
        default:
            fprintf(stderr, "Fatal error: Unknown return value of reading function. Should not happen! Return value was: %i\n",r);
            return EXIT_FAILURE;
        }
    }
    if (preprocessConstArray(c)){
        freeConstArray(c);
        return EXIT_SUCCESS;
    }
    printAllFeasibleBinaryAssignments(c);
    freeConstArray(c);

    return EXIT_SUCCESS;
}
