#include <iostream>
#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include <math.h>    // pow
#include <reader.h>


#define MAX_LINE_LEN   512  // Maximum input line length



using namespace std;

int main(int argc, char** argv){

    if (argc < 2 || strlen(argv[1]) <= 0)
    {
        fprintf(stderr, "usage: %s filename TYPE", argv[0]);
        return EXIT_FAILURE;
    }
    binPrg bp;
    if(process_file(argv[1], bp) < 0){
        return EXIT_FAILURE;
    }
    if(!bp.isValid()){
    	fprintf(stderr, "Binary Program is not valid, please check the file.");
    	return EXIT_FAILURE;
    }

    bp.solve();
    return EXIT_SUCCESS;
}

