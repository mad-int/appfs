#include <iostream>
#include "reader.h"


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
    	fprintf(stderr, "Binary Program was not correctly initialized, please check the file.");
    	return EXIT_FAILURE;
    }

    char outputFile[32];
    sscanf(argv[1],"%[^.]", outputFile);
    sprintf(outputFile, "%s.s", outputFile);
    FILE *f = fopen(outputFile, "w");
    bp.solve(f);
   	fclose(f);
    return EXIT_SUCCESS;
}

