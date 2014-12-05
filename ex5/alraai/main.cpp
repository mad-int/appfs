/*
 * main.cpp
 *
 *  Created on: 16.11.2014
 *      Author: Akram
 */


#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include "readin.h"

#define MAX_LINE_LEN 512

using namespace std;

int main(int argc, char** argv){
    if (argc < 2 || strlen(argv[1]) <= 0)
    {
        fprintf(stderr, "usage: %s filename TYPE", argv[0]);
        return EXIT_FAILURE;
    }
    binaryprog bipo;
    if(process_file(argv[1], bipo) < 0){
        return EXIT_FAILURE;
    }
    if(!bipo.isLegal()){
    	fprintf(stderr, "Binary Program is not valid, please check the file.");
    	return EXIT_FAILURE;
    }

    bipo.solveProg();
    return EXIT_SUCCESS;
}





