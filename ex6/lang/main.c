/* 
 * File:   main.c
 * Author: clauslang
 *
 * Created on November 13, 2014, 3:47 PM
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include "calculations.h"
#include "structure.h"
#include "allocate.h"
#include "checks.h"
#include "logic.h"

int main(int argc, char** argv) {
    
    printf("%s\n", "program started");
    
    clock_t begin, end;
    double time_spent;
    begin = clock();
    
    if(D) printf("using double\n");
    else printf("using int\n");
    
    problem* instance = process_file(argv[1]);
    print_solutions(instance);
    
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("\ncomputing time: %f seconds\n", time_spent);
    
    deallocate(instance);
    
    return 0;
}