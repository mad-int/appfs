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
#include "logic_int.h"
#include "logic_double.h"

int main(int argc, char** argv) {
    
    clock_t begin, end;
    double time_spent;
    begin = clock();

    problem* instance = NULL;
    
    if (D) {
        instance = process_file_double(argv[1]);
        print_solutions_double(instance);
    } else {
        instance = process_file_int(argv[1]);
        print_solutions_int(instance);
    }
    
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("\ncomputing time: %f seconds\n", time_spent);
    
    deallocate(instance);
    
    return 0;
}