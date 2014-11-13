//
//  instance.h
//  ex4
//
//  Created by Leon Eifler on 09/11/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#ifndef __ex4__instance__
#define __ex4__instance__
#include <stdio.h>
#define type double

/* constitutes the structure for the constraints of a LP */
typedef struct{
    int rows;
    int columns;
    int * vector;
    int * matrix;
}problem;

problem create_problem(char*);
problem init_problem(int, int);
void free_problem(problem);
int is_Solution(problem , int* );
void set_elem(problem, int, int, int);
int* get_elem(problem, int, int);

#endif /* defined(__ex4__instance__) */
