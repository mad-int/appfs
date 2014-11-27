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
#include <stdbool.h>
#ifdef DOUBLE
#define type double
#else 
#define type int
#endif

enum ordination_sign {non_def, greatereq, smallereq, eq};

/* constitutes the structure for the constraints of a LP */
typedef struct{
    int rows;
    int columns;
    enum ordination_sign ordin;
    type * vector;
    type * matrix;
}problem;

problem create_problem(char*);
problem init_problem(int, int);
void free_problem(problem);
int is_Solution(problem , int* );
void set_elem(problem, int, int, type);
type* get_elem(problem, int, int);
void print_problem(problem);
long long find_solutions(problem, bool);

#endif /* defined(__ex4__instance__) */
