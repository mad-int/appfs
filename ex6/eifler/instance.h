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
}bip;

bip create_bip(char*);
bip init_bip(int, int);
void free_bip(bip);
int is_Solution(bip , int* );
void set_elem(bip, int, int, type);
type* get_elem(bip, int, int);
void print_bip(bip);
long long find_solutions(bip, bool);

#endif /* defined(__ex4__instance__) */
