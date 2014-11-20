//
//  instance.c
//  ex4
//
//  Created by Leon Eifler on 09/11/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#include "instance.h"
#include "allocate.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "permute.h"

#define MAX_BUF 1024


problem init_problem(int rows, int columns){
    problem out;
    out.rows=rows;
    out.columns=columns;
    out.vector=allocate(rows, sizeof(int));
    out.matrix=allocate(rows * columns, sizeof(int));
    return out;
  }
static bool is_initialized(problem test){
    if(0==test.rows && 0==test.columns){
        printf("Instanz nicht initialisiert. \n");
        return false;
    }
    return true;
}

/* return a pointer to the row,col-element of the Matrix */
int* get_elem(problem inst, int row, int col){
    if (!is_initialized(inst)) {
        return NULL;
    }
    return inst.matrix+row*inst.columns+col;
}

/* change to value of the row,col-element of the matrix */
void set_elem(problem inst, int row, int col, int value){
    if (!is_initialized(inst)) return;
    inst.matrix[row*inst.columns+col]=value;
}

/* deallocate the space of an initialized problem */
void free_problem(problem in){
    if (!is_initialized(in))return;
    deallocate(in.vector);
    deallocate(in.matrix);
}


int scalar_product(int* vector1, int* vector2, int dim){
    int productout=0;
    for(int i=0; i<dim; i++){
        if (NULL==vector1 || NULL==vector2) {
            printf("Element nicht initialisiert."); return 0;
        }
        productout+=vector1[i]*vector2[i];
    }
    return productout;
}


/* checks if a vector is a feasible point of the LP */
int is_Solution(problem inst, int* test){
    if (!is_initialized(inst)) {
        return 0;
    }
    for (int i=0; i<inst.rows; ++i){
        if (scalar_product(get_elem(inst, i, 0),test, inst.columns)>inst.vector[i]) return 0;
    }
    return 1;
}
//prints out a vector, followd by a new line
void print_solution(int* solution, int length){
    for (int i=0; i<length; ++i) {
        printf("%d \t", solution[i]);
    }
    printf("\n");
}

//test all possible solutions on the problem
void find_solutions(problem inst){
    if (!is_initialized(inst)) {
        return ;
    }
    for (int i=0; i<inst.columns; ++i) {
        int test[inst.columns];
        for (int j=0  ; j< inst.columns; ++j) {
            if (j<inst.columns-i) {
                test[j]=0;
            }else test[j]=1;
        }
        if (is_Solution(inst, test)) {
            print_solution(test,inst.columns);
        }
        while (next_permutation(test, inst.columns)) {
            if (is_Solution(inst, test)) {
                print_solution(test,inst.columns);
            }
        }
   }
}

