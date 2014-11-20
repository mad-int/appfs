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
#include <time.h>
#include <assert.h>

#define MAX_BUF 1024
#define GET_SEC(a, b)  ((b - a) / (double)CLOCKS_PER_SEC) 

problem init_problem(int rows, int columns){
    problem out;
    out.rows=rows;
    out.columns=columns;
    out.ordin= non_def;
    out.vector=allocate(rows, sizeof(*out.vector));
    out.matrix=allocate(rows * columns, sizeof(*out.matrix));
    return out;
  }
static bool is_initialized(problem test){
    if(!test.rows || !test.columns || NULL==test.vector || NULL==test.matrix){
        printf("Instanz nicht initialisiert. \n");
        return false;
    }
    return true;
}

/* return a pointer to the row,col-element of the Matrix */
type* get_elem(problem inst, int row, int col){
    if (!is_initialized(inst)) {
        return NULL;
    }
    return inst.matrix+row*inst.columns+col;
}

/* change to value of the row,col-element of the matrix */
void set_elem(problem inst, int row, int col, type value){
    if (!is_initialized(inst)) return;
    inst.matrix[row*inst.columns+col]=value;
}

/* deallocate the space of an initialized problem */
void free_problem(problem in){
    if (!is_initialized(in))return;
    deallocate(in.vector);
    deallocate(in.matrix);
}


type scalar_product(type* vector1, int* vector2, int dim){
    type productout=0;
    for(int i=0; i<dim; i++){
        if (NULL==vector1 || NULL==vector2) {
            printf("Element nicht initialisiert."); return 0;
        }
        productout+=vector1[i]*(type)vector2[i];
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

int is_Solution_eq(problem inst, int* test){
    if (!is_initialized(inst)) {
        return 0;
    }
    for (int i=0; i<inst.rows; ++i){
        if (scalar_product(get_elem(inst, i, 0),test, inst.columns)!=inst.vector[i]) return 0;
    }
    return 1;
}
//prints out a vector, followd by a new line
void print_solution(int* solution, int length){
    for (int i=0; i<length; ++i) {
        printf("%d  \t", solution[i]);
    }
    printf("\n");
}
/* Print the Problem as A<=b or A>=b or A=b to the console */
void print_problem(problem p){
    if (!is_initialized(p)) {
        return;
    }
    printf("Die Eingegebene Problemstellung ist: \n");
    char * ord;
    switch (p.ordin) {
        case non_def:
            ord="<=";
            printf("Warnung: Kein Ordnungszeichen angegeben.");
            break;
        case smallereq:
            ord="<=";
            break;
        case greatereq:
            ord=">=";
            break;
        case eq:
            ord="=";
            break;
    }
#ifdef DOUBLE
    for (int i=0; i<p.rows; ++i) {
        for (int j=0; j<p.columns; ++j) {
            printf("%.3f  ",*get_elem(p,i, j));
        }
        printf("%s  %.3f  \n", ord, p.vector[i]);
    }
#else
    for (int i=0; i<p.rows; ++i) {
        for (int j=0; j<p.columns; ++j) {
            printf("%d  ",*get_elem(p,i, j));
        }
        printf("%s  %d  \n", ord, p.vector[i]);
    }
#endif
    
    
}
int bip_enumerate(const problem bip, bool with_output)
{
        assert(is_initialized(bip));
        clock_t start = clock();
        int     solus = 0;
        int     count = 0;
        for(unsigned long long bitvec = 0; bitvec < (1uL << bip.columns); bitvec++)
           {
              unsigned long long mask = 1;  // long long because long is 32 bit on 32 bit computers
                   int    x[bip.columns];
            
                assert(sizeof(bitvec) * 8 > bip.columns); //lint !e506
                  for(int j = 0; j < bip.columns; j++, mask <<= 1)
                          x[j] = (bitvec & mask) ? 1.0 : 0.0;
               if (bip.ordin==eq) {
                   if (is_Solution_eq(bip, x)){
                       if (with_output)
                           print_solution(x, bip.columns);
                       
                       solus++;
                   }
               }else{
                   if (is_Solution(bip, x)) //lint !e772
                   {
                       if (with_output)
                           print_solution(x, bip.columns);
                       
                       solus++;
                   }
               }
               
                  count++;
            }
       assert((unsigned)count == (1u << bip.columns));
    
        double elapsed = GET_SEC(start, clock());
    
        printf("Checked %d vectors in %.3f s = %.3f kvecs/s\n",
                    count, elapsed, count / elapsed / 1000.0);
    
        return solus;
     }
//test all possible solutions on the problem
int find_solutions(problem inst){
    if (!is_initialized(inst)) {
        return 0;
    }
    if (greatereq==inst.ordin) {
        for (int i=0; i<inst.rows; ++i) {
            for (int j=0; j<inst.columns; ++j) {
                int temp = -(*get_elem(inst, i, j));
                set_elem(inst, i, j, temp);
            }
            inst.vector[i]=-inst.vector[i];
        }
    }
    
    int sols=bip_enumerate(inst, true);
    return sols;
}
