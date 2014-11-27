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
#include <time.h>
#include <assert.h>

#define MAX_BUF 1024
#define GET_SEC(a, b)  ((b - a) / (double)CLOCKS_PER_SEC) 
#define MAX_COEFF 1e20


bip init_bip(int rows, int columns){
    bip out;
    out.rows=rows;
    out.columns=columns;
    out.ordin= non_def;
    out.vector=allocate(rows, sizeof(*out.vector));
    out.matrix=allocate(rows * columns, sizeof(*out.matrix));
    return out;
  }

static bool is_initialized(bip test){
    if(!test.rows || !test.columns || NULL==test.vector || NULL==test.matrix){
        fprintf(stderr, "Instanz nicht initialisiert. \n");
        return false;
    }
    return true;
}

/* return a pointer to the row,col-element of the Matrix */
type* get_elem(bip inst, int row, int col){
    if (!is_initialized(inst)) {
        return NULL;
    }
    return inst.matrix+row*inst.columns+col;
}

/* change to value of the row,col-element of the matrix */
void set_elem(bip inst, int row, int col, type value){
    if (!is_initialized(inst)) return;
    inst.matrix[row*inst.columns+col]=value;
}

/* deallocate the space of an initialized bip */
void free_bip(bip in){
    if (!is_initialized(in))return;
    deallocate(in.vector);
    deallocate(in.matrix);
}


type scalar_product(type* vector1, int* vector2, int dim){
    type productout=0;
    for(int i=0; i<dim; i++){
        assert (NULL!=vector1 && NULL!=vector2);
        productout+=vector1[i]*(type)vector2[i];
    }
    return productout;
}


/* checks if a vector is a feasible point of the LP */
int is_Solution(bip inst, int* test){
    assert(is_initialized(inst));
    for (int i=0; i<inst.rows; ++i){
        if (scalar_product(get_elem(inst, i, 0),test, inst.columns)>inst.vector[i]) return 0;
    }
    return 1;
}

int is_Solution_eq(bip inst, int* test){
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
        printf("%d  ", solution[i]);
    }
    printf("\n");
}
/* Print the bip as A<=b or A>=b or A=b to the console */
void print_bip(bip p){
    if (!is_initialized(p)) {
        return;
    }
    printf("The input BIP is: \n");
    char * ord;
    switch (p.ordin) {
        case non_def:
            ord="<=";
            printf("Warning: No ordinance sign put in. By default use <=. \n");
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
long bip_enumerate(const bip bip, bool with_output)
{
        assert(is_initialized(bip));
        clock_t start = clock();
        long long     solus = 0;
        long long     count = 0;
    if (with_output)printf("The solution vectors are: \n");
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
    
        printf(" %lld vectors were tested in %.3f s = %.3f kvecs/s\n",
                    count, elapsed, count / elapsed / 1000.0);
    
        return solus;
     }
int cmpfunc (const void ** a, const void ** b)
{
    double** first = (double* *)a;
    double** second = (double* *)b;
    if (0.0<**first- **second) {
        return -1;
    }
    return 1;
}

void pre_order(bip inst, bool with_output){
    bip temp=init_bip(inst.rows, inst.columns);
    for (int i=0; i<inst.rows; ++i) {
        for (int j=0; j<inst.columns; ++j) {
            set_elem(temp, i, j, *get_elem(inst, i, j));
        }
        temp.vector[i]=inst.vector[i];
    }
    double order[inst.rows];
    for (int i=0; i<inst.rows; ++i) {
        for (int j=0; j<inst.columns; ++j) {
            order[i]+=(double)*get_elem(inst, i, j);
        }
        if(0<inst.vector[i])order[i]=order[i]/ (double)(inst.vector[i]);
        if (0>inst.vector[i])order[i]=order[i]*(double)-inst.vector[i];
        //printf("%f bei %d \n", order[i], i);
    }
    double* pointers[inst.rows];
    for (int i=0; i<inst.rows; ++i) {
        pointers[i]=&order[i];
    }
    qsort(pointers, inst.rows, sizeof(pointers[0]), cmpfunc);
    int orderind[inst.rows];
    for (int i=0; i<inst.rows; i++) {
        orderind[i]=pointers[i]-order;
    }
    for (int i=0; i<inst.rows; ++i) {
        for (int j=0; j<inst.columns; ++j) {
            set_elem(inst, i, j, *get_elem(temp, orderind[i], j));
        }
        inst.vector[i]=temp.vector[orderind[i]];
    }
    free_bip(temp);
    if(with_output)print_bip(inst);
}

bool check_overflow(bip inst){
    
#ifdef DOUBLE
    return true;
#else
    long long check=0;
    int check2=0;
    for (int i=0; i<inst.rows; ++i) {
        for (int j=0; j<inst.columns; ++j) {
            check+=abs(*get_elem(inst, i, j));
            check2+=abs(*get_elem(inst, i, j));
            if( check!=check2)return false;
        }
        if (abs(inst.vector[i])>MAX_COEFF) {
            return false;
        }
    }
    return true;
#endif
}
//test all possible solutions on the bip
long long find_solutions(bip inst, bool with_print){
    assert(is_initialized(inst));
    
    //check for overflow
    if(!check_overflow(inst)){
        fprintf(stderr, "Coefficients too large. Overflow! \n");
        return -1;
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
    pre_order(inst, with_print);
    long long sols=bip_enumerate(inst, with_print);
    return sols;
}
