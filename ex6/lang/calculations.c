#include <stdio.h>
#include <stdlib.h>
#include "calculations.h"

NUM scalar_product(int* x, NUM* y, int size) {
    NUM product = 0;
    for (int i = 0; i < size; i++) {
        if (x[i] == 1)
            product += y[i];
    }
    return product;
}

int increment(int* x, int size) {
    
    int* p = x + size - 1;
    
    for (int i = 0; i < size; i++) {
        if (*p == 1) {
            *p = 0; 
            p--;
            continue;
        }
        if (*p == 0) {
            *p = 1;
            break;
        }
        printf("Something is wrong with the potential solution vectors!");
        abort();
    }
    
    return 0;
}
