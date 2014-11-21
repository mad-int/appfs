#include "calculations.h"

int scalar_product(int* x, int* y, int size) {
    int product = 0;
    for (int i = 0; i < size; i++) {
        product += x[i]*y[i];
    }
    return product;
}

double scalar_product_double(double* x, double* y, int size) {
    double product = 0;
    for (int i = 0; i < size; i++) {
        product += x[i]*y[i];
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
    }
    
    return 0;
}
