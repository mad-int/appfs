//
//  permute.c
//  ex4
//
//  Created by Leon Eifler on 10/11/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#include "permute.h"
#include <stdbool.h>

void swap(int *v, const int i, const int j)
{
    int t;
    t = v[i];
    v[i] = v[j];
    v[j] = t;
}

/* get the next permutation of an array. Here, the array is regarded as an integer and the next-smallest permutation of Digits is searched. The input-array gets changed to it's next permutation and the return falue is true unless the array is at it's largest permuation.  */
bool next_permutation(int *array,  int length)
{
    int i= length-1;
    while (i>0 && array[i-1]>=array[i]) {
        --i;
    }
    if (i<=0) {
        return false;
    }
    int j=length-1;
    while (array[j]<=array[i-1]) {
        j--;
    }
    swap(array, i-1, j);
    j=length-1;
    while (i<j) {
        swap(array, i, j);
        ++i;--j;
    }
    return true;
}