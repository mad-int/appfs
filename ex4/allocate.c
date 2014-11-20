//
//  allocate.c
//  ex4
//
//  Created by Leon Eifler on 10/11/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#include "allocate.h"
#include <stdio.h>
 #include <stdlib.h>
 #include <assert.h>
 #include "allocate.h"

 #ifdef USE_MSHELL
 #include "mshell.h"
 #endif

 void* allocate(int elems, int size)
 {
        void* p;
    
        assert(elems > 0);
        assert(size  > 0);
    
        if (NULL == (p = calloc((size_t)elems, (size_t)size)))
        assert(p != NULL);
    
        return p;
     }

 void deallocate(void* p)
 {
        assert(p != NULL);
    
        free(p);
     }
