//
//  main.c
//  ex1
//
//  Created by Leon Eifler on 24/10/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#include <stdio.h>
#include <limits.h>   // INT_MAX
#define BUF_NUMS 4096

int main(int argc, const char * argv[]) {
    // insert code here...
    
    static unsigned int bucket[INT32_MAX];
    int  buffer[BUF_NUMS];
    FILE * stream;
    
    if(NULL==argv[1]){
        printf("inputerrpr");
        return 2;
    }
    if(NULL==(stream = fopen(argv[1], "r" ))){
        printf("streamerr");
        return 2;
    }
    
    while (BUF_NUMS==fread(buffer, sizeof(buffer[1]),BUF_NUMS,stream)) {
        for (int i=0; i<BUF_NUMS; ++i) {
            if (buffer[i]>0) {
               bucket[buffer[i]]=1;
            }
        }
    }
    for (int i=0; i<BUF_NUMS; ++i) {
        if (buffer[i]>0) {
            bucket[buffer[i]]=1;
        }
    }
    
    for (int i=0; i<INT32_MAX; ++i) {
        if (bucket[i]>0) {
            printf("%d \n", i);
        }
    }
    

    
 
    return 0;
}
