/* 
 * File:   main.c
 * Author: clauslang
 *
 * Created on November 13, 2014, 3:47 PM
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>

#define MAX_LINE_LEN 512

/*
 * 
 */
int main(int argc, char** argv) {
    
    char cwd[1024];
    getcwd(cwd, sizeof(cwd));
    //process_file(strcat(cwd, "/example2"));
    
    process_file(argv[1]);
    
    return (EXIT_SUCCESS);
}

int process_file (const char* filename)
{   
    assert(NULL != filename);
    assert(0 < strlen(filename));
    
    FILE* fp;
    char buf[MAX_LINE_LEN];
    char* s;
    
    if (NULL == (fp = fopen(filename, "r"))) {
        fprintf(stderr, "Can't open file %s\n", filename);
        return -1;
    }
    
    int columns = 0;
    int rows = 0;
    int line = 0;
    int matrix[32][32];
    int r[32];
    
    while(NULL != (s = fgets(buf, sizeof(buf), fp))) {
    
        char* t = strpbrk(s, "#\n\r");
        if (NULL != t)
            *t = "\0";
        while(isspace(*s))
            s++;
        if(!*s)
            continue;
        
        if (!columns) {
            columns = strtol(s, NULL, 10);
            continue;
        }
        if (!rows) {
            rows = strtol(s, NULL, 10);
            continue;
        }
        
        for (int i = 0; i < columns; i++) {
            matrix[line][i] = strtol(s, &s, 10);
            s++;
            while(isspace(*s))
                s++;
        }
        
        s = s+2;
        while(isspace(*s))
            s++;
        r[line] = strtol(s, NULL, 10);
        
        line++;
    }
    fclose(fp);
    
    printf("matrix:\n");
    for (int a = 0; a < rows; a++) {
        printf("\n");
        for (int b = 0; b < columns; b++) {
            printf("%d ",matrix[a][b]);
        }
    }
    printf("\n\nright hand side:\n\n");
    for (int c = 0; c < rows; c++) {
        printf("%d\n",r[c]);
    }
    
    printf("\ncalculating feasible solutions...\n");
    printf("these are the feasible solutions:\n\n");
    
    int x[columns];
    for (int i = 0; i < columns; i++)
        x[i] = 0;
    
    for (int i = 0; i < pow(2, columns); i++) {
        
        int print = 1;
        
        for (int j = 0; j < rows; j++) {
            if (scalar_product(x, matrix[j], columns) > r[j]) {
                print = 0;
                break;
            }
        }
        
        if (print) {
            for (int i = 0; i < columns; i++) {
                printf("%d ", x[i]);
            }
            printf("\n");
        }
        
        increment(x, columns);
    }
    return 0;
}

int scalar_product(int* x, int* y, int size) {
    int product = 0;
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