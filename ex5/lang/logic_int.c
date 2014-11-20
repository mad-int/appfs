#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include "calculations.h"
#include "structure.h"
#include "allocate.h"
#include "checks.h"
#include "logic_int.h"

#define MAX_LINE_LEN 512

problem* process_file_int (const char* filename)
{   
    assert(NULL != filename);
    assert(0 < strlen(filename));
    
    FILE* fp;
    char buf[MAX_LINE_LEN];
    char* s;
    
    if (NULL == (fp = fopen(filename, "r"))) {
        fprintf(stderr, "Can't open file %s\n", filename);
        exit(EXIT_FAILURE);
    }
    
    problem* instance = allocate(1, sizeof(*instance));
    instance->columns = 0;
    instance->rows = 0;
    instance->matrix = allocate(32,sizeof(*(instance->matrix)));
    for (int i = 0; i < 32; i++) {
        instance->matrix[i] = allocate(32, sizeof(**(instance->matrix)));
    }
    instance->r = allocate(32,sizeof(*(instance->r)));
    instance->flag = allocate(32,sizeof(*(instance->flag)));
    for (int i = 0; i < 32; i++) {
        instance->flag[i] = "?";
    }
    
    int line_num = 1;
    int row_num = 0;
    
    while(NULL != (s = fgets(buf, sizeof(buf), fp))) {
        
        check_line_int(s, MAX_LINE_LEN, line_num);
        
        while(isspace(*s)) s++;
        if(!*s || strchr("#", *s)) {
            line_num++;
            continue;
        }
        
        if (!instance->columns) {
            check_line_rc(s, line_num);
            instance->columns = strtol(s, NULL, 10);
            line_num++;
            continue;
        }
        
        if (!instance->rows) {
            check_line_rc(s, line_num);
            instance->rows = strtol(s, NULL, 10);
            line_num++;
            continue;
        }
        
        int col_num = 0;
        
        while(*s) {
            //printf("\n%c\n",*s);
            if (isspace(*s)) {
                s++; continue;
            } else if (strchr("#", *s)) {
                if (col_num < instance->columns ) {
                    printf("\nError: found %d coefficients in line %d, need: %d", col_num, line_num, instance->columns);
                    exit(EXIT_FAILURE);
                }
                break;
            } else if (strchr("1234567890", *s)) {
                if (row_num >= instance->rows || col_num >= instance->columns) {
                    printf("\nError: too many rows or columns in line %d, rows: %d/%d, cols: %d/%d", line_num, row_num+1, instance->rows, col_num+1, instance->columns);
                    exit(EXIT_FAILURE);
                }
                instance->matrix[row_num][col_num] = strtol(s, &s, 10);
                col_num++;
            } else if (strchr("<>=", *s)) {
                if (col_num < instance->columns) {
                    printf("\nError: found %d coefficients in line %d, need: %d", col_num, line_num, instance->columns);
                    exit(EXIT_FAILURE);
                } else {
                    instance->flag[row_num] = *s;
                    int found_num = 0;
                    while(*s) {
                        if(strchr("<>=", *s) || isspace(*s)) {
                            s++;
                            continue;
                        } else if (strchr("#", *s)) {
                            printf("\nError: no right hand side found in line %d", line_num);
                            exit(EXIT_FAILURE);
                        } else if (strchr("1234567890", *s)) {
                            found_num = 1;
                            break;
                        }
                    }
                    if (!found_num) {
                        printf("\nError: no right hand side found in line %d", line_num);
                        exit(EXIT_FAILURE);
                    }
                    instance->r[row_num] = strtol(s, &s, 10);
                    row_num++;
                }
            }
        }
        if (col_num < instance->columns) {
            printf("\nError: not enough columns in line %d, cols: %d/%d", line_num, col_num+1, instance->columns);
            exit(EXIT_FAILURE);
        }
        line_num++;
    }
    fclose(fp);
    
    if (row_num < instance->rows) {
        printf("\nError: not enough valid rows: %d/%d", row_num+1, instance->rows);
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < instance->rows; i++) {
            if (!strchr("<>=", instance->flag[i])) {
                printf("\nError: no right hand side for row %d", i);
                exit(EXIT_FAILURE);
            }
    }
    return instance;
}

void print_solutions_int(problem* instance) {
    
    printf("matrix:\n");
    for (int i = 0; i < instance->rows; i++) {
        printf("\n");
        for (int j = 0; j < instance->columns; j++) {
            printf("%d ", instance->matrix[i][j]);
        }
    }
    printf("\n\nright hand side:\n\n");
    for (int i = 0; i < instance->rows; i++) {
        printf("%c= %d\n",instance->flag[i],instance->r[i]);
    }
    printf("\ncalculating feasible solutions...\n");
    printf("these are the feasible solutions:\n\n");
    
    int x[instance->columns];
    for (int i = 0; i < instance->columns; i++)
        x[i] = 0;
    
    for (int i = 0; i < pow(2, instance->columns); i++) {
        
        int print = 1;
        
        for (int j = 0; j < instance->rows; j++) {
            int product = (scalar_product(x, instance->matrix[j], instance->columns));
            
            if (strchr("<", instance->flag[j]) && product > instance->r[j]) {
                print = 0; break;
            } else if (strchr(">", instance->flag[j]) && product < instance->r[j]) {
                print = 0; break;
            } else if (strchr("=", instance->flag[j]) && product != instance->r[j]) {
                print = 0; break;
            }
        }
        
        if (print) {
            for (int i = 0; i < instance->columns; i++) {
                printf("%d ", x[i]);
            }
            printf("\n");
        }
        
        increment(x, instance->columns);
    }
}