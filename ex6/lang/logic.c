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
#include "logic.h"

#define MAX_LINE_LEN 512

problem* process_file(const char* filename)
{   
    assert(NULL != filename);
    assert(0 < strlen(filename));
    
    FILE* fp;
    char buf[MAX_LINE_LEN];
    char* s;
    
    if (NULL == (fp = fopen(filename, "r"))) {
        fprintf(stderr, "Can't open file %s\n", filename);
        abort();
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
        instance->flag[i] = *"?";
    }
    
    int line_num = 1;
    int row_num = 0;
    
    while(NULL != (s = fgets(buf, sizeof(buf), fp))) {
        
        check_line(s, MAX_LINE_LEN, line_num);
        
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
            if (isspace(*s) || (D && strchr(".", *s))) {
                s++; continue;
            } else if (strchr("#", *s)) {
                if (col_num < instance->columns ) {
                    printf("\nError: found %d coefficients in line %d, need: %d", col_num, line_num, instance->columns);
                    abort();
                }
                break;
            } else if (strchr("1234567890", *s)) {
                if (row_num >= instance->rows || col_num >= instance->columns) {
                    printf("\nError: too many rows or columns in line %d, rows: %d/%d, cols: %d/%d", line_num, row_num+1, instance->rows, col_num+1, instance->columns);
                    abort();
                }
                if(D) {
                    instance->matrix[row_num][col_num] = strtod(s, &s);
                } else {
                    instance->matrix[row_num][col_num] = strtol(s, &s, 10);
                }
                col_num++;
            } else if (strchr("<>=", *s)) {
                if (col_num < instance->columns) {
                    printf("\nError: found %d coefficients in line %d, need: %d", col_num, line_num, instance->columns);
                    abort();
                } else {
                    instance->flag[row_num] = *s;
                    int found_num = 0;
                    while(*s) {
                        if(strchr("<>=", *s) || isspace(*s) || (D && strchr(".",*s))) {
                            s++;
                            continue;
                        } else if (strchr("#", *s)) {
                            printf("\nError: no right hand side found in line %d", line_num);
                            abort();
                        } else if (strchr("1234567890", *s)) {
                            found_num = 1;
                            break;
                        }
                    }
                    if (!found_num) {
                        printf("\nError: no right hand side found in line %d", line_num);
                        abort();
                    }
                    if(D) {
                        instance->r[row_num] = strtod(s, &s);
                    } else {
                        instance->r[row_num] = strtol(s, &s, 10);
                    }
                    row_num++;
                }
            }
        }
        if (col_num < instance->columns) {
            printf("\nError: not enough columns in line %d, cols: %d/%d", line_num, col_num+1, instance->columns);
            abort();
        }
        line_num++;
    }
    fclose(fp);
    
    if (row_num < instance->rows) {
        printf("\nError: not enough valid rows: %d/%d", row_num+1, instance->rows);
        abort();
    }
    for (int i = 0; i < instance->rows; i++) {
            if (!strchr("<>=", instance->flag[i])) {
                printf("\nError: no right hand side for row %d", i);
                abort();
            }
    }
    return instance;
}

void print_solutions(problem* instance) {
    
    printf("matrix:\n");
    for (int i = 0; i < instance->rows; i++) {
        printf("\n");
        for (int j = 0; j < instance->columns; j++) {
            if (!D) printf("%d ", (int) instance->matrix[i][j]);
            else printf("%f ", (double) instance->matrix[i][j]);
        }
    }
    printf("\n\nright hand side:\n\n");
    for (int i = 0; i < instance->rows; i++) {
        if (!D) printf("%c= %d\n",instance->flag[i], (int) instance->r[i]);
        else printf("%c= %f\n",instance->flag[i], (double) instance->r[i]);
    }
    printf("\ncalculating feasible solutions...\n");
    printf("these are the feasible solutions:\n\n");
    
    int x[instance->columns];
    for (int i = 0; i < instance->columns; i++)
        x[i] = 0;
    int total = pow(2, instance->columns);
    int feasible = 0;
    double epsilon = pow(10, -7);
    for (int i = 0; i < pow(2, instance->columns); i++) {
        
        int print = 1;
        for (int j = 0; j < instance->rows; j++) {
            
            NUM product = (scalar_product(x, instance->matrix[j], instance->columns));
            
            if (strchr("<", instance->flag[j]) && product > instance->r[j]) {
                print = 0; break;
            } else if (strchr(">", instance->flag[j]) && product < instance->r[j]) {
                print = 0; break;
            } else if (strchr("=", instance->flag[j])) {
                NUM difference = product - instance->r[j];
                if (difference < -epsilon || difference > epsilon)
                    print = 0; break;
            }
        }
        if (print) {
            feasible++;
            for (int i = 0; i < instance->columns; i++) {
                printf("%d ", x[i]);
            }
            printf("\n");
        }
        increment(x, instance->columns);
    }
    printf("\ntotal number of feasible solutions: %d / %d", feasible, total);
}