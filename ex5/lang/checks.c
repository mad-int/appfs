#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "checks.h"

void check_line_int(char* line, int max_len, int line_num) {
    int len = strlen(line);
    if (len > max_len) {
        printf("Line too long: %d, max: %d", len, max_len);
        exit(EXIT_FAILURE);
    }
    char *p = line;
    while(*p) {
        //printf("%c\n",*p);
        if (strchr("#", *p)) {
            break;
        } else if (isspace(*p)) {
            p++;
            continue;
        } else if (strchr("1234567890<>=", *p)) {
            p++;
            continue;
        } else {
            printf("\nError: invalid character '%c' in line %d", *p, line_num);
            exit(EXIT_FAILURE);
        }
    }   
}

void check_line_double(char* line, int max_len, int line_num) {
        int len = strlen(line);
    if (len > max_len) {
        printf("Line too long: %d, max: %d", len, max_len);
        exit(EXIT_FAILURE);
    }
    char *p = line;
    while(*p) {
        //printf("%c\n",*p);
        if (strchr("#", *p)) {
            break;
        } else if (isspace(*p)) {
            p++;
            continue;
        } else if (strchr("1234567890<>=.", *p)) {
            p++;
            continue;
        } else {
            printf("\nError: invalid character '%c' in line %d", *p, line_num);
            exit(EXIT_FAILURE);
        }
    }   
}

void check_line_rc(char* line, int line_num) {
    char *p = line;
    while(*p) {
        if (strchr("#", *p)) {
            break;
        } else if (isspace(*p)) {
            p++;
            continue;
        } else if (strchr("1234567890", *p)) {
            p++;
            continue;
        } else if (strchr("<>=", *p)) {
            printf("\nError: invalid character '%c' in line %d\nSpecify number of rows and columns first", *p, line_num);
            exit(EXIT_FAILURE);
        } else {
            printf("\nError: invalid character '%c' in line %d", *p, line_num);
            exit(EXIT_FAILURE);
        }
    }
}