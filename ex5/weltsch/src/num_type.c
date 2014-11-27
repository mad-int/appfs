#include <limits.h>
#include "num_type.h"

#ifdef USE_DOUBLE
static const num_t MAX_COEF_VAL = (num_t) LLONG_MAX;
static const num_t MIN_COEF_VAL = (num_t) LLONG_MIN;

num_t parse_num(char* num_str, char** end_ptr) {
    return strtod(num_str, end_ptr);
}

bool is_num_valid(num_t num, char* num_str, char* end_ptr) {
    if (num < MIN_COEF_VAL || num > MAX_COEF_VAL) {
        fprintf(stderr, "number %.0lf is to big for an int\n", num);
        return false;
    }

    return (num_str != end_ptr);
}

void print_num(num_t num) {
    printf("%.0lf ", num);
}

#else
static const num_t MAX_COEF_VAL = INT_MAX;
static const num_t MIN_COEF_VAL = INT_MIN;

num_t parse_num(char* num_str, char** end_ptr) {
    return (num_t) strtol(num_str, end_ptr, 10);
}

bool is_num_valid(num_t num, char* num_str, char* end_ptr) {
    if (num > MAX_COEF_VAL || num < MIN_COEF_VAL) {
        fprintf(stderr, "number %d is to big for an int\n", num);
        return false;
    }

    if (num_str == end_ptr) {
        fprintf(stderr, "not a valid integer %s\n", num_str);
        return false;
    }
    return true;
}

void print_num(num_t num) {
    printf("%d ", num);
}

#endif
