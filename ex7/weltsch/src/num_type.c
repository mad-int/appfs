#include <limits.h>
#include "num_type.h"

#ifdef USE_DOUBLE

num_t parse_num(char* num_str, char** end_ptr) {
    return strtod(num_str, end_ptr);
}

bool is_num_valid(num_t num, char* num_str, char* end_ptr) {
    if (num < MIN_COEF_VAL || num > MAX_COEF_VAL) {
        return false;
    }

    return (num_str != end_ptr);
}

void print_num(num_t num) {
    printf("%.0lf ", num);
}

#else

num_t parse_num(char* num_str, char** end_ptr) {
    return (num_t) strtol(num_str, end_ptr, 10);
}

bool is_num_valid(num_t num, char* num_str, char* end_ptr) {
    if (num > MAX_COEF_VAL || num < MIN_COEF_VAL) {
        return false;
    }

    if (num_str == end_ptr) {
        return false;
    }
    return true;
}

void print_num(num_t num) {
    printf("%d ", num);
}

#endif
