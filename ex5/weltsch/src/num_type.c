#include "num_type.h"

#ifdef USE_DOUBLE

num_t parse_num(char* num_str, char** end_ptr) {
    return strtod(num_str, end_ptr);
}

bool is_num_valid(num_t num, char* num_str, char* end_ptr) {
    return (num_str != end_ptr);
}

void print_num(num_t num) {
    printf("%lf ", num);
}

#else

num_t parse_num(char* num_str, char** end_ptr) {
    return (num_t) strtol(num_str, end_ptr, 10);
}

bool is_num_valid(num_t num, char* num_str, char* end_ptr) {
    if (num >= INT_MAX || num <= INT_MIN) {
        fprintf(stderr, "number %ld is to big for an int\n", num);
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
