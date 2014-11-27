#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#ifdef USE_DOUBLE

typedef double num_t;

static const num_t MAX_COEF_VAL = (num_t) LLONG_MAX;
static const num_t MIN_COEF_VAL = (num_t) LLONG_MIN;

#else

typedef int num_t;

static const num_t MAX_COEF_VAL = INT_MAX;
static const num_t MIN_COEF_VAL = INT_MIN;

#endif

num_t parse_num(char* num_str, char** end_ptr);

bool is_num_valid(num_t num, char* num_str, char* end_str);

void print_num(num_t num);
