#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#ifdef USE_DOUBLE

typedef double num_t;

#else

typedef int num_t;

#endif

static const num_t MAX_COEF_VAL;
static const num_t MIN_COEF_VAL;

num_t parse_num(char* num_str, char** end_ptr);

bool is_num_valid(num_t num, char* num_str, char* end_str);

void print_num(num_t num);
