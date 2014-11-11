#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "../src/linear_program.c"

// create an empty
static void test_lp_creation(void **state) {
    int expected_rows = 4;
    int expected_cols = 5;
    LinearProgram *lp = lp_new(expected_rows, expected_cols);
    assert_int_equal(lp->rows, expected_rows);
    assert_int_equal(lp->cols, expected_cols);
    print_bin_solutions_lp(lp);
    lp_free(lp);
}

int main(void) {
    const UnitTest tests[] = {
        unit_test(test_lp_creation)
    };
    return run_tests(tests);
}

