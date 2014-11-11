#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "../src/linear_program.c"
#define TEST_FILE_1 "test_file_1"

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

static void test_lp_parser(void **state) {
    int expected_rows = 3;
    int expected_cols = 3;
    LinearProgram *lp = new_lp_from_file(TEST_FILE_1);
    assert_int_equal(lp->rows, expected_rows);
    assert_int_equal(lp->cols, expected_cols);

    // check matrix entries
    assert_int_equal(lp->matrix[0][0], 1);
    assert_int_equal(lp->matrix[0][1], 2);
    assert_int_equal(lp->matrix[0][2], 3);
    assert_int_equal(lp->matrix[1][0], 1);
    assert_int_equal(lp->matrix[1][1], 2);
    assert_int_equal(lp->matrix[1][2], 3);
    assert_int_equal(lp->matrix[2][0], -4);
    assert_int_equal(lp->matrix[2][1], 3);
    assert_int_equal(lp->matrix[2][2], -5);

    // check vector entries
    assert_int_equal(lp->vector[0], 3);
    assert_int_equal(lp->vector[1], 2);
    assert_int_equal(lp->vector[2], 5);

    print_bin_solutions_lp(lp);
    lp_free(lp);
}

int main(void) {
    const UnitTest tests[] = {
        unit_test(test_lp_parser),
    };
    return run_tests(tests);
}

