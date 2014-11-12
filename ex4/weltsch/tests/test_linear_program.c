#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "../src/linear_program.c"
#define TEST_FILE_1 "test_file_1"
#define TEST_FILE_2 "feasibility_check"

/* creates a new lp */
static void test_lp_new(void **state) {
    int expected_rows = 4;
    int expected_cols = 5;
    LinearProgram *lp = lp_new(expected_rows, expected_cols);
    assert_int_equal(lp->rows, expected_rows);
    assert_int_equal(lp->cols, expected_cols);
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

static void test_parse_row(void **state) {
    char* expected_row = "-1123 31234 11234 2123 <= 508976";
    LinearProgram* lp = lp_new(1, 5);
    parse_row(expected_row, 0, lp);

    assert_int_equal(lp->matrix[0][0], -1123);
    assert_int_equal(lp->matrix[0][1], 31234);
    assert_int_equal(lp->matrix[0][2], 11234);
    assert_int_equal(lp->matrix[0][3], 2123);

    assert_int_equal(lp->vector[0], 508976);
    lp_free(lp);
}

static void test_parse_row_fail(void **state) {
    char* expected_row = "abc 1243 >= c";
    LinearProgram* lp = lp_new(1, 5);
    int ret_val = parse_row(expected_row, 0, lp);

    assert_int_equal(ret_val, -1);
    lp_free(lp);
}

static void test_next_configuration(void **state) {
    int len = 5;
    int config[] = {1, 0, 0, 1, 1, 0};

    next_configuration(config, len);
    assert_int_equal(0, config[0]);
    assert_int_equal(1, config[1]);
    assert_int_equal(0, config[2]);
    assert_int_equal(1, config[3]);
    assert_int_equal(1, config[4]);
    assert_int_equal(0, config[5]);
}

static void test_is_feasible(void **state) {
    int feasible[] = {1, 0, 1, 1, 0, 0};
    int not_feasible[] = {0, 0, 1, 0, 0, 1};
    LinearProgram* lp = new_lp_from_file(TEST_FILE_2);

    assert_true(is_feasible(feasible, lp));
    assert_false(is_feasible(not_feasible, lp));
    lp_free(lp);
}

int main(void) {
    const UnitTest tests[] = {
        unit_test(test_lp_new),
        unit_test(test_lp_parser),
        unit_test(test_parse_row),
        unit_test(test_parse_row_fail),
        unit_test(test_next_configuration),
        unit_test(test_is_feasible)
    };
    return run_tests(tests);
}

