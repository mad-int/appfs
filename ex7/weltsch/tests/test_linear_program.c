#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "../src/linear_program.c"
#define TEST_FILE_1 "test_file_1.dat"
#define TEST_FILE_2 "feasibility_check.dat"
#define TEST_FILE_3 "feasibility_check_geq.dat"
#define TEST_FILE_4 "feasibility_check_eq.dat"
#define TEST_FILE_5 "test_constraint_types.dat"

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
    assert_int_equal(get_rows(lp), expected_rows);
    assert_int_equal(get_cols(lp), expected_cols);

    // check matrix entries
    assert_int_equal(lp->matrix[0][0], 1);
    assert_int_equal(lp->matrix[0][1], 2);
    assert_int_equal(lp->matrix[1][2], 3);
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

    int expected_sols = 3;
    int sols = get_bin_solutions_lp(lp);
    assert_int_equal(sols, expected_sols);
    lp_free(lp);
}

static void test_parse_row(void **state) {
    char* expected_row = "-1123 31234 11234 2123 123 <= 508976";
    LinearProgram* lp = lp_new(1, 5);
    assert_true(parse_row(expected_row, 0, lp));

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
    assert_false(parse_row(expected_row, 0, lp));
    lp_free(lp);
}

static void test_is_feasible(void **state) {
    unsigned int feasible = 13u;
    unsigned int not_feasible = 9u;
    LinearProgram* lp = new_lp_from_file(TEST_FILE_2);

    assert_true(is_feasible(feasible, lp));
    assert_false(is_feasible(not_feasible, lp));

    int expected_sols = 1;
    int sols = get_bin_solutions_lp(lp);
    assert_int_equal(sols, expected_sols);
    lp_free(lp);
}

static void test_is_feasible_geq(void **state) {
    unsigned int feasible = 13u;
    unsigned int not_feasible = 9u;
    LinearProgram* lp = new_lp_from_file(TEST_FILE_3);

    assert_true(is_feasible(feasible, lp));
    assert_false(is_feasible(not_feasible, lp));

    int expected_sols = 1;
    int sols = get_bin_solutions_lp(lp);
    assert_int_equal(sols, expected_sols);
    lp_free(lp);
}

static void test_is_feasible_eq(void **state) {
    unsigned int feasible = 13u;
    unsigned int not_feasible = 9u;
    LinearProgram* lp = new_lp_from_file(TEST_FILE_4);

    assert_true(is_feasible(feasible, lp));
    assert_false(is_feasible(not_feasible, lp));

    int expected_sols = 1;
    int sols = get_bin_solutions_lp(lp);
    assert_int_equal(sols, expected_sols);
    lp_free(lp);
}

static void test_is_feasible_mixed(void **state) {
    unsigned int feasible = 13u;
    unsigned int not_feasible = 9u;
    unsigned int not_feasible_zeros = 0;
    LinearProgram* lp = new_lp_from_file(TEST_FILE_4);

    assert_true(is_feasible(feasible, lp));
    assert_false(is_feasible(not_feasible, lp));
    assert_false(is_feasible(not_feasible_zeros, lp));

    int expected_sols = 1;
    int sols = get_bin_solutions_lp(lp);
    assert_int_equal(sols, expected_sols);
    lp_free(lp);
}

static void test_nth_bit(void **state) {
    unsigned int bits = 38u;

    assert_false(__get_nth_bit(bits, 0));
    assert_true(__get_nth_bit(bits, 1));
    assert_true(__get_nth_bit(bits, 2));
    assert_false(__get_nth_bit(bits, 3));
    assert_false(__get_nth_bit(bits, 4));
    assert_true(__get_nth_bit(bits, 5));
}

static void test_gray_code(void **state) {
    /* 7th gray_code is 101 */
    unsigned int gray_code = next_vars(6u);

    assert_true(__get_nth_bit(gray_code, 0));
    assert_false(__get_nth_bit(gray_code, 1));
    assert_true(__get_nth_bit(gray_code, 2));
}

static void test_constraint_type(void **state) {
    int expected_rows = 4;
    int expected_cols = 1;
    LinearProgram *lp = new_lp_from_file(TEST_FILE_5);
    assert_int_equal(get_rows(lp), expected_rows);
    assert_int_equal(get_cols(lp), expected_cols);

    assert_int_equal(lp->constraint_types[0], LEQ);
    assert_int_equal(lp->constraint_types[1], GEQ);
    assert_int_equal(lp->constraint_types[2], EQ);
    assert_int_equal(lp->constraint_types[3], EQ);

    int solutions = get_bin_solutions_lp(lp);
    assert_int_equal(solutions, 1);
}

int main(void) {
    const UnitTest tests[] = {
        unit_test(test_lp_new),
        unit_test(test_lp_parser),
        unit_test(test_parse_row),
        unit_test(test_parse_row_fail),
        unit_test(test_is_feasible),
        unit_test(test_is_feasible_geq),
        unit_test(test_is_feasible_eq),
        unit_test(test_is_feasible_mixed),
        unit_test(test_nth_bit),
        unit_test(test_gray_code),
        unit_test(test_constraint_type)
    };

    return run_tests(tests);
}

