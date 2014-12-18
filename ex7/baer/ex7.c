#include <assert.h>  // assert
#include <stdio.h>   // printf
#include <stdlib.h>  // EXIT_*
#include <time.h>    // clock() and CLOCKS_PER_SEC

#include "constr.h"

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC)

/*
 * I print all the feasible vectors right now.
 * This is bad for time measurement
 * (one don't want the print-time to interfere),
 * but the testcase rely on this behaviour.
 * (run_check.sh that is called on "make check".)
 *
 * Comment print_binvec() below for time measurement.
 */

/* In one exercise, we should split our enumeration-program over multiple files.
 * I decided to have the enumeration-functionality here in this file
 * and the constraint-handling stuff in constr.[ch]
 * in such a way that this USE_DOUBLE and USE_INT define-compile-flag
 * only has effects in constr.c and not here,
 * to keep this complexity there.
 *
 * But in the last exercise with the "column"-optimization
 * (with n and neg n) this design decision wore off a bit.
 * (If it was any good at all.)
 * For this reason I had to introduce
 * this ugly state-parameter in is_feasible_bitflip().
 */

//

void reorder_rows(constraints_t* cs);

//

int main(int argc, char** argv)
{
  { // Assert that the endianess is correct.
    binvec_t x = { .data = 0 };
    x.bits._0 = 1;
    assert(x.data == 1);
  }

  if(argc < 2)
  {
    fprintf(stderr, "usage: %s filename\n", argv[0]);
    return EXIT_FAILURE;
  }

  // Read an inequality A[x] <= b from a file.
  constraints_t Ab = malloc_constraints_from_file(argv[1]);
  if(is_null(Ab))
    return EXIT_FAILURE;
  assert(is_valid(Ab));

  reorder_rows(&Ab);

  print_constraints(Ab);

  clock_t start = clock();
  uint32_t runs = 0;
  size_t sols = 0; // solutions found

  binvec_t x = { .data = 0 };
  void *state = NULL;

  // Do the old is_feasible()-check only for coverage purposes.
  is_feasible(Ab, x);

  // Zero is a kind of special case as no bit was flipped, yet.
  // Check it first.
  if(is_feasible_bitflip(Ab, x, 0, &state))
  {
    print_binvec(x, Ab.cols);
    sols++;
  }

  // On bit-level (here for cols == 4):
  // n = 0001 and negn is all ones: negn = 1111;
  uint32_t n = 0x1;
  uint32_t negn = (uint32_t)1 << ((uint32_t)Ab.cols - 1);
  negn += (negn - 1);

  /* Enumerate all possible binary-vector values in a smart way
   * by bit flipping only 1 bit at a time.
   */
  for(; negn != 0; n++, negn--, runs++)
  {
    /* updatemask has only one bit set at each time
     * and this bit is flipped in x.
     */
    uint32_t updatemask = n & negn;
    x.data ^= updatemask;

    /* For time measurement you may want to comment
     * "print_binvec()" below, but caution
     * this will break the testcase in run_checks.sh,
     * that check if the calculation is correct!
     * They rely on this output.
     */

    /* This is the old way.
    if(is_feasible(Ab, x))
    {
      print_binvec(x, Ab.cols);
      sols++;
    }*/

    if(is_feasible_bitflip(Ab, x, updatemask, &state))
    {
      print_binvec(x, Ab.cols);
      sols++;
    }
  }

  /* "runs" counted only the loop runs.
   * The zero-vector was checked additionally before the loop.
   * For 32-Bit "runs+1" overflows to 0. May should take long.
   */
  double elapsed = GET_SEC(start, clock());
  printf("Checked %d vectors in %.3f s = %.3f kvecs/s\n",
      runs+1, elapsed, runs / elapsed / 1000.0);
  printf("%ld feasible vectors found.\n", sols);

  free_constraints(Ab);
  if(NULL != state)
    free(state);

  return EXIT_SUCCESS;
}

void reorder_rows(constraints_t* cs)
{
  /* Delete redundant rows.
   * Only rows that are redundant wrt another row.
   * Maybe shouldn't do this as it changes the constraint system
   * and this is not allow for the task. But anyway.
   */
  int r1=0, r2=0;
  for(r1=0; r1<cs->rows; r1++)
    for(r2=r1+1; r2<cs->rows; r2++)
    {
      if(is_redunant(*cs, r1, r2))
      {
        delete_row(cs, r2);
        r2--;
      }
      else if(is_redunant(*cs, r2, r1))
      {
        delete_row(cs, r1);
        r1--;
        break;
      }
    }

  // Sort up the rows that look like(!) as if they fail first.
  reorder_rows_failfirst(cs);
}

