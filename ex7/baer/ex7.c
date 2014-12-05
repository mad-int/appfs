#include <assert.h>  // assert
#include <stdio.h>   // printf
#include <stdlib.h>  // EXIT_*
#include <time.h>    // clock() and CLOCKS_PER_SEC

#include "constr.h"

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC)


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

  binvec_t x = { .data = 0 };
  void *state = NULL;

  // Zero is a kind of special case as no bit was flipped, yet.
  // Check it first.
  if(is_feasible_bitflip(Ab, x, 0, &state))
    print_binvec(x, Ab.cols);

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

    //if(is_feasible(Ab, x))
    //  print_binvec(x, Ab.cols);

    if(is_feasible_bitflip(Ab, x, updatemask, &state))
      print_binvec(x, Ab.cols);
  }

  /* "runs" counted only the loop runs.
   * The zero-vector was checked additionally before the loop.
   * For 32-Bit "runs+1" overflows to 0. May should take long.
   */
  double elapsed = GET_SEC(start, clock());
  printf("Checked %d vectors in %.3f s = %.3f kvecs/s\n",
      runs+1, elapsed, runs / elapsed / 1000.0);

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

