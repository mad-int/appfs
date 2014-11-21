#include <assert.h>  // assert
#include <stdio.h>   // printf
#include <stdlib.h>  // EXIT_*
#include <time.h>    // clock() and CLOCKS_PER_SEC

#include "constr.h"

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC)

//

int main(int argc, char** argv)
{
  { // Assert that the endianess is correct.
    binvec_t x = { .data = 0 };
    x._0 = 1;
    assert(x.data == 1);
  }

  if(argc < 2)
  {
    fprintf(stderr, "usage: %s filename\n", argv[0]);
    return EXIT_FAILURE;
  }

  // Read an inequality A[x] <= b from a file.
  constraints_t* Ab = malloc_constraints_from_file(argv[1]);
  if(NULL == Ab)
    return EXIT_FAILURE;
  assert(is_valid(*Ab));

  print_constraints(*Ab);

  clock_t start = clock();

  binvec_t x = { .data = 0 };

  /* Check x=0 "by hand" and
   * use it later to detect overflows.
   * (This can only happen when x has the maximal
   * 32 possible dimensions.)
   */
  if(is_feasible(*Ab, x))
      print_binvec(x, Ab->cols);

  const uint32_t max = Ab->cols < 32 ?
    (1 << Ab->cols) // = (2^Ab->cols)
    : 0;            // = 2^32 = 0x100000000 = 0 for 32-Bit

  // Enumerate all possible binary-vector values starting from 1.
  for(x.data=1; x.data<max; x.data++)
  {
    // Overflow happend we are out.
    if(0 == x.data)
      break;

    if(is_feasible(*Ab, x))
      print_binvec(x, Ab->cols);
  }

  // TODO: doesn't work for the full 32-Bit
  // as with the overflow x.data will be 0.
  double elapsed = GET_SEC(start, clock());
  printf("Checked %d vectors in %.3f s = %.3f kvecs/s\n",
      x.data, elapsed, x.data / elapsed / 1000.0);

  free_constraints(Ab);

  return EXIT_SUCCESS;
}

