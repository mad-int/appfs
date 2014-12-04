#include <assert.h>  // assert
#include <stdio.h>   // printf
#include <stdlib.h>  // EXIT_*
#include <time.h>    // clock() and CLOCKS_PER_SEC

#include "constr.h"

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC)


//

void sort_rows(constraints_t* cs);

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

  sort_rows(&Ab);

  print_constraints(Ab);

  clock_t start = clock();

  binvec_t x = { .data = 0 };

  /* Check x=0 "by hand" and
   * use it later to detect overflows.
   * (This can only happen when x has the maximal
   * 32 possible dimensions.)
   */
  if(is_feasible(Ab, x))
    print_binvec(x, Ab.cols);

  // Conversion only to get rid of compiler warnings (-Wsign-conversion).
  assert(Ab.cols > 0);
  uint32_t Ab_cols = (unsigned int)Ab.cols;
  const uint32_t max = Ab.cols < 32 ?
    (uint32_t)(1 << Ab_cols)-1 // = (2^Ab->cols)
    : UINT32_MAX;            // = 2^32 = 0x100000000 = 0 for 32-Bit

  // Enumerate all possible binary-vector values starting from 1.
  for(x.data=1; x.data<=max; x.data++)
  {
    // Overflow happend we are out.
    if(0 == x.data)
      break;

    if(is_feasible(Ab, x))
      print_binvec(x, Ab.cols);
  }

  // We checked "max"-vectors here.
  double elapsed = GET_SEC(start, clock());
  printf("Checked %d vectors in %.3f s = %.3f kvecs/s\n",
      max, elapsed, max / elapsed / 1000.0);

  free_constraints(Ab);

  return EXIT_SUCCESS;
}

void sort_rows(constraints_t* cs)
{
  // Delete redundant rows.
  // Only rows that are redundant wrt another row.
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
  sort_rows_failfirst(cs);
}

