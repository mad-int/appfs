/**@file   ex4.c
 * @brief  Appfs Exercise 4: Read linear program Ax<=b and print all feasible binary solutions.
 * @author Hendrik Schrezenmaier
 * @date   12Nov2014
 *
 * gcc -O2 -Wall -o ex4 ex4.c
 *
 * Using fgets() for input
 */  
#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <assert.h>  // assert
#include "linear_program.h"

int main(int argc, char** argv)
{
   
   if (argc < 3)
   {
      fprintf(stderr, "usage: %s input_filename output_filename\n", argv[0]);
      return EXIT_FAILURE;
   }
   
   LinearProgram* lp = read_from_file_lp(argv[1]);
   print_lp(lp);
   print_feasible_binary_lp(lp, argv[2]);
   
   return EXIT_SUCCESS;
}
 
