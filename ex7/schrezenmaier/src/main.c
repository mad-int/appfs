/**
 * @file   main.c
 * @brief  Contains the main method of a program which reads in a linear program Ax<=b
 *         and prints out all feasible binary solutions.
 * @author Hendrik Schrezenmaier
 * @date   02 Dez 2014
 */
 
#include <stdio.h>   // fprintf
#include <stdlib.h>  // EXIT_*
#include "linear_program.h"

int main(int argc, char** argv)
{
   if(argc < 2 || argc > 3)
   {
      fprintf(stderr, "usage: %s input_filename [output_filename]\n", argv[0]);
      return EXIT_FAILURE;
   }
   
   char* input_filename = argv[1];
   
   char* output_filename = "/dev/null";
   if(argc >= 3)
      output_filename = argv[2];
   
   LinearProgram* lp = read_from_file_lp(input_filename);
   print_feasible_binary_lp(lp, output_filename);
   free_lp(lp);
   
   return EXIT_SUCCESS;
}
 
