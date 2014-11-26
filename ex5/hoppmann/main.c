/*
 * main.c
 *
 *  Created on: 12.11.2014
 *      Author: bzfhoppm
 */

#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include <stdbool.h> // bool

#include "lin_ieq_system.h"
#include "readline.h"

int main(int argc, char** argv)
{
   if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename\n", argv[0]);
      return EXIT_FAILURE;
   }

   LinIeqSys* linIeqSys;


   int lines = process_file(argv[1], &linIeqSys);

   if(lines < 0)
   {
      fprintf(stderr, "Error while processing file %s\n", argv[1]);
      return EXIT_FAILURE;
   }

   printf("%d lines of input file processed.\n", lines);

   linIeqSys_print(linIeqSys);

   linIeqSys_solve(linIeqSys);

   linIeqSys_free(linIeqSys);

   return EXIT_SUCCESS;
}
