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
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include "linear_program.h"

#define MAX_LINE_LEN   512  // Maximum input line length

/** Calculate the feasible binary solutions of the given linear program and print them to a text file.
 *  @param A the matrix of the linear program Ax<=b
 *  @param b the right hand side of the linear program Ax<=b
 *  @param var the number of variables
 *  @param constr the number of constraints
 *  @param out_filename name of the file to write the solutions to
 */
void printFeasibleBinary(int** A, int* b, int var, int constr, const char* out_filename)
{
    assert(NULL != out_filename);
    assert(0 < strlen(out_filename));
   
    FILE *output;
    if(NULL == (output = fopen(out_filename, "w")))
    {
        fprintf(stderr, "Can't open file %s\n", out_filename);
    }
    
    long x = 0l;
    long x_count = 0l;
    int eval[constr];
    int i;
    for(i=0; i < constr; ++i)
       eval[i] = 0;
    int solutions = 0;
    while(1)
    {
       /* check current vector for feasibility */
       int feasible = 1;
       for(i = 0; i < constr; ++i)
       {
          if(eval[i] > b[i])
          {
             feasible = 0;
             break;
          }
       }
       
       /* print feasible solution to file */
       if (feasible)
       {
          for(i=0;i<var;++i)
          {
             fprintf(output, "%ld",((x >> i) & 1l));
          }
          fprintf(output, "\n");
          ++solutions;
       }
       
       /* calculate new vector in gray code */
       ++x_count;
       long x_new = x_count ^ (x_count >> 1);
       long diff = x_new ^ x;
       int changed_var = 0;
       while((1ul & diff) == 0)
       {
          ++changed_var;
          diff >>= 1;
       }
       if(changed_var >= var)
          break;
       x = x_new;
       
       /* calculate new evaluation */
       int sign = -1;
       if(x & (1ul << changed_var))
          sign = 1;
       for(i = 0; i < constr; ++i)
          eval[i] += sign * A[i][changed_var];
    }
    fclose(output);
    printf("printed %i solutions to file %s\n", solutions, out_filename);
}

/** Read a linear program from text file, calculate its feasible binary solutions and print them to a text file.
 * @param filename name of file to read
 * @param out_filename name of file to write to
 */
void process_file(const char* filename, const char* out_filename)
{
   assert(NULL != filename);
   assert(0 < strlen(filename));
   assert(NULL != out_filename);
   assert(0 < strlen(out_filename));

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;
	 
	 int var = 0, constr = 0;
	 int **A;
	 int *b;
   
   if (NULL == (fp = fopen(filename, "r")))
      fprintf(stderr, "Can't open file %s\n", filename);
   while(NULL != (s = fgets(buf, sizeof(buf), fp)))
   {
      char* t = strpbrk(s, "#\n\r");

      if (NULL != t) /* else line is not terminated or too long */
         *t = '\0';  /* clip comment or newline */
         
      /* Skip over leading space
       */
      while(isspace(*s))
         s++;

      /* Skip over empty lines
       */
      if (!*s)  /* <=> (*s == '\0') */
         continue;

      if(lines == 0)
				 var = atoi(s);
			else if(lines == 1) {
				 constr = atoi(s);
				 A = malloc(constr * sizeof(A));
				 b = malloc(constr * sizeof(b));
			}
			else if(lines >= 2) {
				 A[lines-2] = malloc(var * sizeof(A[0]));
				 int i = 0;
				 char *ptr;
				 ptr = strtok(s, " ");
				 while(ptr != NULL) {
					  if(i < var)
							 A[lines-2][i] = atoi(ptr);
						else if(i == var+1)
							 b[lines - 2] = atoi(ptr);
						ptr = strtok(NULL, " ");
						++i;
				 }
			}
			
			++lines;
			
   }
   fclose(fp);
	 
	 printFeasibleBinary(A, b, var, constr, out_filename);
}

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
 
