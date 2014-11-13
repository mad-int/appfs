/**@file   ex4.c
 * @brief  Appfs Exercise 4: Read linear program Ax<=b and print all feasible binary solutions.
 * @author Hendrik Schrezenmaier
 * @date   12Nov2014
 *
 * gcc -O2 -Wall -o ex4_readline ex4_readline.c
 *
 * Using fgets() for input
 */  
#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace

#define MAX_LINE_LEN   512  // Maximum input line length

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

/** Calculate the feasible binary solutions of the given linear program and print them to a text file.
 * @param A the matrix of the linear program Ax<=b
 * @param b the right hand side of the linear program Ax<=b
 * @param var the number of variables
 * @param constr the number of constraints
 * @param out_filename name of the file to write the solutions to
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
	 
   int x = 0;
	 int max = 1 << var;
	 if (var == 32)
		  max = 0;
	 int solutions = 0;
	 do
	 {
		  int i;
			int feasible = 1;
		  for(i=0; i < constr; i++)
			{
				 int sum = 0;
				 int j;
				 for(j=0;j<var;++j)
				 {
					  sum += ((x & (1 << j)) >> j) * A[i][j];
				 }
				 if (sum > b[i])
				 {
					  feasible = 0;
						break;
				 }
			}
			if (feasible)
		  {
				 for(i=0;i<var;++i)
				 {
					  fprintf(output, "%i",((x >> i) & 1));
				 }
				 fprintf(output, "\n");
				 ++solutions;
			}
		  ++x;
	 } while(x != max);
	 fclose(output);
	 printf("printed %i solutions to file\n", solutions);
}

int main(int argc, char** argv)
{
   if (argc < 3 || strlen(argv[1]) <= 0 || strlen(argv[2]) <= 0)
   {
      fprintf(stderr, "usage: %s input_filename output_filename", argv[0]);
      return EXIT_FAILURE;
   }
   process_file(argv[1], argv[2]);
   
   return EXIT_SUCCESS;
}
 
