/**@file   ex4.c
 * @brief  TODO Appfs Example: line wise text input.
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
#include "allocate.h"

#define MAX_LINE_LEN   512  // Maximum input line length

/** Read a textfile, remove comments and process lines.
 * @param filename name of file to read
 * @return number of lines read
 */
int process_file(const char* filename)
{
   assert(NULL != filename);
   assert(0    <  strlen(filename));

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;
	 
	 int var = 0, constr = 0;
	 int **A;
	 int *b;
   
   if (NULL == (fp = fopen(filename, "r")))
   {
      fprintf(stderr, "Can't open file %s\n", filename);
      return -1;
   }
   while(NULL != (s = fgets(buf, sizeof(buf), fp)))
   {
      char* t = strpbrk(s, "#\n\r");

      /*lines++;*/
      
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

      /* do processing here
       */
			
			if(lines == 0)
				 var = atoi(s);
			else if(lines == 1) {
				 constr = atoi(s);
				 A = malloc(constr * sizeof(A));
				 b = malloc(constr * sizeof(b));
				 /*A = allocate(constr, sizeof(A));
				 b = allocate(constr, sizeof(b));*/
			}
			else if(lines >= 2) {
				 A[lines-2] = malloc(var * sizeof(A[0]));
				 /*A[lines-2] = allocate(var, sizeof(A[lines-2]));*/
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
	 
	 printf("var: %i\n", var);
	 printf("constr: %i\n", constr);
	 printf("b: ");
	 int i;
	 for(i=0; i < constr; ++i)
		  printf("%i ",b[i]);
	 printf("\nM:\n");
	 for(i=0; i < constr; ++i){
		  int j;
			for(j=0; j < var; ++j)
				 printf("%i ", A[i][j]);
			printf("\n");
	 }
	 
	 getFeasibleBinary(A, b, var, constr);

   return lines;
}

void getFeasibleBinary(int** A, int* b, int var, int constr)
{
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
				 /*for(i=0;i<var;++i)
				 {
					  printf("%i",((x >> i) & 1));
				 }
				 printf("\n");*/
				 ++solutions;
			}
		  ++x;
	 } while(x != max);
	 printf("solutions: %i\n", solutions);
}

int main(int argc, char** argv)
{
   if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }
   printf("%d lines\n", process_file(argv[1]));
   
   return EXIT_SUCCESS;
}
 
