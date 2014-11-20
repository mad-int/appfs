#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include "../allocate.h"

#define MAX_LINE_LEN   512  // Maximum input line length

struct{
   int** conss;
   int*  rhs;
   int   nconss;
   int   nvars;
} typedef BIP;


/** Read a textfile, remove comments and process lines.
 * @param filename name of file to read
 * @param prob problem data structure
 * @return number of lines read
 */
int process_file(const char* filename, BIP* prob)
{
   assert(NULL != filename);
   assert(0    <  strlen(filename));

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;
   int   cons_number = 0;
   int   var_number = 0;
   int   rhs_marker = 0;

   if( NULL == (fp = fopen(filename, "r")) )
   {
      fprintf(stderr, "Can't open file %s\n", filename);
      return -1;
   }
   while( NULL != (s = fgets(buf, sizeof(buf), fp)) )
   {
      char* t = strpbrk(s, "#\n\r");

      lines++;

      if( NULL != t ) /* else line is not terminated or too long */
         *t = '\0';  /* clip comment or newline */

      /* Skip over leading space
         */
      while( isspace(*s) )
         s++;

      /* Skip over empty lines
         */
      if( !*s )  /* <=> (*s == '\0') */
         continue;

      // read number of variables
      if( lines == 1 )
      {
         prob->nvars = atoi(s);
         continue;
      }
      // read number of constraints
      if( lines == 2 )
      {
         prob->nconss = atoi(s);
         prob->conss = allocate(prob->nconss, sizeof(*(prob->conss)));
         for( int i = 0; i < prob->nconss; ++i )
         {
            prob->conss[i] = allocate(prob->nvars, sizeof(**(prob->conss)));
         }
         prob->rhs = allocate(prob->nconss, sizeof(*(prob->rhs)));
         continue;
      }
      // read constraints
      for( int i = 0; i < prob->nvars; ++i )
      {
         prob->conss[cons_number][i] = atoi(s);
         s++;
         while( isspace(*s) )
            s++;
      }

      // skip over "<="
      s++;
      s++;
      while( isspace(*s) )
         s++;

      // read rhs
      prob->rhs[cons_number] = atoi(s);
      cons_number++;
   }
   fclose(fp);

   return lines;
}

void next_one( int* vars, int nvars )
{
   for( int i = 0; i < nvars; ++i )
   {
      if( vars[i] == 0)
      {
         vars[i] = 1;
         return;
      }
      else
         vars[i] = 0;
   }
}

int is_feasible( int* vars, BIP* prob)
{
   int sum;
   for( int i = 0; i < prob->nconss; ++i )
   {
      sum = 0;
      for( int j = 0; j < prob->nvars; ++j )
      {
         sum += prob->conss[i][j] * vars[j];
         if( sum > prob->rhs[i] )
            return 0;
      }
   }
   return 1;
}

void print_solutions( BIP* prob )
{
   int max_sols = 2;
   int nfeas = 0;
   int* vars;

   vars = allocate(prob->nvars, sizeof(*vars));

   // all possible combinations
   for( int i = 1; i < prob->nvars; ++i)
      max_sols *= 2;

   // init variables
   for( int i = 0; i < prob->nvars; ++i )
      vars[i] = 0;

   // find for feasible solutions
   for( int i = 0; i < max_sols; ++i )
   {
      if( is_feasible(vars, prob) )
      {
         for( int j = 0; j < prob->nvars; ++j )
            printf("%d ", vars[j]);
         printf("\n");
         ++nfeas;
      }
      next_one(vars, prob->nvars);
   }
   printf("found %d feasible solutions\n", nfeas);
}

int main(int argc, char** argv)
{
   if( argc < 2 || strlen(argv[1]) <= 0 )
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }

   // create problem pointer
   BIP* prob;
   prob = allocate(1, sizeof(*prob));

   printf("%d lines\n", process_file(argv[1], prob));
   printf("nvars: %d\n", prob->nvars);
   printf("nconss: %d\n", prob->nconss);
   for( int i = 0; i<prob->nconss; ++i)
   {
      for( int j = 0; j<prob->nvars; ++j)
         printf("%d ", prob->conss[i][j]);
      printf("<= %d\n", prob->rhs[i]);
   }

   printf("\n");

   print_solutions(prob);

   return EXIT_SUCCESS;
}
