/**@file   ex6.c
 * @brief  Appfs Exercise 6: BIP Enumerator V3
 * @author Thorsten Koch
 * @date   25Nov2014
 */  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <time.h>
#include <limits.h>
#include <errno.h>
#include <getopt.h>

#include "mshell.h" 
#include "bip.h"
#include "enum_basic.h"

#define GET_SEC(a, b)  ((b - a) / (double)CLOCKS_PER_SEC) 

enum verbosity  { VERB_QUIET = 0, VERB_NORMAL, VERB_VERBOSE, VERB_CHATTER, VERB_DEBUG };
typedef enum verbosity Verbosity;

static Verbosity verbosity = VERB_NORMAL;

static void report_sol(const BIP* bip, unsigned int x)
{
   assert(bip_is_feasible(bip, x));

   unsigned int bit = 1;

   if (VERB_VERBOSE <= verbosity)
   {
      printf("%8x: ", x);
      for(int c = 0; c < bip_cols(bip); c++, bit += bit)
         printf("%d ", (x & bit) ? 1 : 0);
      printf("\n");
   }
}


/** Usage: ex6 bip_file.
  */
int main(int argc, char** argv) 
{
   static const char* banner = 
      "***************************************\n"  \
      "* EX6 - APPFS BIP Enumeration Program *\n"  \
      "* Copyright (C) 2014 by Thorsten Koch *\n"  \
      "***************************************\n";  

   static const char* options = "hv:V";
   static const char* usage   = "usage: %s [options] file.dat\n";

   static const char* help =
      "\n"                                  \
      "  -h             show this help.\n"                          \
      "  -v[0-5]        verbosity level: 0 = quiet, 1 = default, up to 5 = debug\n" \
      "  -V             print program version\n"                        \
      "  file.dat       is the name of the BIP input file.\n"           \
      "\n" ; 

   int option;
   
   while((option = getopt(argc, argv, options)) != -1)
   {
      switch(option)
      {
      case 'h' :
         printf(banner, VERSION);
         printf(usage, argv[0]);
         puts(help);
         return EXIT_SUCCESS;
      case 'v' :
         switch(atoi(optarg))
         {
         case 0 : 
            verbosity = VERB_QUIET;
            break;
         case 1 : 
            verbosity = VERB_NORMAL;
            break;
         case 2 : 
            verbosity = VERB_VERBOSE;
            break;
         case 3 : 
            verbosity = VERB_CHATTER;
            break;
         case 4 : 
            verbosity = VERB_DEBUG;
            break;
         default :
            fprintf(stderr, usage, argv[0]);      
            return EXIT_FAILURE;
         }
         break;
      case 'V' :
         printf("%s\n", VERSION);
         return EXIT_SUCCESS;
         break;
      case '?':
         fprintf(stderr, usage, argv[0]);
         return EXIT_SUCCESS;
      default :
         abort();
      }
   }
   if ((argc - optind) < 1)
   {
      fprintf(stderr, usage, argv[0]);      
      return EXIT_FAILURE;
   }
   if (verbosity >= VERB_NORMAL)
      printf(banner, VERSION);

   bip_init();
   
   BIP* bip = bip_read(argv[1]);

   if (NULL == bip)
      return EXIT_FAILURE;

   bip_scale(bip);

   if (VERB_CHATTER <= verbosity)
      bip_print(bip, stdout);

   const int    cols      = bip_cols(bip);
   const int    rows      = ((bip_rows(bip) + 3) / 4) * 4;
   int          equations = 0;       
   const Numb** pcol      = calloc((size_t)cols, sizeof(*pcol));
   Numb*        store     = calloc((size_t)rows * (size_t)cols, sizeof(*store));   
   Numb*        rhs       = calloc((size_t)rows, sizeof(*rhs));

   /* copy equations and remove GE
    */
   int rcnt = 0;
      
   for(int r = 0; r < bip_rows(bip); r++)
   {
      if (bip_sense(bip, r) == EQ)
      {
         rhs[rcnt] = bip_rhs(bip, r);
         
         for(int c = 0; c < bip_cols(bip); c++)
            store[c * rows + rcnt] = bip_a(bip, r, c);

         equations++;
         rcnt++;
      }
   }
   /* copy rest
    */
   for(int r = 0; r < bip_rows(bip); r++)
   {
      switch(bip_sense(bip, r))
      {
      case LE :
         rhs[rcnt] = bip_rhs(bip, r);
         
         for(int c = 0; c < bip_cols(bip); c++)
            store[c * rows + rcnt] = bip_a(bip, r, c);

         rcnt++;
         break;
      case GE :
         rhs[rcnt] = -bip_rhs(bip, r);
         
         for(int c = 0; c < bip_cols(bip); c++)
            store[c * rows + rcnt] = -bip_a(bip, r, c);

         rcnt++;
         break;
      case EQ :
         break;
      default :
         assert(0);
      }
   }
   assert(rows >= rcnt);
   assert(rows - rcnt < 4);
   
   for(int c = 0; c < cols; c++)
      pcol[c] = &store[c * rows];

   //printf("%d solutions\n", bip_enumerate(bip, true));

   clock_t            start     = clock();
   unsigned int       solutions = enumerate_basic(bip, cols, rows, equations, pcol, rhs, report_sol);
   unsigned long long count     = 1u << bip_cols(bip);   
   double             elapsed   = GET_SEC(start, clock());

   if (VERB_NORMAL <= verbosity)
   {
      printf("Checked %Ld vectors in %.3f s = %.3f kvecs/s\n",
         count, elapsed, count / elapsed / 1000.0);

      printf("Found %u feasible solutions\n", solutions);
   }
   bip_free(bip);   

   mem_maximum(stdout);
   
   return EXIT_SUCCESS;
}
