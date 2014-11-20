/**@file   ex4_readline.c
 * @brief  Appfs Example: line wise text input.
 * @author Thorsten Koch
 * @date   07Nov2014
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
#include <stdbool.h> // bool

#include "lin_ieq_system.h"

#define MAX_LINE_LEN    512   // Maximum input line length

/** Read a textfile, remove comments and process lines.
 * @param filename name of file to read
 * @return number of lines read
 */
int process_file(const char* filename, LinIeqSys** linIeqSys)
{
   assert(NULL != filename);
   assert(NULL != linIeqSys);
   assert(0    <  strlen(filename));

   LinIeqSys* new_linIeqSys;

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;

   int num_cls = -1;
   int num_rws = -1;

   int row_counter = 0;
   int col_counter = 0;

   if (NULL == (fp = fopen(filename, "r")))
   {
      fprintf(stderr, "Can't open file %s\n", filename);
      return -1;
   }
   while(NULL != (s = fgets(buf, sizeof(buf), fp)))
   {
      char* t = strpbrk(s, "#\n\r");

      lines++;

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
      if(1 == lines || 2 == lines)
      {
         int n_scanned;

         if(1 == lines)
         {
            /* read the first number in this line
             */

            n_scanned = sscanf (s,"%d", &num_cls);

            if(0 == n_scanned)
            {
               fprintf(stderr, "Wrong format of input file in line %d: missing number of columns!\n", lines);
               return -1;
            }

            if(0 >= num_cls)
            {
               fprintf(stderr, "%d number of columns makes no sense!\n", num_cls);
               return -1;
            }

            if((MAX_LINE_LEN/2 - 4) < num_cls)
            {
               fprintf(stderr, "Only matrices with at most %d columns can be processed!\n", MAX_LINE_LEN/2 - 4);
               return -1;
            }

         }
         else
         {

            /* read the first number in this line
             */
            n_scanned = sscanf (s,"%d", &num_rws);

            if(0 == n_scanned)
            {
               fprintf(stderr, "Wrong format of input file in line %d: missing number of rows!\n", lines);
               return -1;
            }

            if(0 >= num_rws)
            {
               fprintf(stderr, "%d number of rows makes no sense!\n", num_rws);
               return -1;
            }

            new_linIeqSys = linIeqSys_new(num_rws, num_cls);
         }

         continue;
      }
      else
      {

         assert(0 < linIeqSys_nVars(new_linIeqSys));
         assert(0 < linIeqSys_nCons(new_linIeqSys));
         assert(num_cls == linIeqSys_nVars(new_linIeqSys));
         assert(num_rws == linIeqSys_nCons(new_linIeqSys));

         /*
          * First find out, if this inequality is of type <= or >=
          */

         t =  strpbrk(s, "<>");
         if(NULL == t)
         {
            fprintf(stderr, "Wrong format of input file in line %d: missing <= or >= inequality specifier.\n", lines);
            return -1;
         }

         if('=' != *(t+1))
         {
            fprintf(stderr, "Wrong format of input file in line %d: only inequalities of type <= or >= allowed.\n", lines);
            return -1;
         }

         bool reverse_ineq = ('>' == *t);

         float a;

         col_counter = 0;
         bool right_hand_side = false;

         while (*s)
         {
            assert(!isspace(*s));
            assert(NULL != s);

            if('<' == *s || '>' == *s)
            {
               assert(!right_hand_side);

               right_hand_side = true;
            }
            else
            {
               sscanf(s, "%f", &a);

               if(reverse_ineq)
                  a = -a;

               if(!right_hand_side)
               {
                  linIeqSys_put(new_linIeqSys, row_counter, col_counter, a);

                  col_counter++;
               }
               else
                  linIeqSys_putRHS(new_linIeqSys, row_counter, a);
            }

            s = strchr(s, ' ');

            if(NULL == s)
               break;

            while (*s && isspace(*s))
               s++;
         }

         if(col_counter != num_cls)
         {
            fprintf(stderr, "Wrong format of input file in line %d: number of columns != %d.\n", lines, num_cls);
            return -1;
         }

         row_counter++;

      }
   }
   fclose(fp);

   if(row_counter != num_rws)
   {
      fprintf(stderr, "Wrong format of input file in line %d: number of rows != %d.\n", lines, num_rws);
      return -1;
   }

   *linIeqSys = new_linIeqSys;

   assert(NULL != linIeqSys);

   return lines;
}
