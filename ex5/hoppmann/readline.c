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

enum line_t { n_columns, n_rows, constraints };

typedef enum line_t line_type;

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
   line_type mode = n_columns;

   int num_cls = -1;
   int num_rws = -1;

   int row_counter = 0;
   int col_counter = 0;

   int n_scanned;

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

      switch(mode){
          case n_columns  :

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

             if((MAX_LINE_LEN/2) < num_cls)
             {
                fprintf(stderr, "Only matrices with at most %d columns can be processed!\n", MAX_LINE_LEN/2);
                return -1;
             }

             mode++;

             break;
          case n_rows  :

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

             /* now we can initialize the linIeqSystem */

             new_linIeqSys = linIeqSys_new(num_rws, num_cls);

             mode++;

             break;

          case constraints  :

             assert(0 < linIeqSys_nVars(new_linIeqSys));
             assert(0 < linIeqSys_nCons(new_linIeqSys));
             assert(num_cls == linIeqSys_nVars(new_linIeqSys));
             assert(num_rws == linIeqSys_nCons(new_linIeqSys));

             if(row_counter >= num_rws)
             {
                fprintf(stderr, "More than %d rows in the file!\n", num_rws);
                return -1;
             }


#ifdef INT
             int a;
#else
             float a;
#endif
             number n;
             char t_string[3];
             ineq_t ineq_type;

             col_counter = 0;
             bool rel_sign = false;
             bool right_hand_side = false;

             while (*s)
             {
                assert(!isspace(*s));
                assert(NULL != s);

                if('<' == *s || '>' == *s || '=' == *s )
                {
                   assert(!rel_sign);
                   assert(!right_hand_side);

                   if(col_counter != num_cls)
                   {
                      fprintf(stderr, "Wrong format of input file in line %d: number of columns != %d.\n", lines, num_cls);
                      return -1;
                   }

                   n_scanned = sscanf(s, "%s", t_string);

                   if(strcmp ("<=",t_string) == 0)
                   {
                      ineq_type = leq;
                   }
                   else if(strcmp ("=",t_string) == 0)
                   {
                      ineq_type = eq;
                   }
                   else if(strcmp (">=",t_string) == 0)
                   {
                      ineq_type = geq;
                   }
                   else
                   {
                      fprintf(stderr, "Wrong format of input file in line %d: missing <=, >=  or = inequality specifier.\n", lines);
                      return -1;
                   }

                   linIeqSys_setType(new_linIeqSys, row_counter, ineq_type);

                   rel_sign = true;
                }
                else
                {
                   if(col_counter == num_cls && !rel_sign)
                   {
                      fprintf(stderr, "Wrong format of input file in line %d: missing <=, >=  or = inequality specifier or too many columns.\n", lines);
                      return -1;
                   }


#ifdef INT
                   n_scanned = sscanf(s, "%d", &a);
                   n.i  = a;
#else
                   n_scanned = sscanf(s, "%f", &a);
                   n.d  = a;
#endif
                   if(0 == n_scanned)
                   {
                      fprintf(stderr, "Wrong format of input file in line %d in column %d!\n", lines, col_counter+1);
                      return -1;
                   }

                   if(!rel_sign)
                   {
                      linIeqSys_put(new_linIeqSys, row_counter, col_counter, n);

                      col_counter++;
                   }
                   else
                   {
                      linIeqSys_putRHS(new_linIeqSys, row_counter, n);
                      right_hand_side = true;
                   }
                }

                s = strchr(s, ' ');

                if(NULL == s)
                   break;

                while (*s && isspace(*s))
                   s++;
             }

             if(!rel_sign)
             {
                fprintf(stderr, "Wrong format of input file in line %d:  missing <=, >=  or = inequality specifier.\n", lines);
                return -1;
             }

             if(!right_hand_side)
             {
                fprintf(stderr, "Wrong format of input file in line %d:  missing right hand side.\n", lines);
                return -1;
             }
             row_counter++;

             break;

          default :
             fprintf(stderr, "This should not happen. Something went wrong when reading in file!.\n");
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
