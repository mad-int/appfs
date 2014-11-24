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
   }
   fclose(fp);

   return lines;
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
