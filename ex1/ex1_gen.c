/* TK 18Sep2014
 *
 * gcc -O2 -o ex1_gen -static -m32 ex1_gen.c 
 */
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
   const char* usage = "usage: %s count\n";

   int n;
   int i;
   int r;
   
   /* Check arguments, we need a positive number
    */
   if (argc < 2 || atoi(argv[1]) < 1)
   {
      printf(usage, argv[0]);
      exit(-1);
   }
   n = atoi(argv[1]);
   
   srand(130267);
      
   for(i = 0; i < n; i++)
   {
      r = rand();
      
      if (0 != i && 0 == (i % 1000037))
         r *= -1;
      
      // fprintf(stderr, "%d\n", r);
      printf("%d %u ", (int) r, (unsigned int) r);
//      fwrite(&r, sizeof(r), 1, stdout);
   }
   return 0;
}
