/**@file   ex1a.c
 * @brief  Appfs Exercise 1: direct read input.
 * @author Thorsten Koch
 * @date   24Oct2014
 *
 * gcc -O3 -Wall -std=c99 -o ex1 ex1.c
 *
 * Using direct read() for input
 */  
//#include <sys/types.h>
#include <stdio.h>    // printf
//#include <stdlib.h>
#include <stdbool.h>  // bool
#include <limits.h>   // INT_MAX
#include <unistd.h>   // read
#include <fcntl.h>    // O_RDONLY
#include <string.h>   // memset
#include <assert.h>   // assert

#define MAX_NUMS  ((INT_MAX >> 5) + 1) ///< Maximum number of 32 bit ints we need to store bits
#define BUF_NUMS  4096                 ///< read buffer size

/** Program to read a file with binary positive 32 bit integers,
 * sort the numbers and print them sorted.
 * Actually, the numbers are not sorted. There is an array with one
 * bit for each possible number. Upon reading the respective bit
 * is set for the number recognized. At the end the array is scanned
 * in order and the numbers present are printed.
 * @param argv [1] name of file to read
 */
int main(int argc, char** argv)
{
   const char* usage = "usage: %s filename\n";

   /* made static, because otherwise, the stack size might be too small.
    */
   static unsigned int have_num[MAX_NUMS];
   
   int buf[BUF_NUMS];
   int fd;
   int n;
   int total_nums = 0;

   /* we assume 32 bit integers, otherwise it will not work
    */
   assert(sizeof(int) == 4);
   
   /* Check arguments, we need a filename.
    */
   if (argc < 2)
   {
      fprintf(stderr, usage, argv[0]);
      exit(EXIT_FAILURE);
   }
   memset(have_num, 0, sizeof(have_num)); // Proably uneccessary

   /* Open file
    */
   if (0 > (fd = open(argv[1], O_RDONLY)))
   {
      perror(argv[1]);
      exit(EXIT_FAILURE);
   }

   /* Read from file until data exhausted
    */
   while(0 < (n = read(fd, buf, sizeof(buf))))
   {
      int nums_read = n / sizeof(buf[0]); 

      assert(nums_read <= BUF_NUMS);
      
      total_nums += nums_read;

      /* Run through buffer of read numbers and set mark bits
       */
      for(int i = 0; i < nums_read; i++)
      {
         // fprintf(stderr, "%d\n", buf[i]);

         /* Check input: really >= 0 ? Otherwise ignore
          */
         if (buf[i] >= 0)
         {
            int          idx = buf[i] >> 5;         // n / 32
            unsigned int msk = 1 << (buf[i] & 31);  // bit number n mod 32 

            assert(idx >= 0 && idx < MAX_NUMS);
            
            have_num[idx] |= msk;
         }
      }
   }
   if (close(fd))
   {
      perror("close: ");
      exit(EXIT_FAILURE);
   }
   fprintf(stderr, "Total numbers read = %d\n", total_nums);
   
   for(int i = 0; i < MAX_NUMS; i++)
      if (have_num[i])  // just for speed up
         for(int k = 0; k < 31; k++)
            if (have_num[i] & (1 << k))
               printf("%d\n", (i << 5) + k);

   return EXIT_SUCCESS;
}
