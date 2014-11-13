#include <iostream>
#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include <math.h>    // pow
#include <vector>
#include <Matrix.h>


#define MAX_LINE_LEN   512  // Maximum input line length



using namespace std;

void dec2bin(int, Vector&);
int process_file(const char* filename);

void printVector(Vector& x){
    for(int i = 0; i < x.dimension;i++)
        cout << x.getValue(i) << " ";
    cout << endl;
}

int main(int argc, char** argv){

    if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }
    /*  Have to initialize objects already, otherwise get an error due to rvalue,
        how can I avoid that?
    */

    int maxSolutions = pow(2,columns);
    for(int k = 0;k < maxSolutions;k++){
        Vector x(columns);
        dec2bin(k,x);
        Vector sum = matrix.multiply(x);
        if(sum.lessOrEqual(b))
            printVector(x);
    }

    return 0;
}

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

      /* Process binary program here
	  Don't know how to touch it. Tried giving matrix and vector as reference,
	  but it wouldn't store the data I gave into it permanently, meaning,
	  as soon as it left the function, matrix.columns = 0 again.
       */
	}
   fclose(fp);

   return lines;
}

void dec2bin(int decimal, Vector& binary){
    assert(decimal>=0);
    int counter = binary.dimension-1;
    while(decimal){
        binary.setValue(counter,decimal % 2);
        decimal = decimal / 2;
        counter--;
    }
}
