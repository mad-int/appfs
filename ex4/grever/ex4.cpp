/*
 * ex4.cpp
 *
 *  Created on: Nov 11, 2014
 *      Author: andreas
 *
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>  // strpbrk
#include <ctype.h>   // isspace
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define MAX_LINE_LEN   512  // Maximum input line length

#ifdef USE_MSHELL
#include "mshell.h"
#endif



void* allocate(int elems, int size)
{
   void* p;

   assert(elems > 0);
   assert(size  > 0);

   if (NULL == (p = calloc((size_t)elems, (size_t)size)))
   {
      perror("allocate");
      exit(EXIT_FAILURE);
   }
   assert(p != NULL);

   return p;
}


void deallocate(void* p)
{
   assert(p != NULL);

   free(p);
}


/** Read a textfile, remove comments and process lines.
 * @param filename name of file to read, int* A, int* b, int* col, int* row
 * @return number of lines read
 */
int process_file(const char* filename, int* A,int* b,int* col,int* row)
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

      if(lines==1){*col=atoi(s);}	//first line # of columns
      if(lines==2){*row=atoi(s);}	//second line # of rows

      if(lines>2)
      {
    	  for(int i=0; i<*col;i++)
    	  {
    		  A[i+(lines-3)*(*col)]=atoi(s+2*i); //whitespace between values
    	  }
    	  b[(lines-3)]=atoi(s+(2*(*col)+3));
      }
   }

   fclose(fp);
   return lines;
}


void lin_ineq(int* A,int* b,int col,int row)
{
	ofstream sol;
	sol.open("solutions.txt",ofstream::out);
	assert( sol.is_open());
	int* x=static_cast<int*>(allocate((col), sizeof(x[0])));

	//for col-dim x in {0,1} there are 2^col possibilities
	//so we turn the corresponding integers into arrays
	//consisting of their binary presentation
	for(int i=0; i<pow(2,col); i++)
	{
		int tmp=i;
		for(int j=0; j<col; j++)
		{
			if(tmp==0) {x[j]=0;}
			else
			{
				x[j]=tmp%2;
				tmp/=2;
			}
		}

		//checking all the inequalities and printing the x
		//for which they are fulfilled
		int le=1;
		int j=0;
		while(le!=0 && j<row)
		{
			int sum=0;
			for(int k=0;k<col;k++)
			{
				sum=sum+A[k+j*col]*x[k];
			}
			if(sum>b[j]){le=0;}
			j++;
		}

		if(le==1)
		{
			for(int k=0;k<col;k++)
			{
				sol<<x[k]<<"  ";
			}
			sol.put('\n');

		}

	}
	sol.close();
	delete[] x;
	return;
}



int main(int argc, char** argv)
{
   if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }


   int* A;
   int* b;
   int col;
   int row;
   A=static_cast<int*>(allocate(32*32, sizeof(A[0])));
   b=static_cast<int*>(allocate((32), sizeof(b[0])));


   //reads the numbers from a file and writes them into A,b,col and row.
   printf("%d lines\n", process_file(argv[1], A,b,&col,&row));

   //computes all solutions x of the problem Ax<=b,
   //where A is a (row x col) matrix and b a vector of dim row
   //into a file named "solutions.txt

   lin_ineq(A,b,col,row);

   deallocate(A);
   deallocate(b);
   return EXIT_SUCCESS;
}
