
#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace

#define MAX_LINE_LEN   512  // Maximum input line length
#define MAX_NUMBERS_LEN 18   // Maximum cipher length of numbers to read



void print_matrix(int **matrix, int numOfRows, int numOfColumns){
  int row, column;
  for (row=0; row<numOfRows; row++)
  {
    for(column=0; column<numOfColumns; column++)
         printf("%d     ", matrix[row][column]);
    printf("\n");
  }
}

void print_vertex(int n, int vars){
  int i;
  for(i = 0; i < vars; i++){
    if(n & (1 << i)) printf("1");
    else printf("0");
  }
  printf("\n");
}


int process_file(const char* filename)
{
   assert(NULL != filename);
   assert(0    <  strlen(filename));

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;
   char  integ[MAX_NUMBERS_LEN];
   int   i;
   int   j;
   
   short foundColumns = 0;
   short foundRows = 0;
   
   int col;
   int row;
   
   int** matrix = (int**) malloc(16*sizeof(int*));
   
   
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
      
      if(foundColumns == 0){
	while(*s != '\0'){
	  if(!isdigit(*s)){
	    s++;
	    continue;
	  }
	  col = next_number(s);
	  foundColumns = 1;
	  break;
        }
      }
      else if(foundRows == 0){
	while(*s != '\0'){
	  if(!isdigit(*s)){
	    s++;
	    continue;
	  }
	  row = next_number(s);
	  foundRows = 1;
	  break;
        }
        printf("last: %s\n", s);
        printf("int*: %d  , int: %d\n", row*sizeof(int*), sizeof(int));
	
	
      
	//matrix = (int**) malloc(sizeof(int*)*row);
	for(i = 0; i < row; i++){
	  matrix[i] = (int*) malloc((col+1)*sizeof(int));
	}
	i = 0;
	j = 0;
	printf("col: %d  , row: %d  , matcol: %d  , matrow: %d\n", col, row, sizeof(matrix[0]), sizeof(matrix));
      }
      else{
	printf("aktuell: %s\n", s);
	while(*s != '\0'){
	  if(!isdigit(*s)){
	    s++;
	    continue;
	  }
	  int tmp = next_number(s);
	  //printf("%d ausgelesen\n", tmp);
	  matrix[i][j] = tmp;
	  //printf("%d geschrieben\n", matrix[i][j]);
	  j++;
	
	  while(isdigit(*s)){
	    s++;
	  }
        }
        i++;
	j = 0;
     }
   }
   
   int count = 0;
   int sum;
   unsigned int vec = 0;
   
   do{
     for(i = 0; i < row; i++){
       sum = 0;
       for(j = 0; j < col; j++){
         if(vec & (1 << j)) sum += matrix[i][j]; 
       }
       /*if(vec == 10){
	 printf("sum = %d\n", sum);
	 printf("sum: %d  , b:%d  , j:%d  , i:%d\n", sum, matrix[i][j], j, i);
	 print_vertex(vec, col);
       }*/
       if(sum > matrix[i][j]) break;
       if(i == row-1){
	 print_vertex(vec, col);
	 count++;
       }
     }
     vec++;
   }while((col == 32 && vec > 0) || !(vec & (1 << (col))));
   
   
   printf("%d von insgesamt %d\n", count, vec);
   
   
   
   
   print_matrix(matrix, row, col+1);
   
   fclose(fp);

   return lines;
}

int next_number(char* s){
  
  
   char  integ[MAX_NUMBERS_LEN];
        
	assert(isdigit(*s));
	  
	int i = 0;
	while(isdigit(*s)){
	  integ[i] = *s;
	  i++;
	  s++;
	}
	integ[i] = '\0';
	s++;
	
	return atoi(integ);
      
}




int main(int argc, char** argv)
{
   if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }
   printf("%d lines\n", process_file(argv[1]));
   
   printf("hier ist noch kein fehler\n");
   
   return EXIT_SUCCESS;
}
