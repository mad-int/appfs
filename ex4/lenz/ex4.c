#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#define MAX_LINE_LEN 512 // Maximum input line length

typedef struct row
{
   int* coefs;
   int rhs;
   char type;
} ROW;

int process_file( const char* filename, ROW*** matrix, int* rows, int* cols )
{
   assert(NULL != filename);
   assert(0 < strlen(filename));

   FILE* fp;
   char buf[MAX_LINE_LEN];
   char* s;
   char* r;
   int entry;
   int rhs;
   int lines = 0;
   int i;
   int j;

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
         *t = '\0'; /* clip comment or newline */
      /* Skip over leading space
      */
      while(isspace(*s))
         s++;
      /* Skip over empty lines */
      if (!*s) /* <=> (*s == '\0') */
         continue;
      /* do processing here */

      if( lines == 1 )
      {
         *cols = strtol(s, NULL, 0);
         /* printf("Column = %d \n", *cols); */
      }
      if( lines == 2 )
      {
         *rows = strtol(s, NULL, 0);
	 /* printf("rows = %d \n", *rows); */
         /* allocate memory and initialize everything, since we know the rows and cols */ 
         *matrix = (ROW **)malloc(*rows * sizeof(ROW *));
         if(*matrix == NULL)
         {
            fprintf(stderr, "out of memory\n");
            exit(0);
         }
         for(i = 0; i < *rows; i++)
         {
            /* printf("allocating row %i\n", i); */
            (*matrix)[i] = (ROW *)malloc(*rows * sizeof(ROW));
            if((*matrix)[i] == NULL)
            {
               fprintf(stderr, "out of memory\n");
               exit(0);
            }
            /* (*matrix)[i]->type = 'I';
            (*matrix)[i]->rhs = '-1000'; */
            (*matrix)[i]->coefs = (int *)malloc(*cols * sizeof(int));
            if((*matrix)[i]->coefs == NULL)
            {
               fprintf(stderr, "out of memory\n");
               exit(0);
            }
         }
      }
      if( lines > 2 )
      {
         /* get columns */ 
         for( j = 0; j < *cols; j++ )
         {
            while(isspace(*s))
               s++;
	    
            r = s;
            while(!isspace(*r))
               r++;

            entry = strtol(s, &r, 0);
	    /* printf("read entry: %d\n", entry); */

            /* store it in matrix */ 
            (*matrix)[lines-3]->coefs[j] = entry;
            s = r;
         }
         /* get type of inequality */
         while(isspace(*s))
            s++;
         switch(*s)
         {
            case '=':
               /* printf("equality row\n"); */
               (*matrix)[lines-3]->type = 'E';
               assert( *(s+1) == '=' );
               break;
            case '<':
	       /* printf("less row\n"); */
               (*matrix)[lines-3]->type = 'L';
               assert( *(s+1) == '=' );
               break;
            case '>':
	       /* printf("larger row\n"); */
               (*matrix)[lines-3]->type = 'R';
               assert( *(s+1) == '=' );
               break;
         }
         /* get rhs */
         s += 2;
         rhs = strtol(s, NULL, 0);
         /* printf("rhs is %d\n", rhs); */
         (*matrix)[lines-3]->rhs = rhs;
      }
   }
   fclose(fp);
   return lines;
}
void print_matrix( ROW** matrix, int nrows, int ncols )
{
   int i;
   int j;
   printf("Printing matrix of %d rows and %d cols\n", nrows, ncols);
   for( i = 0; i < nrows; i++ )
   {
      ROW* row = matrix[i];
      for( j = 0; j < ncols; j++ )
      {
         printf("%+d ", row->coefs[j]);
      }
      switch( row->type )
      {
         case 'E':
            printf(" == %d\n", row->rhs);
            break;
         case 'L':
            printf(" <= %d\n", row->rhs);
            break;
         case 'R':
            printf(" >= %d\n", row->rhs);
            break;
         default:
            fprintf(stderr,"ERROR\n");
            exit(-1);
            break;
      }
   }
}
/*
    say we have 0000,
    next sol is 1000,
    next sol is 0100,
    next sol is 1100,
    next sol is 0010,
    ...
    say we have 1111,
    next sol is return false
*/
int next_sol(int* sol, int ncols)
{
   int i;
   for( i = 0; i < ncols; i++ )
   {
      if( sol[i] == 0 )
      {
         sol[i] = 1;
         return 1; /* return true, i.e there is another solution */
      }
      else
         sol[i] = 0;
   }
   return 0; /* there is no further point */
}
int test_sol(int* sol,ROW** matrix,int nrows,int ncols)
{
   int lhs;
   int i;
   int j;
   for( i = 0; i < nrows; i++ )
   {
      ROW* row;
      row = matrix[i];
      /* compute lhs */
      lhs = 0;
      for( j = 0; j < ncols; j++ )
         lhs += row->coefs[j] * sol[j];
      switch( row->type )
      {
         case 'E':
            if( lhs != row->rhs )
               return 0;
            break;
         case 'L':
            if( lhs > row->rhs )
               return 0;
            break;
         case 'R':
            if( lhs < row->rhs )
               return 0;
            break;
         default:
            fprintf(stderr,"ERROR\n");
            exit(-1);
            break;
      }
   }
   return 1;
}

void find_binary_solutions( ROW** matrix, int nrows, int ncols )
{
   int sol[ncols];
   int i;
   /* start with the zero solution*/
   for( i = 0; i < ncols; i++ )
      sol[i] = 0;
   do
   {
      if( test_sol(sol, matrix, nrows, ncols) )
      {
         printf("sol: ");
         for( i = 0; i < ncols; i++ )
            printf("%d ", sol[i]);
         printf("\n");
      }
   }while(next_sol(sol, ncols));
} 

int main(int argc, char** argv)
{
   int** A;
   int* b;
   int rows;
   int cols;
   ROW** matrix;
   if (argc < 2 || strlen(argv[1]) <= 0)
   {
      fprintf(stderr, "usage: %s filename", argv[0]);
      return EXIT_FAILURE;
   }
   process_file(argv[1], &matrix, &rows, &cols);
   /* print_matrix(matrix, rows, cols); */
   find_binary_solutions(matrix, rows, cols);
   return EXIT_SUCCESS;
}