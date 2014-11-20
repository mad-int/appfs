/*
 * reader.cpp
 *
 *  Created on: Nov 16, 2014
 *      Author: luke
 */
#include <reader.h>
#define MAX_LINE_LEN   512  // Maximum input line length
#define LIMIT 32            // Maximum column or row size

typedef enum{READ_COLS, READ_ROWS, READ_MATRIX} READ_MODE;

void printFormat(){
	fprintf(stderr, "--------------------------------------------------------------\n"
			"EXPECTED FILE FORMAT:\n\n"
			"# COMMENT\n"
			"'n_columns' #COMMENT\n"
			"'n_rows'    #COMMENT\n"
			"'A[1][1]' 'A[1][2]' ... '=/<=/>=' 'b[1]'\n.\n.\n.\n"
			"--------------------------------------------------------------\n"
			"INTEGER EXAMPLE:\n\n"
			"3 #columns\n"
			"3 #rows\n"
			"1 3 2 <= 5\n"
			"0 1 0  = 1\n"
			"1 0 1 <= 3\n"
			"--------------------------------------------------------------\n"
			"DOUBLE EXAMPLE:\n"
			"# You can as well use integral numbers\n\n"
			"3 #columns\n"
			"3 #rows\n"
			"1.0 3.5 2.7 <= 5.2\n"
			"0   1   0    = 1.3\n"
			"1.2 0   1.2 <= 2.4\n"
			"--------------------------------------------------------------\n\n");
}

int process_file(const char* filename, binPrg& bp)
{
   assert(NULL != filename);
   assert(0    <  strlen(filename));

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;
   int rows = 0, cols;
   READ_MODE mode = READ_COLS;


   if (NULL == (fp = fopen(filename, "r")))
   {
      fprintf(stderr, "Can't open file %s\n", filename);
      goto readError;;
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
       */
       switch(mode){

       case(READ_COLS):

           //check errors in format

           //read columns
		if(!isdigit(*s)){
			fprintf(stderr, "Error in line %d: Expects only digits as input! Found: %c \n",lines, *s);
			goto readError;
		}
           bp.columns = atoi(s);
           if(1 > bp.columns || LIMIT < bp.columns){
                fprintf(stderr, "Error in line %d: cols = %d. Allowed limits: 0 < cols <= %d \n",lines, bp.columns, LIMIT);
                goto readError;
           }
           mode = READ_ROWS;
           break;


        case(READ_ROWS):

           //check errors in format
		if(!isdigit(*s)){
			fprintf(stderr, "Error in line %d: Expects only digits as input! Found: %c \n",lines, *s);
			goto readError;
		}
           //read columns
           bp.rows = atoi(s);
           if(1 > bp.rows || LIMIT < bp.rows){
                fprintf(stderr, "Error in line %d. rows = %d. Allowed limits: 0 < rows <= %d \n",lines, bp.rows, LIMIT);
                goto readError;
           }
           mode = READ_MATRIX;
           break;


        case(READ_MATRIX):

			if(rows>=bp.rows){
				fprintf(stderr, "Error in line: %d. Expected %d rows, "
						"got at least %d rows. Check your file.\n",lines,bp.rows,rows+1);
				goto readError;
			}
            // read row
            for(cols = 0; cols < bp.columns; cols++){

            	bp.A[rows][cols] = strtod(s,&t);

            	if(s == t){
                    fprintf(stderr, "Error in line: %d. Expected %d columns with digits. Got %d columns. \n", lines, bp.columns, cols);
                    goto readError;
                }
                s = t;
            }

            // skip white spaces
            while(isspace(*s)){
                s++;
            }

            if(!strncmp(s, "<=", 2)){
                s+=2;
                bp.b[rows] = strtod(s, &t);
                if(s == t){
                    fprintf(stderr, "Error in line: %d. Expected constraint value. \n", lines);
                    goto readError;
                }
            }
            else if(!strncmp(s, ">=", 2)){
                s+=2;
                bp.multiplyRow(rows,-1);
                bp.b[rows] = -1*strtod(s, &t);
                if(s == t){
                    fprintf(stderr, "Error in line: %d. Expected constraint value. \n", lines);
                    goto readError;
                }
            }
            else if(!strncmp(s,  "=", 1)){
                s+=1;
                bp.b[rows] = strtod(s, &t);
                bp.comparator[rows] = true;
                if(s == t){
                    fprintf(stderr, "Error in line: %d. Expected constraint value. \n", lines);
                    goto readError;
                }
            }
            else {
                fprintf(stderr, "Error in line: %d. Expected =, <= or >=.\n", lines);
                goto readError;
            }
            rows++;
            break;
       }



       }
   fclose(fp);

   return lines;

   readError:
   fprintf(stderr,"Error in file. Terminated.\n");
   printFormat();
   return -1;
}



