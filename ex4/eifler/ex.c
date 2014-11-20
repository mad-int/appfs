//
//  main.c
//  ex4
//
//  Created by Leon Eifler on 09/11/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>   // assert
#include "instance.h"
#include "permute.h"
#define MAX_LINE_LEN   512  // Maximum input line length


//increment a pointer to the next withespace character. return NULL if no such character is present.
char* next_number(char* point){
    char* out=point;
    while(isspace(*out))
        out++;
    out=strpbrk(out, " ");
    if (out==NULL) {
        printf("Eingabe entspricht nicht der gewünschten Formatierung");
        return NULL;
    }
    return out;
}

//read the file and get all solutions for the system of inequalities

 int process_file(const char* filename)
 {
        assert(NULL != filename);
        assert(0    <  strlen(filename));
    
        FILE* fp;
        char  buf[MAX_LINE_LEN];
        char* current_num;
        int   lines = 0;
        problem inst;
    
        if (NULL == (fp = fopen(filename, "r")))
            {
                   fprintf(stderr, "Can't open file %s\n", filename);
                   return -1;
                }
     printf("Die Eingegebene Problemstellung ist: \n");
        while(NULL != (current_num = fgets(buf, sizeof(buf), fp)))
            {
                   char* t = strpbrk(current_num, "#\n\r");
            
                   lines++;
            
                   if (NULL != t) /* else line is not terminated or too long */
                          *t = '\0';  /* clip comment or newline */
            
                   /* Skip over leading space
                              */
                   while(isspace(*current_num))
                          current_num++;
            
                   /* Skip over empty lines
                              */
                if (!*current_num){
                    lines--;
                    continue;
                }/* <=> (*s == '\0') */
                
             
                   /* do processing here
                              */
                //get number of columns
                int rows, cols;
                if (0==cols) {
                    cols=atoi(current_num);
                    if(cols<=0){printf("Ungültige Spaltenanzahl: %d \n", cols); goto wrong_input;}
                //get number of rows, initialize problem
                }else if (0==rows){
                    rows=atoi(current_num);
                    if(rows<=0 ){printf("Ungülige Zeilenanzahl: %d \n", rows); goto wrong_input;}
                    inst = init_problem(rows, cols);
                }
                if (lines>inst.rows+2) {
                    goto wrong_input;
                }
                
                //fill the matrix and the vector
                if (2<lines) {
                    for (int i=0; i<inst.columns; ++i) {
//                        //assign current element to matrix
                        set_elem(inst, lines-3, i, atoi(current_num));
                        printf("%d \t", *(get_elem(inst, lines-3, i)));
                        current_num=next_number(current_num);
                        if(current_num==NULL)goto wrong_input;

//
                        
                    }
                    
                    current_num=next_number(current_num);
                    if(current_num==NULL)goto wrong_input;
                    inst.vector[lines-3]=atoi(current_num);
                    printf(" <= %d \n", inst.vector[lines-3]);
                }
                
            }
     if (lines-2!=inst.rows)goto wrong_input;
     printf("Die Lösungen des Problems sind: \n");
     find_solutions(inst);
     fclose(fp);
     free_problem(inst);
     return lines;
     
wrong_input:
     printf("Die Eingabe entspricht nicht der gewünschten Formatierung. Bitte Eingabe prüfen. \n");
     free_problem(inst);
     return lines;
     }


int main(int argc, const char * argv[]) {
   
    const char* usage = "usage: %s filename\n";
    
   if (argc < 2)
           {
                   fprintf(stderr, usage, argv[0]);
                   exit(EXIT_FAILURE);

           }
    
    
    printf("%d nichtleere Zeilen in der Quelldatei.\n", process_file(argv[1]));
  
}
