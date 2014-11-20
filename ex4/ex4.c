//
//  main.c
//  ex4
//
//  Created by Leon Eifler on 09/11/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>   // assert
#include "instance.h"
#include "permute.h"
#define MAX_LINE_LEN   512  // Maximum input line length


//increment a pointer to the next withespace character. return NULL if no such character is present.
bool is_number( char * point){
    if(NULL == point){
       return false;
    }
    if (atoi(point)==0 && *point!='0') {
        printf("Unerlaubter char %c ist keine Zahl, \n", *point);
        return false;
    }
    return true;
}

char* next_number(char* point){
    char* out=point;
    while(isspace(*out))
        out++;
    out=strpbrk(out, " ");
    if (out==NULL) {
        return NULL;
    }
    while (isspace(*out)) {
        ++out;
    }
    return out;
}

type get_number(char*point){
#ifdef DOUBLE
    return atof(point);
#else
    return atoi(point);
#endif
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
                }else if (lines>inst.rows+2){
                    printf("Es sind %d nichtleere Zeilen in der Eingabedatei. Das Problem sollte aber nur %d Zeilen haben. ", lines-2, inst.rows);
                    goto wrong_input;
                }else{
                    
                    for (int i=0; i<inst.columns; ++i) {
                        //assign current element to matrix
                        while (isspace(*current_num)) {
                            ++current_num;
                        }
                        if (!is_number(current_num)) {
                            goto wrong_input;
                        }
                        set_elem(inst, lines-3, i, get_number(current_num));
                        //printf("%d \t", *(get_elem(inst, lines-3, i)));
                        if (i==inst.columns-1) continue;
                        if(NULL==(current_num=next_number(current_num)))goto wrong_input;
                    }
                    
                    if(NULL==(current_num=strpbrk(current_num, "<>="))) goto wrong_input;
                    if (*current_num=='<'&&*(current_num+1)=='=' ) {
                        if((non_def==inst.ordin || smallereq==inst.ordin) && is_number(current_num+2))inst.ordin=smallereq;
                        else goto wrong_input;
                    }else if (*current_num=='>'&&*(current_num+1)=='=' ){
                        if((non_def==inst.ordin || greatereq==inst.ordin) && is_number(current_num+2))inst.ordin=2;
                        else goto wrong_input;
                    }else if (*current_num=='=' ){
                        if((non_def==inst.ordin || eq==inst.ordin )&& is_number(current_num+1))inst.ordin=3;
                        else goto wrong_input;
                    }
                    if(NULL==(current_num=next_number(current_num)))goto wrong_input;
                    if (!is_number(current_num)) goto wrong_input;
                    inst.vector[lines-3]=get_number(current_num);
                    //printf(" <= %d \n", inst.vector[lines-3]);
                    current_num=next_number(current_num);
                    if (NULL != current_num && 0 != *current_num ) {
                        goto wrong_input;
                    }
                }
                
                
                
            }
     if (lines-2!=inst.rows)goto wrong_input;
     print_problem(inst);
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
