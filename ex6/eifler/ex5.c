//
//  main.c
//  ex4
//
//  Created by Leon Eifler on 09/11/14.
//  Copyright (c) 2014 leonoel. All rights reserved.
//

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>   // assert
#include "instance.h"
#define MAX_LINE_LEN   512  // Maximum input line length

/* return the double representation if so-defined. Otherwise return the integer representation of the char */
type get_number(char*point){
#ifdef DOUBLE
    return atof(point);
#else
    return atoi(point);
#endif
}


/** check whether the next non-whitespace entry is a number, print error message if desired */
bool is_number( char * point, bool printerror){
    if (NULL==point) {
        return false;
    }
    char * temp = point;
    while (isspace(*temp)) {
        ++temp;
    }
    if (get_number(temp)==(type)0 && *temp!='0') {
        if (printerror) {
            printf("Unerlaubter char %c ist keine Zahl, \n", *temp);
        }
        return false;
    }
    return true;
}

/*increment the pointer until the next withespace-character is found */
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


//read the file and get all solutions for the system of inequalities

 int process_file(const char* filename, bool with_print)
 {
        assert(NULL != filename);
        assert(0    <  strlen(filename));
    
        FILE* fp;
        char  buf[MAX_LINE_LEN];
        char* current_num;
        int   lines = 0;
        int rel_lines=0;
        problem inst;
        int rows=0, cols=0;

        if (NULL == (fp = fopen(filename, "r")))
            {
                   fprintf(stderr, "Can't open file %s\n", filename);
                   return -1;
                }
     
        while(NULL != (current_num = fgets(buf, sizeof(buf), fp)))
            {
                   char* t = strpbrk(current_num, "#\n\r");
            
                   lines++;
                   rel_lines++;
            
                   if (NULL != t) /* else line is not terminated or too long */
                          *t = '\0';  /* clip comment or newline */
            
                   /* Skip over leading space
                              */
                   while(isspace(*current_num))
                          current_num++;
            
                   /* Skip over empty lines
                              */
                if (!*current_num){
                    rel_lines--;
                    continue;
                }/* <=> (*s == '\0') */
                
             
                   /* do processing here
                              */
                //get number of columns
                while (!is_number(current_num, 0)) {
                    ++current_num;
                }
                if (0==cols) {
                    cols=atoi(current_num);
                    if(cols<=0){printf("Ungültige Spaltenanzahl: %d \n", cols); goto wrong_input;}
                //get number of rows, initialize problem
                }else if (0==rows){
                    rows=atoi(current_num);
                    if(rows<=0 ){printf("Ungülige Zeilenanzahl: %d \n", rows); goto wrong_input;}
                    inst = init_problem(rows, cols);
                }else if (rel_lines>inst.rows+2){
                    printf("Es sind %d Parameter-Zeilen in der Eingabedatei. Das Problem sollte aber nur %d Zeilen haben. ", rel_lines-2, inst.rows);
                    goto wrong_input;
                }else{
                    
                    for (int i=0; i<inst.columns; ++i) {
                        //assign current element to matrix
                        while (isspace(*current_num)) {
                            ++current_num;
                        }
                        if (!is_number(current_num, true)) {
                            goto wrong_input;
                        }
                        set_elem(inst, rel_lines-3, i, get_number(current_num));
                        //printf("%d \t", *(get_elem(inst, lines-3, i)));
                        if (i==inst.columns-1){
                            if (NULL==next_number(current_num) || is_number(next_number(current_num),false))  {
                                goto wrong_input;
                            }
                            continue;
                        }
                        if(NULL==(current_num=next_number(current_num)))goto wrong_input;
                    }
                    
                    if(NULL==(current_num=strpbrk(current_num, "<>="))) goto wrong_input;
                    if (*current_num=='<'&&*(current_num+1)=='=' ) {
                        if((non_def==inst.ordin || smallereq==inst.ordin) && is_number(current_num+2,true))inst.ordin=smallereq;
                        else goto wrong_input;
                    }else if (*current_num=='>'&&*(current_num+1)=='=' ){
                        if((non_def==inst.ordin || greatereq==inst.ordin) && is_number(current_num+2,true))inst.ordin=2;
                        else goto wrong_input;
                    }else if (*current_num=='=' ){
                        if((non_def==inst.ordin || eq==inst.ordin )&& is_number(current_num+1,true))inst.ordin=3;
                        else goto wrong_input;
                    }
                    if(NULL==(current_num=next_number(current_num)))goto wrong_input;
                    if (!is_number(current_num,true)) goto wrong_input;
                    
                    inst.vector[rel_lines-3]=get_number(current_num);
                    
                    current_num=next_number(current_num);
                    if (NULL != current_num && 0 != *current_num ) {
                        printf("Unerlaubtes Zeichen nach RHs: %c \n", *current_num);
                        goto wrong_input;
                    }
                }
                
                
                
            }
     if (rel_lines-2!=inst.rows)goto wrong_input;
     if(with_print)print_problem(inst);
     long long sols=find_solutions(inst, with_print);
     printf("Es wurden %lld Lösungen gefunden. \n", sols);
     fclose(fp);
     free_problem(inst);
     return lines;
     
wrong_input:
     printf("Die Eingabe entspricht nicht der gewünschten Formatierung. Bitte Eingabe prüfen. \n");
     free_problem(inst);
     return -1;
     }


int main(int argc, const char * argv[]) {
   
    const char* usage = "usage: %s filename\n";
    
   if (argc < 2)
           {
                   fprintf(stderr, usage, argv[0]);
                   exit(EXIT_FAILURE);

           }
    bool with_print=0;
    if (argc > 2) {
        if (strcmp(argv[2], "print_info")==0) {
            with_print=1;
        }
    }
    int res=process_file(argv[1], with_print);
    if (res==-1) {
        printf("Abbruch des Programms.");
    }else printf("Erfolgreicher Durchlauf. %d Zeilen wurden gelesen. \n", res);
  
}
