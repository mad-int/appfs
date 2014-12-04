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
type get_number(char**point){
    assert(NULL!=point);
    type temp=0;
#ifdef DOUBLE
    temp= strtod(*point, point);
#else
    temp = (int)strtol(*point, point, 0);
#endif
    return temp;
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
        bip inst;
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
                
                if (0==cols) {

                    cols=(int)strtol(current_num, &current_num, 0);
                    while (isspace(*current_num)) {
                        current_num++;
                    }
                    if (*current_num!=' '&&*current_num!='\0')goto wrong_input;
                    if(cols<=0){printf("Invalid number of columns: %d \n", cols); goto wrong_input;}
                //get number of rows, initialize bip
                }else if (0==rows){
                    rows=(int)strtol(current_num, &current_num, 0);
                    while (isspace(*current_num)) {
                        current_num++;
                    }
                    if (*current_num!=' '&&*current_num!='\0')goto wrong_input;
                    if(rows<=0 ){printf("Invalid number of lines: %d \n", rows); goto wrong_input;}
                    inst = init_bip(rows, cols);
                }else if (rel_lines>inst.rows+2){
                    printf("There are %d rows of constraints in the input file. The specified number of lines is %d. ", rel_lines-2, inst.rows);
                    goto wrong_input;
                }else{
                    
                    for (int i=0; i<inst.columns; ++i) {
                        //assign current element to matrix
                      while (isspace(*current_num)) {
                           ++current_num;
                       }

                        
                        set_elem(inst, rel_lines-3, i, get_number(&current_num));
                        if (*current_num!=' ')goto wrong_input;

                    }
                    if (0!=get_number(&current_num)) {
                        goto wrong_input;
                    }
                    if(NULL==(current_num=strpbrk(current_num, "<>="))) goto wrong_input;
                    if (*current_num=='<'&&*(current_num+1)=='=' ) {
                        if((non_def==inst.ordin || smallereq==inst.ordin))inst.ordin=smallereq;
                        else goto wrong_input;
                    }else if (*current_num=='>'&&*(current_num+1)=='=' ){
                        if((non_def==inst.ordin || greatereq==inst.ordin))inst.ordin=2;
                        else goto wrong_input;
                    }else if (*current_num=='=' ){
                        if((non_def==inst.ordin || eq==inst.ordin ))inst.ordin=3;
                        else goto wrong_input;
                    }
                        
                    while (*current_num=='<' || *current_num=='>' ||*current_num=='=' || isspace(*current_num)) {
                        current_num++;
                    }
                    if ('\0'==*current_num) {
                        printf("Missing RHs."); goto wrong_input;
                    }
                    inst.vector[rel_lines-3]=get_number(&current_num);
                    while (isspace(*current_num)) {
                        current_num++;
                    }
                    if (*current_num!=' '&&*current_num!='\0')goto wrong_input;

                }
                
                
                
            }
     if (rel_lines-2!=inst.rows)goto wrong_input;
     if(with_print)print_bip(inst);
     long long sols=find_solutions(inst, with_print);
     if (0>sols)return -1;
     printf("There were %lld feasible binary vectors found. \n", sols);
     fclose(fp);
     free_bip(inst);
     return lines;
     
wrong_input:
     fprintf(stderr, "Error in line %d. The input file was not formatted correctly. Please check the input file. \n", lines);
     free_bip(inst);
     return -1;
     }


int main(int argc, const char * argv[]) {
   
    const char* usage = "usage: %s filename\n";
    
   if (argc < 2)
           {
                   fprintf(stderr, usage, argv[0]);
                   exit(EXIT_FAILURE);

           }
    
    printf("Opening file: %s \n", argv[1]);
#ifdef DOUBLE
    printf("Compile-mode is double. \n ");
#endif
    
    bool with_print=0;
    if (argc > 2) {
        if (strcmp(argv[2], "print_info")==0) {
            with_print=1;
        }
    }
    int res=process_file(argv[1], with_print);
    if (res==-1) {
        printf("Terminating the program. \n");
    }
    printf("-------------------------- \n");
  
}
