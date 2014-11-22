/*
 * readin.cpp
 *
 *  Created on: 16.11.2014
 *      Author: Akram
 */

#include "readin.h"
#define MAX_LENGTH 512
#define LIMIT 32

typedef enum{R_COLS, R_ROWS, R_MATRIX} R_MODE;

int process_file(const char* filename, binaryprog& bipo){
	assert(NULL != filename);
	assert(0    <  strlen(filename));

	FILE* fp;
	char  buf[MAX_LENGTH];
	char* s;
	int   lines = 0;
	int rows = 0, cols;
	R_MODE mode = R_COLS;

	if(NULL == (fp = fopen(filename, "r"))){
		fprintf(stderr, "Can't open file %s\n", filename);
		goto readError;
	}

	while(NULL != (s = fgets(buf, sizeof(buf), fp))){
		char* t = strpbrk(s, "#\n\r");

		lines++;

		if(NULL != t)
			*t = '\0';

		while(isspace(*s))
			s++;

		if(!*s)
			continue;

		switch(mode){
			case(R_COLS):
				//see if *s is digit
				if(!isdigit(*s)){ //<-> isalpha(*s)
					fprintf(stderr, "There is an error in line %d: Only digits, no letters %c!", lines, *s);
					goto readError;
				}
				bipo.columns = atoi(s); //ascii to integer
				if(bipo.columns < 1 || LIMIT < bipo.columns){
					fprintf(stderr, "There is an error in line %d: cols must be between zero and %d.\n", lines, LIMIT);
					goto readError;
				}
				mode = R_ROWS;
				break;

			case(R_ROWS):
				if(!isdigit(*s)){ //<-> isalpha(*s)
					fprintf(stderr, "There is an error in line %d: Only digits, no letters %c!", lines, *s);
					goto readError;
				}
				bipo.rows = atoi(s);
				if(bipo.rows < 1 || LIMIT < bipo.rows){
					fprintf(stderr, "There is an error in line %d: rows must be between zero and %d.\n", lines, LIMIT);
					goto readError;
				}
				mode = R_MATRIX;
				break;

			case(R_MATRIX):
				if(rows >= bipo.rows){
					fprintf(stderr, "There is an error in line %d: Expected %d rows.\n", lines, bipo.rows);
					goto readError;
				}
				for(cols = 0; cols < bipo.columns; cols++){
					bipo.matrix[rows][cols] = strtod(s, &t); //string to double
					if(s == t){
						fprintf(stderr, "There is an error in line %d: Expected %d columns.\n", lines, bipo.columns);
						goto readError;
					}
					s = t;
				}

				while(isspace(*s))
					s++;

				if(!strncmp(s, "<=", 2)){
					s += 2;
					bipo.vector[rows] = strtod(s, &t);
					if(s == t){
						fprintf(stderr, "There is an error in line %d: Expected legal value.\n", lines);
						goto readError;
					}
				}else if(!strncmp(s, ">=", 2)){
					s += 2;
					bipo.multiplyRow(rows, -1);
					bipo.vector[rows] = -1*strtod(s, &t);
					if(s == t){
						fprintf(stderr, "There is an error in line %d: Expected legal value.\n", lines);
						goto readError;
					}
				}else if(!strncmp(s, "=", 1)){
					s += 1;
					bipo.vector[rows] = strtod(s, &t);
					bipo.compSys[rows] = true;
					if(s == t){
						fprintf(stderr, "There is an error in line %d: Expected legal value.\n", lines);
						goto readError;
					}
				}else{
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
	fprintf(stderr, "Error! Look up your File.\n");
	return -1;
}


