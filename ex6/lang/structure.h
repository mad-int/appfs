/* 
 * File:   structure.h
 * Author: clauslang
 *
 * Created on November 17, 2014, 7:15 PM
 */

#ifndef STRUCTURE_H
#define	STRUCTURE_H

#ifdef USE_DBL
#define D 1
#define NUM double
#else
#define D 0
#define NUM int
#endif

typedef struct {
    int rows;
    int columns;
    NUM** matrix;
    NUM* r;
    char* flag;
} problem;

#endif	/* STRUCTURE_H */

