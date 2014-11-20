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
typedef struct {
    int rows;
    int columns;
    double** matrix;
    double* r;
    char* flag;
} problem;
#else
#define D 0
typedef struct {
    int rows;
    int columns;
    int** matrix;
    int* r;
    char* flag;
} problem;
#endif

#endif	/* STRUCTURE_H */

