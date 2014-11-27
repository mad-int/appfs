/*
 * BinaryProg.h
 *
 * Created on: 23.11.2014
 *      Author: André Greiner-Petter
 */

#ifndef BINARYPROGRAM_H_
#define BINARYPROGRAM_H_

// define max row and cols
#define BIP_ROWS 32
#define BIP_COLS 32

#define MAX_LINE_LENGTH 1024
#define MAX_COEF_VAL	1e20

#define FALSE	0
#define TRUE	1

// types of constraints
enum equalityType { EQUAL, LEQ, GEQ, LESS, GREATER, NONE };

// define binary program
struct binaryProg {
	int cols;
	int rows;
	double a[BIP_ROWS][BIP_COLS];
	double b[BIP_ROWS];
	int equalityType[BIP_ROWS];
};

// use BIP for it
typedef struct binaryProg BIP;

extern void storeMeta( char buf[], FILE* fp, int* storage );
extern BIP* read_BinaryProgram( const char* filename);

#endif /* BINARYPROGRAM_H_ */
