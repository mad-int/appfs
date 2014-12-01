/*
 * binaryprog.h
 *
 *  Created on: 16.11.2014
 *      Author: Akram
 */

#ifndef BINARYPROG_H_
#define BINARYPROG_H_
//--------------------
#include <cstdio>
#include <assert.h>
#include <math.h>
//--------------------
#define dtype double
#define LIMIT 32
//--------------------
class binaryprog{
public:
	//create data
	int rows;
	int columns;
	dtype matrix[32][32];
	dtype vector[32];

	//true <-> "=",  false <-> "<="
	bool compSys[32];

	//constructor
	binaryprog();
	binaryprog(int, int);

	//deconstructor
	virtual ~binaryprog();

	//create functions
	bool isLegal();
	void multiplyRow(int, int);
	bool isSolution(const dtype*);
	void printSolution(const int[]);
	void solveProg();

};





#endif /* BINARYPROG_H_ */

