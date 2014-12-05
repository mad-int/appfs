/*
 * binPrg.h
 *
 *  Created on: 30.11.2014
 *      Author: Luke
 */

#ifndef EX6_SRC_BINPRG_H_
#define EX6_SRC_BINPRG_H_

#include <cstdio>
#include <assert.h>
#include <math.h>
#include <time.h>
#define LIMIT 32
#define EPSILON (1E-8)
#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC) // returns time in seconds


class binPrg
{

public:
	//members
	int rows;
	int columns;
	double A[32][32];
	double b[32];
	bool comparator[32];

	//constructor
	binPrg();

	//destructor
	~binPrg();

	//functions
	bool isValid();
	void multiplyRow(int,int);

	void dec2bin(int,long,int[]);
	bool isFeasible(const double[]);
	void printFeasibleToFile(const int[], FILE*);
	void solve(FILE*);


};



#endif /* EX6_SRC_BINPRG_H_ */
