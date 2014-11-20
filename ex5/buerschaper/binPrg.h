/*
 * binPrg.h
 *
 *  Created on: Nov 16, 2014
 *      Author: luke
 */

#ifndef EX5_BINPRG_H_
#define EX5_BINPRG_H_

#include <cstdio>
#include <assert.h>
#include <math.h>
#define dtype double
#define LIMIT 32

class binPrg {
public:
	//members
	int rows;
	int columns;
	dtype A[32][32];
	dtype b[32];
    bool comparator[32]; //reflects whether constraint in row i is "="(true) or "<=" (false)

	//constr
	binPrg();
	binPrg(int,int);

	//deconstr
	virtual ~binPrg();

	//functions
	bool isValid();
	void multiplyRow(int,int);

	bool isFeasible(const dtype*);
	void printFeasible(const int[]);
	void solve();


};

#endif /* EX5_BINPRG_H_ */
