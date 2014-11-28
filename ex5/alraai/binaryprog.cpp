/*
 * binaryprog.cpp
 *
 *  Created on: 16.11.2014
 *      Author: Akram
 */

#include "binaryprog.h"

binaryprog::binaryprog(){
	//initialize compSys with "false"
	for(int i = 0; i < 32; i++){
		compSys[i] = false;
	}
}

binaryprog::binaryprog(int m, int n){
	rows = m;
	columns = n;
	//initialize matrix with zero
	for(int i = 0; i < rows; i++){
		for(int j = 0; j < columns; j++){
			matrix[i][j] = 0;
		}
		//initialize vector with zero
		vector[i] = 0;
		//initialize compSys with false
		compSys[i] = false;
	}
}

binaryprog::~binaryprog(){
}

bool binaryprog::isLegal(){
	//see if everything is in order
	if(rows < 1 || columns < 1 || LIMIT < rows || LIMIT < columns){
		fprintf(stderr, "Rows and Columns must be greater than zero. Got rows = %d and columns = %d!\n", rows, columns);
		return false;
	}
	return true;
}

void binaryprog::multiplyRow(int position, int value){
	//If this expression evaluates to 0, this causes an assertion failure that terminates the program
	assert(0 <= position && position < rows);
	for(int i = 0; i < columns; i++){
		matrix[position][i] *= value;
	}
}

bool binaryprog::isSolution(const dtype vec[]){
	//see if solution vector "vec[]" coincides with the right-hand side
	for(int i = 0; i < columns; i++){
		if(compSys[i]){
			if(vec[i] != vector[i]){
				return false;
			}
		}else if(vec[i] > vector[i]){
			return false;
		}
	}
	return true;
}

void binaryprog::printSolution(const int foo[]){
	printf("[");
	for(int i = 0; i < columns; i++){
		printf(" %d ",foo[i]);
	}
	printf("]\n");
}

void decimalTObinary(int size, int decimal, int binary[]){
	assert(decimal >= 0);
	int i = size - 1;
	for(int k = 0; k < size; k++){
		binary[k] = 0;
	}
	while(decimal){
		binary[i] = decimal % 2;
		decimal = decimal / 2;
		i--;
	}
}

void binaryprog::solveProg(){
	long power = pow(2,columns);
	for(int i = 0; i < power; i++){
		int foo[columns];
		decimalTObinary(columns, i, foo);
		dtype sum[rows];
		//check foo on feasibility by vector multiplication with the "matrix", i.e. matrix * vector
		//and then comparing the result with the vector "vector"
		for(int j = 0; j < rows; j++){
			sum[j] = 0;
			for(int k = 0; k < columns; k++){
				sum[j] += foo[k]*matrix[j][k];
			}
		}
		if(isSolution(sum))
			printSolution(foo);
	}
}

