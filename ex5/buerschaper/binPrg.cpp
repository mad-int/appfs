/*
 * binPrg.cpp
 *
 *  Created on: Nov 16, 2014
 *      Author: luke
 */

#include "binPrg.h"

binPrg::binPrg() {
    for(int i=0; i<32;i++){
        comparator[i] = false;
    }
}

binPrg::binPrg(int m, int n){
	rows = m;
	columns = n;

	for(int i=0; i<rows;i++){
		for(int j=0;j<columns;j++){
				A[i][j] = 0;
 		}
		b[i] = 0;
		comparator[i] = false;
	}
}

binPrg::~binPrg() {
}

bool binPrg::isValid(){

	if(1 > rows || 1 > columns
				|| LIMIT < rows
				|| LIMIT < columns){
		fprintf(stderr, "Expected 0 < cols, rows <= %d for binary program. Got rows = %d, cols=%d\n", LIMIT, rows, columns);
		return false;
	}

//
//	for(int i=0; i<rows;i++){
//		for(int j=0; i<columns;j++){
//			if(A[i][j]>MAX_VALUE){
//				fprintf(stderr, "Value in A[%d][%d] too big.", i,j);
//				return false;
//			}
//		}
//		if(b[i]>MAX_VALUE){
//			fprintf(stderr, "Value in b[%d] too big.", i);
//							return false;
//		}

//	}

	return true;
}

void binPrg::multiplyRow(int pos, int value){
    assert(0 <= pos && rows > pos);
    for(int cols = 0; cols < columns; cols++){
        A[pos][cols] *= value;
    }
}

bool binPrg::isFeasible(const dtype vector[]){
	for(int i=0; i<rows;i++){
		if(comparator[i]){
			if(vector[i]!=b[i]){
                return false;
			}
		}
		else if(vector[i]>b[i]){
            return false;

		}
	}
	return true;
}

void binPrg::printFeasible(const int x[]){
	for(int i = 0; i < columns; i++)
		printf("%d ",x[i]);
    printf("\n");
}

void dec2bin(int size, long decimal, int binary[]){
    assert(decimal>=0);
    int i = size-1;
    for(int k = 0; k < size;k++){
        binary[k] = 0;
    }
    while(decimal){
        binary[i] = decimal % 2;
        decimal = decimal / 2;
        i--;
    }
}


void binPrg::solve(){
	long range = pow(2,columns);
	for(long i = 0; i<range; i++){
		int x[columns];
		dec2bin(columns, i,x);
		dtype sum[rows];

		/*
		 * Check x on feasibility by vector multiplication with A
		 * and then comparing the result with b
		*/
		for(int k = 0; k<rows;k++){
	        sum[k] = 0;
	    	for(int j = 0; j<columns;j++){
	    		sum[k] += x[j]*A[k][j];
	    	}
		}

		if(isFeasible(sum))
			printFeasible(x);

	}

}






