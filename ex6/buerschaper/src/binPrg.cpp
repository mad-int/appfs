/*
 * binPrg.cpp
 *
 *  Created on: Nov 16, 2014
 *      Author: luke
 */
#include "binPrg.h"


/**
 * Represents a binary program with maximum 32x32 matrix and maximum 32 constraints ( <= , >= or = ), that calculates its feasible solutions.
 */

    /**
     * Constructor
     */
    binPrg::binPrg() {
    	rows 	= 0;
    	columns = 0;

        for(int i=0;i<32;i++){
        	comparator[i] = false;
        	b[i] = INFINITY;
        	for(int j=0;j<32;j++)
        		A[i][j] = INFINITY;
        }
    }

    /**
     * Destructor
     */
    binPrg::~binPrg() {
    }

    bool binPrg::isValid(){

    	if(1 > rows || 1 > columns
    				|| LIMIT < rows
    				|| LIMIT < columns){
    		fprintf(stderr, "Expected 0 < cols, rows <= %d for binary program. Got rows = %d, cols=%d\n", LIMIT, rows, columns);
    		return false;
    	}
    	for(int i=0; i<rows;i++){
    		if(INFINITY == b[i])
    			return false;
    		for(int j=0;j<columns;j++)
    			if(INFINITY==A[i][j])
    				return false;
    	}
    	return true;
    }

    /**
     * Multiplies the row with index pos of A with a scalar value.
     */
    void binPrg::multiplyRow(int pos, int value){
        assert(0 <= pos && rows > pos);
        for(int cols = 0; cols < columns; cols++){
            A[pos][cols] *= value;
        }
    }

    /**
     * Checks whether A*x = vector yields the constraints b, so whether x is feasible.
     * @return true if feasible, else false
     */
    bool binPrg::isFeasible(const double vector[]){
    	for(int i=0; i<rows;i++){
    		if(comparator[i]){
    			if(fabs(vector[i]-b[i]) > EPSILON){
                    return false;
    			}
    		}
    		else if(b[i]-vector[i]>EPSILON){
                return false;

    		}
    	}
    	return true;
    }

    /**
     * Prints feasible solution x.
     * TODO correct file writing
     */
    void binPrg::printFeasibleToFile(const int x[], FILE* f){
    	for(int i = 0; i < columns; i++)
    		fprintf(f,"%d ",x[i]);
    	fprintf(f,"\n");
    }

    /*
     * Calculates the first size bits of the binary representation of decimal.
     */
    void binPrg::dec2bin(int size, long decimal, int binary[]){
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

    /*
     * Solves binary program and prints all feasible solutions.
     */
    void binPrg::solve(FILE* f){
    	long range = pow(2,columns);
    	clock_t start;
    	start = clock();
    	for(long i = 0; i<range; i++){
    		int x[columns];
    		dec2bin(columns, i,x);
    		double sum[rows];

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
    			printFeasibleToFile(x, f);

    	}
    	printf("Solving and printing into file took %.5f secs.", GET_SEC(start, clock()) );
    }










