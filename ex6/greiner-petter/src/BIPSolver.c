/*
 * BIPIterator.c
 * 
 * This program reads in a given binary program and
 * prints all feasible solutions.
 *
 * Created on: 20.11.2014
 *      Author: André Greiner-Petter
 */

#include <stdio.h> // standard I/O lib
#include <stdlib.h> // standard library (for instance for constants)
#include <time.h> // time measurements
#include <limits.h> // maximum number of unsigned integer

// include binary programming
#include "BinaryProgram.h"

/**
 * Returns TRUE (=1 given in BinaryProg.h) if the given vector x (32-Bit Unsigned Integer) 
 * is a feasable solution for the given binary program, otherwise FALSE (=0 given in the BinaryProg.h)
 */
int isFeasible( unsigned int x, BIP* bip ){
	// value of Ax, indexing
	double value = 0; 
	int i, j;
	// iterate through each constraint
	for ( i = 0; i < bip->rows; i++ ){
		// compute value for Ax per constraint
		for ( j = 0; j < bip->cols; j++ ){
			// unsigned integer to binary
			if ( ((x>>(bip->cols-j-1)) & 1) != 0 ) {
				value += bip->a[i][j];
			}
		}
		
		// after the value is computed check the constraint
		switch(bip->equalityType[i]){
		case EQUAL: // '='
			if ( value != bip->b[i] ) return FALSE; 
			break;
		case LEQ: // '<='
			if ( value > bip->b[i] ) return FALSE;
			break;
		case GEQ: // '>='
			if ( value < bip->b[i] ) return FALSE;
			break;
		case LESS: // '<'
			if ( value >= bip->b[i] ) return FALSE;
			break;
		case GREATER: // '>'
			if ( value <= bip->b[i] ) return FALSE;
			break;
		default: // something goes wrong here
			printf("The constraint type isn't specified for the %d-constraint.\n", i);
			return FALSE;
		}
		// refresh the value of Ax
		value = 0.0;
	}
	
	return TRUE;
}

/**
 * This method fill the given char[] (as a string) with the
 * binary form of a given unsigned integer.
 * Make sure you allocate enough space first and terminate the string
 * with a \0.
 * Usually it should look like this
 * 
 * 		int mem_size = sizeof(unsigned int)*sizeof(char)*2+1;
 * 		char* str = malloc(mem_size+1);
 */
char* intToBin( unsigned int a, char* str, int numOfVars ){
	// gets the number of bits without ')' and '\0'
	size_t bits = numOfVars*sizeof(char)*2;
	
	// make sure u is unsigned
	unsigned int u = a;
	for (; bits--; u>>=1){
		str[bits] = u & 1 ? '1':'0';
		bits--;
		str[bits] = ',';
	}
	
	// overwrite the last ',' with '('
	str[0] = '(';
	
	// finally return the string
	return str;
}

/**
 * This program list all feasible solutions of a given binary program.
 * The given file should contains 
 * 		- the number of columns in the first line
 * 		- the number of rows in the second line followed by
 * 		- the constraints without arithmetic expressions (use whitespace instead)
 */
int main( int argc, const char** argv ){
	if ( argc < 2 ){
		printf("[ERROR] You have to specify a filename (or the complete path).\
				BIPIterator <filename>\n");
		return EXIT_SUCCESS;
	}
	
	puts("Please notice that this program only use double precision (15 decimal places) for fractional coefficients.\n");
	
	// read in the given file
	BIP *bip = read_BinaryProgram(argv[1]);
	if ( NULL == bip ){
		printf("[ERROR] An error occurred after reading the file. Program stopped.\n");
		free(bip);
		return EXIT_SUCCESS;
	}
	
	// maximum number of valid rows/columns
	if ( bip->cols > 32 || bip->rows > 32 ){
		printf("[ERROR] The maximal number of rows and columns are 32! Program stopped.\n");
		free(bip);
		return EXIT_SUCCESS;
	}
	
	// preparations to print all solutions
	unsigned int i = 0; // iterate through integers
	// allocate a string representation of the solutions.
	// a solution should be a vector of zeros and ones.
	int mem_size = bip->cols*sizeof(char)*2+1; // *2 for ',' + 1 for ')'
	char* str = malloc(mem_size+1); // + \0
	
	// if memory allocation failed
	if ( !str ){
		puts("[ERROR] Error occurred while allocating memory to print binary string.");
		free(str); 
		free(bip);
		return EXIT_SUCCESS;
	}
	
	str[mem_size] = 0; // terminate string
	str[mem_size-1] = ')'; // last visible character
	
	// max integer to print all feasible solutions
	unsigned int max = bip->cols >= BIP_COLS ? UINT_MAX : (1 << (bip->cols))-1;
	printf("Maximum number of solutions: %d with max solution could be %s\n", max, intToBin(max, str, bip->cols));
	
	puts("Start to find all feasible solutions.\n");
	
	puts("The feasible solutions of the BIP are:");
	time_t t = clock(); // time measurement
	time_t toBinaryStringTime = 0;
	time_t tmp;
	// iterate through all possible constellations
	for ( i = 0; i <= max; i++ ){
		// if a solution is feasible print it
		tmp = clock();
		if ( isFeasible(i, bip) ){
			intToBin( i, str, bip->cols );
			printf("%s\n", str);
			fflush(stdout);
		}
		if ( i == UINT_MAX ) break;
		toBinaryStringTime += clock()-tmp;
	}
	t = clock()-t;// time measurement
	
	// free the memory of the solution string and for the BIP
	free(str); 
	free(bip);
	
	printf("\nProcess finished after %.4lf seconds (just find and print all solutions).\n", 
			((double)t)/CLOCKS_PER_SEC);
	printf("Needed %.8lf seconds to convert int to binary string.\n",((double)toBinaryStringTime)/CLOCKS_PER_SEC);
	
	// everything works fine
	return EXIT_SUCCESS;
}
