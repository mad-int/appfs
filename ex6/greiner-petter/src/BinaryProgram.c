/*
 * BinaryProg.c
 *
 * Created on: 19.11.2014
 *      Author: André Greiner-Petter
 */

// include something stuff
#include <stdio.h> // standard I/O lib
#include <stdlib.h> // standard library (for instance for constants)
#include <assert.h> // assertions
#include <string.h> // for strtok
#include <stddef.h> // pointer arithmetics

#include "BinaryProgram.h"

// result definitions
#define DONE		0
#define FAILURE		1
#define LINE_JUMP	2

/*
 * Stores the meta informations contains in the current line of the given fp file
 * and write it to the given storage.
 */
void storeMeta( char buf[], FILE* fp, int* storage ){
	// create a string pointer to tokanize string
	char *stringPointer;
	fgets(buf, MAX_LINE_LENGTH, fp); // read in buf
	
	// set pointer to first element until ' ' or '#'
	stringPointer = strtok(buf, "# ");
	// read integer from char array
	sscanf(stringPointer, "%d", storage);
}

/*
 * Returns the integer value of the enumeration equalityType.
 * None means this char pointer doesn't pointing to an equality type
 */
int getConstraintType( char* str ){
	if ( *str == '<' ){
		if ( *(str+1) == '=' ) return LEQ;
		else return LESS;
	} else if ( *str == '>' ){
		if ( *(str+1) == '=' ) return GEQ;
		else return GREATER;
	} else if ( *str == '=' ) {
		if ( *(str+1) == '>' ) return GEQ;
		else if ( *(str+1) == '<' ) return LEQ;
		else return EQUAL;
	}
	else return NONE;
}

/**
 * Read and store a constraint from the given file.
 * 
 * Returns 0 if the process finished correctly, otherwise returns 1.
 */
int readConstraint( char buf[], FILE* fp, double* a, double* b, int* equalityType, int variables ){
	char *stringPointer;
	if( NULL == fgets(buf, MAX_LINE_LENGTH, fp) ) // read in buf
		return FAILURE;
	
	// sets the pointer to the start the of the string to check some stuff
	stringPointer = &buf[0];
	// find the position of the first wrong symbol in that line, if some exists
	int posWrongSymbol = strspn(stringPointer, "#-0123456789.<=> ");
	// find the position of a comment
	char *posComment = strchr(stringPointer, '#');
	
	// difference between comment and code
	ptrdiff_t diff = posComment-stringPointer;
	
	// if a wrong symbol comes before a comment starts stop all calculations
	if ( diff > posWrongSymbol-1 || (posWrongSymbol < strlen(stringPointer)-1 && diff < 0 ) ){
		printf("[ERROR] Found undefined characters at position %d.\n", posWrongSymbol);
		return FAILURE;
	}
	
	// if there are no whitespaces or something other weired stuff happened, stopp all calculations
	if ( NULL == (stringPointer = strtok(buf, " ")) ){
		printf("[ERROR] Cannot split the given constraint on whitespaces.\n");
		return FAILURE;
	}
	
	// otherwise read in all coefficients
	int i;
	for ( i = 0; i < variables; i++ ){
		// if the followed section is not a number
		if ( strspn(stringPointer, "-.0123456789") < strlen(stringPointer) ){
			printf( "[ERROR] Not a number at position %d\n", (i+1));
			return FAILURE;
		} else sscanf( stringPointer, "%lf", (a+i) );
		// null pointer check
		if ( NULL == (stringPointer = strtok(NULL, " ")) ){
			printf( "[ERROR] Reached line end after %d coefficients... expected more information for this constraint.\n", 
					(i+1));
			return FAILURE;
		}
	}
	
	// if equality type doesn't exist, stop calculations
	if ( NONE == (*equalityType = getConstraintType(stringPointer)) ){
		printf("[ERROR] Undefined constraint type. Try '<', '<=', '=', '>=' or '>'.\n");
		return FAILURE;
	} else if ( NULL == (stringPointer = strtok(NULL, " ")) ){
		printf("[ERROR] Reached line end... expected a solution value.\n");
		return FAILURE;
	} else sscanf( stringPointer, "%lf", b );
	
	return DONE;
}

/*
 * Reads a file and try to build a binary program.
 */
BIP* read_BinaryProgram(const char* filename){
	// assertions
	assert( NULL != filename );
	assert( 0	 < strlen(filename));
	
	// read buffer
	char	buf[MAX_LINE_LENGTH];
	
	// file and strcuture pointer
	FILE*	fp;
	BIP*	bip = malloc(sizeof(*bip));
	
	// try to open file
	if ( NULL == ( fp = fopen(filename,"r") ) ){
		printf("[ERROR] Can't open file %s\n", filename);
		return NULL;
	}
	
	// inform user
	printf("Reading %s.\n", filename);
	
	// read in columns and rows
	storeMeta( buf, fp, &bip->cols );
	storeMeta( buf, fp, &bip->rows );
	
	// inform the user
	printf("BIP contains %d columns and %d rows.\n", bip->cols, bip->rows);
	
	// read constraints (number of rows)
	int i, j, mod;
	for ( i = 0, j = 0; i < bip->rows; i++ ){
		mod = readConstraint( buf, fp, bip->a[i+j], &bip->b[i+j], &bip->equalityType[i+j], bip->cols );
		if ( mod == FAILURE ){
			printf("[ERROR] Cannot read constraint %d.\n", (i+1));
			return NULL;
		} else if ( mod == LINE_JUMP ){
			j++; // comment line counter
		}
	}
	
	// close the file and return the bip
	fclose(fp);
	return bip;
}
