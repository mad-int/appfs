/*
 * reader.h
 *
 *  Created on: 30.11.2014
 *      Author: Luke
 */

#ifndef EX6_SRC_READER_H_
#define EX6_SRC_READER_H_

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "binPrg.h"

#define MAX_LINE_LEN   512  // Maximum input line length
#define LIMIT 32            // Maximum column or row size

typedef enum{READ_COLS, READ_ROWS, READ_MATRIX} READ_MODE;

void printForamt();

int process_file(const char*, binPrg&);


#endif /* EX6_SRC_READER_H_ */
