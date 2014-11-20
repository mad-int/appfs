/*
 * reader.h
 *
 *  Created on: Nov 16, 2014
 *      Author: luke
 */

#ifndef EX5_READER_H_
#define EX5_READER_H_

#include <iostream>
#include <stdio.h>   // fopen
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include <binPrg.h>


int process_file(const char*, binPrg&);

#endif /* EX5_READER_H_ */
