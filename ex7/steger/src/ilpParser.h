#ifndef _ILPPARSER_H
#define _ILPPARSER_H
#include "linearProgram.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include "allocate.h"

enum readMode {READ_COL, READ_ROW, READ_CONSTR};


struct linearProgram* createLPFromFile(const char*);
int validateNumber(char*);
int fillRow(num*, int, char*, char*);
int parseDecimal(char*);

#endif