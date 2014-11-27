#ifndef _BP_H_
#define _BP_H_

#include "def.h"

typedef struct binaryProgram
{
    Value** conss;
    Value*  rhs;
    char* eq_type;
    int   nconss;
    int   nvars;
} BP;

/* prints optimizaton problem */
void print_problem( BP* prob );

#endif // _BP_H_
