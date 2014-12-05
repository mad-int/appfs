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

/* prints optimizaton model */
void print_problem( BP* prob );

/* prints solution of current bitvector */
void printSols( BP* prob, unsigned int* vars, int* cntSols);

/* frees binary program struct data */
void free_problem( BP* prob );

/* calculates max activity of the lhs of a given constraint i */
Value calcMaxActivity(BP* prob, int i);

/* calculates min activity of a given constraint */
Value calcMinActivity(BP* prob, int i);

/* substitutes the row of a redundant constraint with
   the last constraint; removes the last constraint;
   and counts redundant constraint */
void subsConstraint( BP* prob, int i );

/* simple preprocessing, removes redundant constraints.
   If a constraint is redundant, it is substituted by
   the last one and the last constraint is removed */
void preprocessing( BP* prob );

#endif // _BP_H_
