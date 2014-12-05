#ifndef _ENUMERATION_H_
#define _ENUMERATION_H_

#include "def.h"
#include "bp.h"

/* shifts bits to the right until val is 0 and returns
 * number of shifts */
unsigned int posBitShift( unsigned int val );

/* checks constraint according to its type and increases
 * the counter for infeas constraints */
void checkEqType( BP* prob, int i,
                  Value* lhss, int* numInfeasCons );

/* checks constraints for first bitvector */
void checkConstraintsFistVec( BP* prob, unsigned int* singleBitVec,
                              Value* lhss, int* cntSols );

/* check constraints for current bitvectors
 * (that are generated according to Gray Codes) */
void checkConstraints( BP* prob, unsigned int* singleBitVec,
                       Value* lhss, unsigned int pos,
                       unsigned int flipType, int* cntSols );

void genGrayCode( BP* prob );

#endif //_ENUMERATION_H_
