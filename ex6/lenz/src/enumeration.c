#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <ctype.h>
#include <stdbool.h>
#include <time.h> // clock

#include "enumeration.h"
#include "allocate.h"

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC)


/* shifts bits to the right until val is 0 (log2) and returns
 * number of shifts */
unsigned int posBitShift( unsigned int val )
{
    unsigned int result = 0;
    while ( val )
    {
        result += 1;
        val = val >> 1;
    }
    return result;
}

/* checks constraint according to its type and increases
 * the counter for infeas constraints */
void checkEqType( BP* prob, int i,
                  Value* lhss, int* numInfeasCons )
{
    if( 'L' == prob->eq_type[i] )
    {
        if( lhss[i] > prob->rhs[ i ] )
        {
            *numInfeasCons += 1;
        }
    }
    else if( 'E' == prob->eq_type[i] )
    {
        if( lhss[i] != prob->rhs[ i ] )
        {
            *numInfeasCons += 1;
        }
    }
    else
    {
        fprintf(stderr,"Equation type should be of type 'L' or 'E' at this program point.");
        free_problem( prob );
        exit(1);
    }
}

/* checks constraints for first bitvector */
void checkConstraintsFistVec( BP* prob, unsigned int* singleBitVec,
                              Value* lhss, int* cntSols )
{
    int i;
    int numInfeasCons = 0;

    assert(prob != NULL);

    for( i = 0; i < prob->nconss; ++i )
    {
        lhss[i] = 0;
    }

    /* check constraints */
    for( i = 0; i < prob->nconss; ++i )
    {
        checkEqType( prob, i, lhss, &numInfeasCons );
    }

    /* print solution */
    if ( 0 == numInfeasCons )
    {
        *cntSols += 1;
        printSols( prob, singleBitVec, cntSols );
    }
//     printf("   numInfeasCons = %d \n", numInfeasCons);
}

/* check constraints for current bitvectors
 * (that are generated according to Gray Codes) */
void checkConstraints( BP* prob, unsigned int* singleBitVec,
                       Value* lhss, unsigned int pos,
                       unsigned int flipType, int* cntSols )
{
    int i;

    Value lhs;
    assert(prob != NULL);
    int numInfeasCons = 0;

    for( i = 0; i < prob->nconss; ++i )
    {
        assert( pos-1 >= 0 );
        lhs = 0;

        /* if flipType == 1 --> add constraint coeff of corresponding var to lhss[i]
        else --> reduce lhss[i] with coeff of corresponding var */
        if( 1 == flipType )
            lhs = prob->conss[i][prob->nvars -(pos)] * singleBitVec[pos];

        else
            lhs = - prob->conss[i][prob->nvars -(pos)];

        lhss[i] += lhs;
//         printf(", %d.cons, pos = %u \n",i, pos);
//         printf("flipType = %u ", flipType);
//         printf(", lhs = %2d, \n", lhs);
//         printf(" i = %d lhss[i] = %2d.  \n", i, lhss[i]);

        /* check constraint */
        checkEqType( prob, i, lhss, &numInfeasCons );
    }

    /* print solution */
    if ( 0 == numInfeasCons )
    {
        *cntSols += 1;
        printSols( prob, singleBitVec, cntSols );
    }
}

/* generates Gray codes in the following:
 * 0000 -> 0001 -> 0011 -> 0010 -> 0110 -> 0111 -> 0101 -> 0100
   -> 1100 -> 1101 -> 1111 -> 1110 -> 1010 -> 1011 -> 1001 -> 1000,
   i.e. that only one bit flips between successive vectors.
   The position of the bit that changes and its flip type are stored,
   such that the lhss of the constraints only have to be updated for the
   changing bit.
    At each iteration, the current bitvector is checked for feasibility
    using checkConstraintsFistVec and checkConstraints. */
void genGrayCode( BP* prob )
{
    unsigned int j, i;
    unsigned int singleBitVec[prob->nvars]; /* bitvector */
    /* number of bit vectors to be enumerated */
    unsigned int numBitVectors;
    numBitVectors = 1 << prob->nvars;
    /* u = int representation of current bit vector */
    unsigned int u;
    /* v = int representation of previous bit vector */
    unsigned int v = 0;
    /* bit shift position of successive bitvectors */
    unsigned int pos;
    unsigned int flipType;
    /* left-hand sides of constr for current bitvector*/
    Value lhss[prob->nconss];
    /* counter of feasible solutions*/
    int cntSols = 0;

    assert(prob->nvars >= 0);

    /* measure computing time for enumeration */
    clock_t start = clock();

    /* init first binary vector of gray code */
    for( j = prob->nvars; j > 0; --j)
    {
        singleBitVec[j] = 0;
    }

    checkConstraintsFistVec( prob, singleBitVec, lhss, &cntSols );

    for(i = 1; i < numBitVectors; ++i)
    {
        /* example: i:      0110
                    i>> 1:  0011
                     xor:   0101 = u   */
        u = (i >> 1) ^ i;

        for( j = prob->nvars; j > 0; --j)
        {
            if( u & (1<<(j-1)) )
            {
                /* => bit j is 1 */
                singleBitVec[j] = 1;
            }
            else
            {
                /* => bit j is 0 */
                singleBitVec[j] = 0;
            }
        }

        /* u ^v is exclusive or operator, example: 0110
                                                   0011
                                               xor 0101 = u ^v
                                   1 << 3,  posBitShift = 3  */
        pos = posBitShift( u ^v );

        /* check kind of bitshift from previous to current
        bitvector: flipType = 1 --> from 0 to 1 and
        flipType = 0 --> from 1 to 0  */
        if( 1 == singleBitVec[pos]  )
            flipType = 1;
        else
            flipType = 0;

        checkConstraints( prob, singleBitVec, lhss, pos, flipType, &cntSols );

        v = u;
    }
    clock_t end = clock();
    double elapsed = GET_SEC(start, end);
    printf("Computing time for enumeration = %f seconds\n", elapsed);
}
