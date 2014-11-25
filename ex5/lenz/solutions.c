#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <time.h> // clock

#include "allocate.h"
#include "solutions.h"

#define GET_SEC(a, b) ((b - a) / (double)CLOCKS_PER_SEC)

/*
    say we have 0000,
    next sol is 1000,
    next sol is 0100,
    next sol is 1100,
    next sol is 0010,
    ...
    say we have 1111,
    next sol is return false
*/
int next_sol(int* sol, int nvars )
{
    int i;
    for( i = 0; i < nvars; i++ )
    {
        if( sol[i] == 0 )
        {
            sol[i] = 1;
            return 1; /* return true, i.e there is another solution */
        }
        else
            sol[i] = 0;
    }
    return 0; /* there is no further point */
}

int test_sol( int* sol, BP* prob )
{
    int i;
    int j;
#ifdef DOUBLE
    double lhs;
#else
    int lhs;
#endif
    for( i = 0; i < prob->nconss; i++ )
    {
        /* compute lhs */
        lhs = 0;
        for( j = 0; j < prob->nvars; j++ )
        {
            lhs += prob->conss[i][j]  * sol[j];
        }
        switch( prob->eq_type[i] )
        {
        case 'E':
            if( lhs != prob->rhs[i] )
                return 0;
            break;
        case 'L':
            if( lhs > prob->rhs[i] )
                return 0;
            break;
        case 'R':
            if( lhs < prob->rhs[i] )
                return 0;
            break;
        default:
            fprintf(stderr,"ERROR\n");
            exit(1);
            break;
        }
    }
    return 1;
}

void find_binary_solutions( BP* prob )
{
    int sol[prob->nvars];
    int i;

    /* measure computing time for enumeration */
    clock_t start = clock();

    /* start with the zero solution*/
    for( i = 0; i < prob->nvars; i++ )
        sol[i] = 0;
    do
    {
        if( test_sol(sol, prob ) )
        {
            printf("sol: ");
            for( i = 0; i < prob->nvars; i++ )
                printf("%d ", sol[i]);
            printf("\n");
        }
    } while(next_sol(sol, prob->nvars));

    clock_t end = clock();
    double elapsed = GET_SEC(start, end);
    printf("Computing time for enumeration = %f seconds\n", elapsed);
}
