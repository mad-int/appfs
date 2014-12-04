#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <stdbool.h>
#include <assert.h> // assert

#include"def.h"
#include "bp.h"
#include "allocate.h"

/* prints optimization model */
void print_problem( BP* prob )
{
    int i;
    int j;
    printf("Optimization problem has %d rows and %d cols\n", prob->nconss, prob->nvars);

    for( i = 0; i < prob->nconss; i++ )
    {
        for( j = 0; j < prob->nvars; j++ )
        {
            printf("%"BPFORMAT" ", prob->conss[i][j]);
        }
        switch( prob->eq_type[i] )
        {
        case 'E':
            printf(" == %"BPFORMAT"\n", prob->rhs[i]);
            break;
        case 'L':
            printf(" <= %"BPFORMAT"\n", prob->rhs[i]);
            break;
        case 'R':
            printf(" >= %"BPFORMAT"\n", prob->rhs[i]);
            break;
        default:
            fprintf(stderr,"ERROR\n");
            exit(1);
            break;
        }
    }
}

/* prints solution of current bitvector */
void printSols( BP* prob, unsigned int* vars, int* cntSols )
{
    int j;
    printf("%3d.sol: ", *cntSols);
    for( j = prob->nvars; j > 0; --j)
    {
        printf("%"BPFORMAT"", vars[j]);
    }
    printf("\n");
}

/* frees problem */
void free_problem( BP* prob )
{
    int i;
    deallocate(prob->rhs);
    deallocate(prob->eq_type);
    for( i = 0; i < prob->nconss; ++i )
    {
        deallocate(prob->conss[i]);
    }
    deallocate(prob->conss);
    deallocate(prob);
}

/* calculates max activity of the lhs of a given constraint i */
Value calcMaxActivity( BP* prob, int i )
{
    int j;
    Value sum = 0;

    for( j = 0; j < prob->nvars; j++ )
    {
        Value coeff = prob->conss[i][j];
        if(coeff > 0.0)
        {
            sum += coeff;
        }
    }
    return sum;
}

/* calculates min activity of a given constraint */
Value calcMinActivity(BP* prob, int i)
{
    int j;
    Value sum = 0;

    for( j = 0; j < prob->nvars; j++ )
    {
        Value coeff = prob->conss[i][j];
        if(coeff < 0.0)
        {
            sum += coeff;
        }
    }
    return sum;
}

/* substitutes the row of a redundant constraint with
   the last constraint; removes the last constraint;
   and counts the redundant constraint */
void subsConstraint( BP* prob, int i )
{
    int j;
    prob->rhs[i] =  prob->rhs[prob->nconss-1];
    prob->eq_type[i] = prob->eq_type[prob->nconss-1];
    for( j = 0; j < prob->nvars; j++ )
    {
        prob->conss[i][j] = prob->conss[prob->nconss-1][j];
    }
    prob->nconss--;
}


/* simple preprocessing, removes redundant constraints
   or detects simple infeasibility.
   If a constraint is redundant, it is substituted by
   the last one and the last constraint is removed */
void preprocessing( BP* prob )
{
    int i;
    int cntRed = 0; /* number of redundant constraints */
    Value maxActivity;
    Value minActivity;
    assert( NULL != prob);

    for( i = 0; i < prob->nconss; ++i )
    {
        assert( 'R' != prob->rhs[i] );
        maxActivity = calcMaxActivity( prob, i );
        minActivity = calcMinActivity( prob, i );

        /* Infeasibility Detection */
        if( minActivity > prob->rhs[i] )
        {
            fprintf(stderr,"Model is infeasible\n");
            exit(1);
        }

        if( 'L' == prob->eq_type[i] )
        {
            /* Redundancy Detection */
            if( maxActivity <= prob->rhs[i] )
            {
                subsConstraint( prob, i );
                --i;
                ++cntRed;
            }
        }
        else if( 'E' == prob->eq_type[i] )
        {
            /* Infeasibility Detection */
            if( maxActivity < prob->rhs[i] )
            {
                fprintf(stderr,"Model is infeasible\n");
                exit(1);
            }
            /* Redundancy Detection */
            if( minActivity == maxActivity && maxActivity == prob->rhs[i] )
            {
                subsConstraint( prob, i );
                --i;
                ++cntRed;
            }
        }
    }
    printf("Preprocessing removed %2d constraints due to maxActivity/minActivity.\n", cntRed);
}
