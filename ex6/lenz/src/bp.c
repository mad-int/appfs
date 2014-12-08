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
   or detects simple infeasibility. */
void preprocessing( BP* prob )
{
    int i, j;
    int cntRed = 0; /* number of redundant constraints */
    Value maxActivity;
    Value minActivity;
    assert( NULL != prob);

    /* obvious redundancy because of min/max activity
       or rather infeasibility detection */
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

    /* Redundancy between constraints */

    /* indicates when a constraint has changed such that
     * inner constraint loop should be left */
    bool stop;
    /* checks if rhs = 0 of current constraint */
    bool rhsZero = false;
    for( i = 0; i < prob->nconss-1 ; ++i )
    {
        if( 0 == prob->rhs[i] )
            rhsZero = true;

        int k;
        stop = false;

        for( k = i+1; k < prob->nconss && !stop; ++k )
        {
            /* both constraints have to have rhs = 0 or
            rhs != 0 at the same time */
            if( true == rhsZero )
            {
                if( 0 != prob->rhs[k] )
                    continue;
            }
            else if( false == rhsZero )
            {
                if( 0 == prob->rhs[k] )
                    continue;
            }

            double coeff1, coeff2, val1, val2;
            bool indRedund = false;
	    bool changeType;

            for( j = 0; j < prob->nvars; ++j)
            {
                coeff1 = prob->conss[i][j];
                coeff2 = prob->conss[k][j];

                if( 0 == coeff1 && 0 == coeff2 )
		    continue;

                if( coeff1 != 0 && coeff2 == 0)
                    break;
                else if ( coeff1 == 0 && coeff2 != 0 )
                    break;

                assert( coeff1 != 0 && coeff2 != 0 );

                if( rhsZero == true )
                {
                    bool relCheck = false;

		    /* this if-clause is skipped in the 1st iteration
		       and from then on true, since val1 hast to be set
		       in the 1st iteration */
                    if( true == relCheck )
                    {
			if( val1 == coeff1/coeff2 )
			    indRedund = true;
			else
			    break;	
                    }
                    else
			val1 = coeff2/coeff1;

                    relCheck = true;
                }
                else
                {
                    val1 = coeff1/ prob->rhs[i];
                    val2 = coeff2/ prob->rhs[k];

                    if( val1 == val2 )
                    {
                        if( coeff1 == -coeff2 )
                        {
			    if( prob->eq_type[i] == 'L' && prob->eq_type[k] == 'L' )
				changeType = true;
                        }
			indRedund = true;
                    }
                    else
                    {
                        indRedund = false;
                        break;
                    }
                }
            } // for nvars

            /* case distinction */
            if( true == indRedund )
            {
                if( 'E' == prob->eq_type[i] && 'E' == prob->eq_type[k] )
                {
                    subsConstraint( prob, i );
                    ++cntRed;
                    --i;
                    stop = true;
                    break;
                }
                else if( 'E' == prob->eq_type[i] && 'L' == prob->eq_type[k] )
                {
                    subsConstraint( prob, k );

                    --k;
                    ++cntRed;
                }
                else if( 'L' == prob->eq_type[i] && 'E' == prob->eq_type[k] )
                {
                    subsConstraint( prob, i );
                    ++cntRed;
                    --i;
                    stop = true;
                    break;
                }
                else if( 'L' == prob->eq_type[i] && 'L' == prob->eq_type[k] )
                {
                    if( true == changeType )
		    {
			prob->eq_type[k] = 'E';
		    }
		    subsConstraint( prob, i );
                    ++cntRed;
                    --i;
                    stop = true;
                    break;
                }
            }
        } // for( 1 = k < prob->nconss )
        rhsZero = false;
    } // for( 0 = i < prob->nconss-1 )

    printf("Preprocessing removed %2d constraints at all.\n", cntRed);
}
