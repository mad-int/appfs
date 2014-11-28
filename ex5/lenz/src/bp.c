#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <stdbool.h>

#include "bp.h"
#include "allocate.h"

void print_problem( BP* prob )
{
    int i;
    int j;
    printf("Optimization problem has %d rows and %d cols\n", prob->nconss, prob->nvars);

    for( i = 0; i < prob->nconss; i++ )
    {
        for( j = 0; j < prob->nvars; j++ )
        {
            printf("#%"BPFORMAT" ", prob->conss[i][j]);
        }
        switch( prob->eq_type[i] )
        {
        case 'E':
            printf(" == #%"BPFORMAT"\n", prob->rhs[i]);
            break;
        case 'L':
            printf(" <= #%"BPFORMAT"\n", prob->rhs[i]);
            break;
        case 'R':
            printf(" >= #%"BPFORMAT"\n", prob->rhs[i]);
            break;
        default:
            fprintf(stderr,"ERROR\n");
            exit(1);
            break;
        }
    }
}

void free_problem(BP* prob)
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
