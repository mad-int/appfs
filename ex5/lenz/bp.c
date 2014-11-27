#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*

#include "bp.h"

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