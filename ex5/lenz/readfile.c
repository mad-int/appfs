#include <stdio.h> // fopen
#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <time.h> // clock

#include "allocate.h"
#include "readfile.h"

#define MAX_LINE_LEN 512 // Maximum input line length

typedef enum { READ_COLS, READ_ROWS, READ_COEF } LINE_MODE;

int process_file( const char* filename, BP* prob )
{
    assert(NULL != filename);
    assert(0 < strlen(filename));

    LINE_MODE mode = READ_COLS;

    FILE* fp;
    char buf[MAX_LINE_LEN];
    char* s;
    char* r;
    int lines = 0;
    int i;
    int j = 0; /* counts number of constraints */
    int cntVars; /* counts number of vars for each constraint*/
    int auxNum;  /* for checking the number of variables and constraints */

#ifdef DOUBLE
    double entryDbl;
    double rhsDbl;
#else
    int entryInt;
    int rhsInt;
#endif // DOUBLE

    if (NULL == (fp = fopen(filename, "r")))
    {
        fprintf(stderr, "Can't open file %s\n", filename);
        return -1;
    }
    while(NULL != (s = fgets(buf, sizeof(buf), fp)))
    {
        char* t = strpbrk(s, "#\n\r");
        lines++;

        if (NULL != t) /* else line is not terminated or too long */
            *t = '\0'; /* clip comment or newline */
        /* Skip over leading space
        */
        while(isspace(*s))
            s++;
        /* Skip over empty lines */
        if (!*s) /* <=> (*s == '\0') */
            continue;

        /* do processing here */
        switch(mode) {

        case READ_COLS:
#ifdef DOUBLE
            prob->nvars = strtod(s, NULL);
#else
            prob->nvars = strtol(s, NULL, 0);
#endif

            if( prob->nvars <= 0 || prob->nvars > MAX_LINE_LEN )
            {
                fprintf(stderr,"Incompatible number of variables are specified, %d many variables.\n", prob->nvars);
                exit(-1);
            }

            /* check if input data for variables is correct */
            auxNum = strtol(s, &r, 10);

            while (isspace(*r))
                r++;

            if( strcmp(r, "\0") != 0 && r!= strpbrk(r, "#\n\r") )
            {
                fprintf(stderr, "Wrong input data for number of variables specified.\n");
                exit(-1);
            }

            prob->eq_type = allocate(prob->nvars, sizeof(*(prob->rhs)));
            mode = READ_ROWS;
            break;

        case READ_ROWS:
#ifdef DOUBLE
            prob->nconss = strtod(s, NULL);
#else
            prob->nconss = strtol(s, NULL, 0);
#endif

            if( prob->nconss <= 0 )
            {
                fprintf(stderr,"Incompatible number of constraints are specified, %d many constraints.\n", prob->nconss);
                exit(-1);
            }

            /* check if input data for constraints is correct */
            auxNum = strtol(s, &r, 10);

            while (isspace(*r))
                r++;

            if( strcmp(r, "\0") != 0 && r!= strpbrk(r, "#\n\r") )
            {
                fprintf(stderr, "Wrong input data for number of constraints specified.\n");
                exit(-1);
            }

            /* allocate memory and initialize everything, since we know the number of constraints
            * and vars */
            prob->conss = allocate( prob->nconss, sizeof(*(prob->conss)) );
            if ( prob->conss == NULL && strpbrk(s, "#\n\r") )
            {
                fprintf(stderr, "out of memory\n");
                return EXIT_FAILURE;
            }
            for( i = 0; i < prob->nconss; ++i )
            {
                prob->conss[i] = allocate(prob->nvars, sizeof(**(prob->conss)));
                if( prob->conss[i] == NULL)
                {
                    fprintf(stderr, "out of memory\n");
                    return EXIT_FAILURE;
                }
            }
            prob->rhs = allocate(prob->nconss, sizeof(*(prob->rhs)));
            mode = READ_COEF;
            break;

        case READ_COEF:
            cntVars = 0;
            for( i = 0; i < prob->nvars; i++ )
            {
                while(isspace(*s))
                    s++;
                /* s is pointing to first non space that we assume is a number */
                r = s;
                /* r is pointing to same place */
                while(!isspace(*r))
                    r++;
                /* r is pointing to a whitespace....  */
                /* so if str is "86 10 34 ..."
                     s r
                we obtain the number from s until r and entry stores 86 */

                /* check input data */
                checkInputData( s, i, j, false);

#ifdef DOUBLE
                entryDbl = strtod(s, &r);
                /* printf("read entry: %f\n", entryDbl); */
#else
                entryInt = strtol(s, &r, 0);
                /* printf("read entry: %d\n", entryInt); */
#endif

                cntVars += 1;

                if( prob->nconss <= j )
                {
                    fprintf(stderr,"Too many constraints are specified, just %d are allowed.\n", prob->nconss);
                    exit(-1);
                }

                /* store it in matrix */
#ifdef DOUBLE
                prob->conss[j][i] = entryDbl;
#else
                prob->conss[j][i] = entryInt;
#endif

                /* we want to go to next number... so s points to where r was pointing! */
                s = r;

                if( *s == '=' || *s == '<' || *s == '>')
                {
                    fprintf(stderr,"Number of variables in %d.constraint is %d, but is expected to be %d \n", j+1, cntVars-1, prob->nvars);
                    exit(-1);
                }
            }
            assert( cntVars == prob->nvars );
            /* get eq_type of inequality */
            while(isspace(*s))
                s++;
            switch(*s)
            {
            case '=':
                prob->eq_type[j] = 'E';
                assert( *(s+1) == '=' );
                s++;
                break;
            case '<':
                prob->eq_type[j] = 'L';
                assert( *(s+1) == '=' );
                s++;
                break;
            case '>':
                prob->eq_type[j] = 'R';
                assert( *(s+1) == '=' );
                s++;
                break;
            default:
                fprintf(stderr,"Equality type is expected to come next in constraint %d.\n", j+1);
                exit(-1);
                break;
            }

            /* get rhs */
            /* s is pointing to the first of two characters '==' or '<=' ... so we have to add 2 */
            s += 2;

            /* check input data */
            checkInputData(s, i, j, true);

#ifdef DOUBLE
            rhsDbl = strtod(s, NULL);
            prob->rhs[j] = rhsDbl;
#else
            rhsInt = strtol(s, NULL, 0);
            prob->rhs[j] = rhsInt;
            /* printf("rhs is %d\n", rhsInt); */
#endif

            j++;
            break;
        default:
            abort();
        }
    }
    if( j != prob->nconss )
    {
        fprintf(stderr,"%d many constraints are specified, but model is expected to have %d \n", j, prob->nconss);
        exit(-1);
    }
    assert( j == prob->nconss );
    fclose(fp);
    return lines;
}


void checkInputData( char* s, int i, int j, bool rhsIndicator)
{
    int auxCheckData;
    int checkData = sscanf(s, "%d", &auxCheckData);

    if(checkData == 0)
    {
        if ( rhsIndicator == true ) {
            fprintf(stderr, "Wrong input data for rhs in %d.constraint.\n", j+1);
        }
        else {
            fprintf(stderr, "Wrong input data for %d.variable in %d.constraint.\n", i+1, j+1);
        }
        exit(-1);
    }
}

void print_problem( BP* prob )
{
    int i;
    int j;
    printf("Optimization problem has %d rows and %d cols\n", prob->nconss, prob->nvars);

    for( i = 0; i < prob->nconss; i++ )
    {
#ifdef DOUBLE
        for( j = 0; j < prob->nvars; j++ )
        {
            printf("%f ", prob->conss[i][j]);
        }
        switch( prob->eq_type[i] )
        {
        case 'E':
            printf(" == %f\n", prob->rhs[i]);
            break;
        case 'L':
            printf(" <= %f\n", prob->rhs[i]);
            break;
        case 'R':
            printf(" >= %f\n", prob->rhs[i]);
            break;
        default:
            fprintf(stderr,"ERROR\n");
            exit(-1);
            break;
        }
#else
        for( j = 0; j < prob->nvars; j++ )
        {
            printf("%d ", prob->conss[i][j]);
        }
        switch( prob->eq_type[i] )
        {
        case 'E':
            printf(" == %d\n", prob->rhs[i]);
            break;
        case 'L':
            printf(" <= %d\n", prob->rhs[i]);
            break;
        case 'R':
            printf(" >= %d\n", prob->rhs[i]);
            break;
        default:
            fprintf(stderr,"ERROR\n");
            exit(-1);
            break;
        }
#endif // DOUBLE
    }
}
