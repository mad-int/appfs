#include <stdlib.h> // EXIT_*
#include <string.h> // strpbrk
#include <assert.h> // assert
#include <ctype.h>
#include <stdbool.h>

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
    int cntRHS_is_R = 0;
    bool rhsIndicator_isR = false;
    int sum = 0;

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
            prob->nvars = tostr(s, NULL);

            if( prob->nvars <= 0 || prob->nvars > MAX_LINE_LEN )
            {
                fprintf(stderr,"Incompatible number of variables are specified, %d many variables.\n", prob->nvars);
                exit(1);
            }

            strtol(s, &r, 10);
            while (isspace(*r))
                r++;

            if( strcmp(r, "\0") != 0 && r!= strpbrk(r, "#\n\r") )
            {
                fprintf(stderr, "Wrong input data for number of variables specified.\n");
                exit(1);
            }

            mode = READ_ROWS;
            break;

        case READ_ROWS:
            prob->nconss = tostr(s, &r);
            assert(prob->nconss > 0);
            prob->eq_type = allocate(prob->nconss, sizeof(*(prob->eq_type)));
            assert(prob->eq_type != NULL);

            if( prob->nconss <= 0 )
            {
                fprintf(stderr,"Incompatible number of constraints are specified, %d many constraints.\n", prob->nconss);
                free_problem( prob );
                fclose(fp);
                exit(1);
            }

            while (isspace(*r))
                r++;

            if( strcmp(r, "\0") != 0 && r!= strpbrk(r, "#\n\r") )
            {
                fprintf(stderr, "Wrong input data for number of constraints specified.\n");
                exit(1);
            }

            /* allocate memory and initialize everything, since we know the number of constraints
            * and vars */
            prob->conss = allocate( prob->nconss, sizeof(*(prob->conss)) );

            for( i = 0; i < prob->nconss; ++i )
            {
                prob->conss[i] = allocate(prob->nvars, sizeof(**(prob->conss)));
                if( prob->conss[i] == NULL)
                {
                    fprintf(stderr, "out of memory\n");
                    free_problem( prob );
                    fclose(fp);
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
                checkInputData( s, i, j, false, prob, fp);

                Value entry = tostr(s, &r);

                cntVars += 1;

                if( prob->nconss <= j )
                {
                    fprintf(stderr,"Too many constraints are specified, just %d are allowed.\n", prob->nconss);
                    free_problem( prob );
                    fclose(fp);
                    exit(1);
                }

                /* store it in matrix */
                prob->conss[j][i] = entry;

                /* we want to go to next number... so s points to where r was pointing! */
                s = r;
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
		/* In case of '>='  change constraint to '<='  */
		rhsIndicator_isR = true;
		prob->eq_type[j] = 'L';
		sum = 0;
		for( i = 0; i < prob->nvars; i++ )
		{
		     prob->conss[j][i] = -prob->conss[j][i];
		     sum += prob->conss[j][i];
		}
                cntRHS_is_R += 1;
                assert( *(s+1) == '=' );
                s++;
                break;
            default:
                fprintf(stderr,"Equality type is expected to come next in constraint %d.\n", j+1);
                free_problem( prob );
                fclose(fp);
                exit(1);
            }

            /* get rhs */
            /* s is pointing to the first of two characters '==' or '<=' ... so we have to add 2 */
            s += 2;

            /* check input data */
            checkInputData(s, i, j, true, prob, fp);
            Value rhs = tostr(s, NULL);
	    if( rhsIndicator_isR == true)
		prob->rhs[j] = -rhs;
	    else
		prob->rhs[j] = rhs;

	    rhsIndicator_isR = false;
            j++;
            break;
        default:
            abort();
        }
        rhsIndicator_isR = false;
    }
    if( j != prob->nconss )
    {
        fprintf(stderr,"%d many constraints are specified, but model is expected to have %d \n", j, prob->nconss);
	free_problem( prob );
	fclose(fp);
        exit(1);
    }
    assert( j == prob->nconss );
    fclose(fp);
    return lines;
}


void checkInputData( char* s, int i, int j, bool rhsIndicator, BP* prob, FILE* fp)
{
    int auxCheckData;
    int checkData = sscanf(s, "%d", &auxCheckData);

    if( 0 == checkData )
    {
        if ( rhsIndicator == true ) {
            fprintf(stderr, "Wrong input data for rhs in %d.constraint.\n", j+1);
        }
        else {
            fprintf(stderr, "Wrong input data for %d.variable in %d.constraint.\n", i+1, j+1);
        }
        free_problem(prob);
	fclose(fp);
        exit(1);
    }
}
