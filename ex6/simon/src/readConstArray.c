#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h> // strpbrk
#include <ctype.h>  // isspace
// local
#include "numb.h"
#include "allocate.h"
#include "constraintArray.h"

#define MAX_LINE_LEN 512 // Maximum input line length

int parseOneIntegerLine(char* s, int* ptrToValue, char* valueTyp,
        const char* filename) {

    char* debug;
    int value = (int) strtol(s, &debug, 10);
    // whitespaces after the number are okay
    while (isspace(*debug)) {
        debug++;
    }
    if (0 != strcmp(debug, "\0")) {
        fprintf(stderr,
                "Invalid data file %s: Invalid %s definition! Expected line format is one integer number optionally surrounded by whitespaces! Given format was: '%s'\n",
                filename, valueTyp, s);
        return -1;
    }

    *ptrToValue = value;
    return 1;
}

/*
 * return values:
 *  1: no feasible or all feasible solution
 *  0: default
 * -1: reading error
 */

int read_constArray_from_file(const char* filename, ConstArray** cA) {
    assert(NULL != filename);
    assert(0 < strlen(filename));

// given data for file reading
    FILE* fp;
    char buf[MAX_LINE_LEN];
    char* s;
    int lines = 0;

// own data for constraint array building
    int dim = -1;
    int nConstraints = -1;
    int constraintIndex = 0;
    int dimensionIndex;
    Numb* coeffs = NULL;
    Numb rhs;
    char* delim = " ";
    char* token;
    constType type = -1;
    ConstArray* c = NULL;
    int readError = 0;
    int infeasConstr = 0;

    if (NULL == (fp = fopen(filename, "r"))) {
        fprintf(stderr, "Can't open file %s\n", filename);
        return -1;
    }
    char* lineCopy = allocate(MAX_LINE_LEN, sizeof(*lineCopy));
    while (!readError && !infeasConstr
            && NULL != (s = fgets(buf, sizeof(buf), fp))) {
        char* t = strpbrk(s, "#\n\r");

        lines++;

        if (NULL != t) /* else line is not terminated or too long */
            *t = '\0'; /* clip comment or newline */

        /* Skip over leading space
         */
        while (isspace(*s))
            s++;

        /* Skip over empty lines
         */
        if (!*s) /* <=> (*s == '\0') */
            continue;

        strcpy(lineCopy, s);

        if (-1 == dim) {
            if (-1
                    == parseOneIntegerLine(s, &dim, "number of variables",
                            filename)) {
                readError = 1;
                continue;
            }
            if (0 < dim && 33 > dim) {
                continue;
            } else {
                fprintf(stderr,
                        "Invalid data file %s: Number x of variables has to be 1 <= x <= 32! Given x was %i.\n",
                        filename, dim);
                readError = 1;
                continue;
            }
            printf("dim %i\n", dim);
        }

        if (-1 == nConstraints) {
            if (-1
                    == parseOneIntegerLine(s, &nConstraints,
                            "number of constraints", filename)) {
                readError = 1;
                continue;
            }
            if (0 < nConstraints) {
                c = newConstArray(dim, nConstraints);
                continue;
            } else {
                fprintf(stderr,
                        "Invalid data file %s: Number of constraints has to be a positive integer! Given number was %i.\n",
                        filename, nConstraints);
                readError = 1;
                continue;
            }
            printf("nConstraints %i\n", nConstraints);
        }

        dimensionIndex = 0;
        coeffs = (Numb*) allocate(dim, sizeof(*coeffs));

        if (constraintIndex >= nConstraints) {
            fprintf(stderr,
                    "Invalid data file %s: To many constraints! There should be at most %i.\n",
                    filename, nConstraints);
            readError = 1;
            continue;
        }

        int continueFlag = 0;
        // while there are tokens and the token is not "<=", "==" or ">="
        for (token = strtok(s, delim);
                NULL != token && 0 != strcmp(token, "<=")
                        && 0 != strcmp(token, "==") && 0 != strcmp(token, ">=");
                token = strtok(NULL, delim)) {

            if (0 != strcmp(token, "<=")) {
                Numb value;
                if (-1 == parseStringToNumb(token, &value)) {
                    fprintf(stderr,
                            "Invalid data file %s: Error parsing coefficient %i of constraint %i: Illegal %s number format of coefficient: '%s'!\n",
                            filename, dimensionIndex + 1, constraintIndex + 1,
                            number_type, token);
                    readError = 1;
                    continueFlag = 1;
                    break;
                }
                // if this if is false, the corresponding error is formulated below
                if (dimensionIndex < dim) {

                    coeffs[dimensionIndex] = value;
                }
                dimensionIndex++;
            }
        }
        if (continueFlag) {
            continueFlag = 0;
            continue;
        }

        if (dimensionIndex != dim) {
            fprintf(stderr,
                    "Invalid data file %s: Error creating constraint %i: Incorrect number of variables! It should be %i but there are %i coefficients. \n",
                    filename, constraintIndex + 1, dim, dimensionIndex);
            readError = 1;
            continue;
        }

        // token should be "<=", "==" or ">=" now
        if (NULL == token) {
            fprintf(stderr,
                    "Invalid data file %s: Error creating constraint %i: No comparison operator and no right-hand-side given!\n",
                    filename, constraintIndex + 1);
            readError = 1;
            continue;
        } else {
            if (0 == strcmp(token, "<=")) {
                type = LESS_EQUAL;
            } else if (0 == strcmp(token, "==")) {
                type = EQUAL;
            } else {
                // no other token possible since otherwise the for loop above would not have finished
                type = GREATER_EQUAL;
            }
        }

        // rhs token
        token = strtok(NULL, delim);
        if (NULL != token) {
            if (-1 == parseStringToNumb(token, &rhs)) {
                fprintf(stderr,
                        "Invalid data file %s: Error parsing rhs of constraint %i: Illegal %s number format of rhs: '%s'!\n",
                        filename, constraintIndex + 1, number_type, token);
                readError = 1;
                continue;
            }
        } else {
            fprintf(stderr,
                    "Invalid data file %s: Error creating constraint %i: No right-hand-side given!\n",
                    filename, constraintIndex + 1);
            readError = 1;
            continue;
        }
        if (NULL != strtok(NULL, delim)) {
            fprintf(stderr,
                    "Invalid data file %s: Error creating constraint %i: Multiple right-hand-sides given!\n",
                    filename, constraintIndex + 1);
            readError = 1;
            continue;
        }
        Constraint* constr=newConstraint(dim, coeffs, rhs, type);
        coeffs=NULL;
        int returnValue = setConstraint(c,constr,constraintIndex);
        switch (returnValue) {
        case 1:
            printf(
                    "Skipping the following constraint, since it is always feasible, and decreasing the number of constraints by 1: '%s' \n",
                    lineCopy);
            nConstraints--;
            setNumberOfConstrains(c, nConstraints);
            freeConstraint(constr);
            break;
        case 0:
            constraintIndex++;
            break;
        case -1:
            infeasConstr = 1;
            continue;
        case -2:
            fprintf(stderr,
                    "Invalid data file %s: Error creating constraint %i: Left-hand-side may add up to a value bigger than the maximum value of the number type %s!\n",
                    filename, constraintIndex + 1, number_type);
            readError = 1;
            continue;
        case -3:
            fprintf(stderr,
                    "Invalid data file %s: Error creating constraint %i: Left-hand-side may add up to a value smaller than the minimum value of the number type %s!\n",
                    filename, constraintIndex + 1, number_type);
            readError = 1;
            continue;
        default:
            fprintf(stderr,
                    "Unknown return status of method 'newConstraint'. This should not happen. Something went terribly wrong. Return state was %i\n",
                    returnValue);
            readError = 1;
            continue;
        }

    }

    fclose(fp);
    deallocate(lineCopy);

    if (readError) {
        freeConstArray(c);
        if (NULL != coeffs) {
            deallocate(coeffs);
        }
        return -1;
    }

    if (infeasConstr) {
        printNoFeasibleAssignment();
        freeConstArray(c);
        return 1;
    }

    if (constraintIndex != nConstraints) {
        fprintf(stderr,
                "Invalid data file %s: Not enough constraints! There are %i but there should be %i.\n",
                filename, constraintIndex, nConstraints);
        freeConstArray(c);
        return -1;
    }

    if (0 == nConstraints){
        printAllAssignmentsFeasible(c->dim);
        freeConstArray(c);
        return 1;
    }

    *cA = c;
    return 0;
}
