#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>   // clock
#include <math.h> // isnan isinf
// local
#include "constraintArray.h"
#include "allocate.h"
#include "numb.h"

#define MAX_NUMBER_LENGTH 80 // maximum length of a string representing a number

Constraint* newConstraint(int dim, Numb* coeffs, Numb rhs, constType type) {

    assert(dim > 0);

    Constraint* c = (Constraint*) allocate(1, sizeof(*c));
    assert(c != NULL);

    c->dim = dim;

    c->coeffs = coeffs;

    c->rhs = rhs;

    c->type = type;

    return c;
}

void freeConstraint(Constraint* c) {

    deallocate(c->coeffs);
    deallocate(c);
}

void setTypeOfConstraint(Constraint* c, constType type) {
    c->type = type;
}

ConstArray* newConstArray(int dim, int numberOfConstraints) {

    assert(numberOfConstraints > 0);
    assert(dim > 0);

    ConstArray* c = (ConstArray*) allocate(1, sizeof(*c));
    assert(c != NULL);

    c->dim = dim;

    c->numberOfConstraints = numberOfConstraints;

    Constraint** constraints = (Constraint**) allocate(numberOfConstraints,
            sizeof(*constraints));
    assert(constraints != NULL);

    c->constraints = constraints;
    return c;
}

void freeConstArray(ConstArray* c) {

    if (NULL == c) {
        return;
    }
    int i;
    for (i = 0; i < c->numberOfConstraints; i++) {
        // we also may want to free a partially filled constraint array
        if (NULL != c->constraints[i]) {
            freeConstraint(c->constraints[i]);
        }
    }
    deallocate(c->constraints);
    deallocate(c);
}

void setNumberOfConstrains(ConstArray* c, int numberOfConstraints) {

    // number of constraints can be zero, if all constraints are removed because of preprocessing
    assert(numberOfConstraints >= 0);
    c->numberOfConstraints = numberOfConstraints;

}

Numb getCoefficient(Constraint* c, int index) {

    assert(index >= 0);
    assert(index < c->dim);

    return c->coeffs[index];
}

/*
 * return values:
 *  1: always feasible
 *  0: default
 * -1: never feasible
 * -2: maximum overflow
 * -3: minimum overflow
 */
int setConstraint(ConstArray* cA, Constraint* c, int index) {

    assert(index >= 0);
    assert(index < cA->numberOfConstraints);
    assert(cA->dim == c->dim);

    cA->constraints[index] = c;

    Numb maxActivity;
    Numb minActivity;

    if (calculateMaxActivityForConstraint(c, &maxActivity) == -1) {
        return -2;
    }
    if (calculateMinActivityForConstraint(c, &minActivity) == -1) {
        return -3;
    }

    switch (c->type) {
    case LESS_EQUAL:
        if (!numbGreaterNumb(maxActivity, c->rhs)) {
            // constraint will be freed in reading method
            cA->constraints[index]=NULL;
            return 1;
        }
        if (numbGreaterNumb(minActivity, c->rhs)) {
            return -1;
        }
        break;
    case GREATER_EQUAL:
        if (numbSmallerNumb(maxActivity, c->rhs)) {
            return -1;
        }
        if (!numbSmallerNumb(minActivity, c->rhs)) {
            cA->constraints[index]=NULL;
            return 1;
        }
        break;
    default:
        if (numbsEqual(maxActivity, minActivity)
                && numbsEqual(maxActivity, c->rhs)) {
            cA->constraints[index]=NULL;
            return 1;
        }
        if (numbSmallerNumb(maxActivity, c->rhs)
                || numbGreaterNumb(minActivity, c->rhs)) {
            return -1;
        }
    }

    return 0;
}

void printNoFeasibleAssignment() {
    printf("No feasible solution found!\n");
}

char* getBinaryFromInt(int binaryAssignment, int length) {

    char* string = allocate(length, sizeof(*string));

    int mask = 1;
    int i;
    for (i = 0; i < length; i++) {
        string[i] = (0 == (mask & binaryAssignment)) ? '0' : '1';
        mask = mask << 1;
    }
    return string;
}

void printFeasibleBinary(int binary, int dimension) {
    char* string;
    string = getBinaryFromInt(binary, dimension);
    printf("Binary assignment %s is valid!\n", string);
    deallocate(string);
}

void printNumberOfFeasibleBinaries(long nFeasSol) {
    printf("%ld feasible solutions found!\n", nFeasSol);
}

void printAllAssignmentsFeasible(int dimension) {

    printf("All constraints are always feasible!\n");

    long upperBound = 1L << (dimension);

#ifdef PRINT_SOLUTIONS
    long count;
    for (count = 0; count < upperBound; count++) {
        // count is only larger then an int if the loop is aborted for dim=32
        printFeasibleBinary((int)count,dimension);
    }
#endif
    printNumberOfFeasibleBinaries(upperBound);
}

/**
 * 1. remove (with respect to scaling) identical or less tight constraints
 * 2. detect feasibility in scaled constraints
 *
 * returns:
 *   1: infeasibility detected
 *   0: default
 */
int preprocessConstArray(ConstArray* cA) {

    const int printCases = 0;

    int i, j, k;
    int* deletedConsts = allocate(cA->numberOfConstraints,
            sizeof(*deletedConsts));
    int feasible = 1;
    for (i = 0; feasible && i < (cA->numberOfConstraints) - 1; i++) {
        for (j = i + 1; feasible && j < cA->numberOfConstraints; j++) {
            // compare constraint i and j
            if (deletedConsts[i] || deletedConsts[j]) {
                continue;
            }
            Constraint* cI = cA->constraints[i];

            Constraint* cJ = cA->constraints[j];

            k = 0;
            Numb coeffI=getCoefficient(cI, k);
            Numb coeffJ=getCoefficient(cJ, k);
            while (numbsEqual(coeffI,0) && numbsEqual(coeffJ,0)){
                k++;
                coeffI=getCoefficient(cI, k);
                coeffJ=getCoefficient(cJ, k);
            }
            double scalar = ((double) coeffI)
                    / coeffJ;

            int scalable = 1;
            if (!(isinf(scalar) || isnan(scalar))) {
                for (k++; scalable && k < cA->dim; k++) {
                    coeffI = getCoefficient(cI, k);
                    coeffJ = getCoefficient(cJ, k);
                    if (numbsEqual(coeffI, 0)) {
                        if (numbsEqual(coeffJ, 0)) {
                            continue;
                        } else {
                            scalable = 0;
                            continue;
                        }
                    }
                    double scalar2 = ((double) coeffI / coeffJ);
                    if (isinf(scalar2) || isnan(scalar2)
                            || !doublesEqual(scalar2, scalar)) {
                        scalable = 0;
                    }
                }
                if (1 == scalable) {
                    if (printCases){
                        printf("Constraint %i and %i are scalable!\n",i,j);
                    }
                    if (cI->type == cJ->type) {
                        switch (cI->type) {
                        case EQUAL:
                            if (doublesEqual(cI->rhs, scalar * (cJ->rhs))) {
                                // equivalent equations, delete j
                                deletedConsts[j] = 1;
                                if (printCases) {
                                    printf("01 Redundant Constraint %i\n", j);
                                }
                            } else {
                                // scalable equations with non-matching RHS
                                feasible = 0;
                                if (printCases) {
                                    printf("02 Infeasible \n");
                                }
                            }
                            break;
                        case LESS_EQUAL:
                            if (doubleGreaterDouble(scalar, 0.0)) {
                                if (doubleSmallerDouble((double) cI->rhs,
                                        scalar * (cJ->rhs))) {
                                    // cJ redundant
                                    deletedConsts[j] = 1;
                                    if (printCases) {
                                        printf("03 Redundant Constraint %i\n",
                                                j);
                                    }
                                } else {
                                    // cI redundant
                                    deletedConsts[i] = 1;
                                    if (printCases) {
                                        printf("04 Redundant Constraint %i\n",
                                                i);
                                    }
                                }
                            } else {
                                // negative scalar, check for infeasibility or possible equation
                                // check possible states of (cJ->rhs)*scalar <= lhs <= (cI->rhs)
                                if (doubleSmallerDouble(cI->rhs,
                                        (cJ->rhs) * scalar)) {
                                    feasible = 0;
                                    if (printCases) {
                                        printf("05 Infeasible \n");
                                    }
                                } else if (doublesEqual(cI->rhs,
                                        (cJ->rhs) * scalar)) {
                                    // constraints can be transformed into an equation
                                    setTypeOfConstraint(cI, EQUAL);
                                    deletedConsts[j] = 1;
                                    if (printCases) {
                                        printf("06 Redundant Constraint %i\n",
                                                j);
                                    }
                                }
                                // if (doubleGreaterDouble(cI->rhs, (cJ->rhs)*scalar)), the constraints are feasible
                            }
                            break;
                        default:
                            if (doubleGreaterDouble(scalar, 0.0)) {
                                if (doubleGreaterDouble((double) (cI->rhs),
                                        scalar * (cJ->rhs))) {
                                    deletedConsts[j] = 1;
                                    if (printCases) {
                                        printf("07 Redundant Constraint %i\n",
                                                j);
                                    }
                                } else {
                                    deletedConsts[i] = 1;
                                    if (printCases) {
                                        printf("08 Redundant Constraint %i\n",
                                                i);
                                    }
                                }
                            } else {
                                // negative scalar, check for infeasibility or possible equation
                                // check possible states of (cI->rhs) <= lhs <= (cJ->rhs)*scalar
                                if (doubleGreaterDouble((double) cI->rhs,
                                        (cJ->rhs) * scalar)) {
                                    feasible = 0;
                                    if (printCases) {
                                        printf("09 Infeasible\n");
                                    }
                                } else if (doublesEqual((double) cI->rhs,
                                        (cJ->rhs) * scalar)) {
                                    // constraints can be transformed into an equation
                                    setTypeOfConstraint(cI, EQUAL);
                                    deletedConsts[j] = 1;
                                    if (printCases) {
                                        printf("10 Redundant Constraint %i\n",
                                                j);
                                    }
                                }
                                // if (doubleSmallerDouble(cI->rhs, (cJ->rhs)*scalar)), the constraints are feasible
                            }
                        }
                    } else {
                        // non-equal constraint types
                        if (cI->type == EQUAL || cJ->type == EQUAL) {
                            // one of the constraints is an equality
                            double scaledEquationRHS;
                            Constraint* nonEqual;
                            int nEqual;
                            if (cI->type == EQUAL) {
                                scaledEquationRHS = (cI->rhs) / scalar;
                                nonEqual = cJ;
                                nEqual = j;
                            } else {
                                scaledEquationRHS = (cJ->rhs) * scalar;
                                nonEqual = cI;
                                nEqual = i;
                            }
                            if (nonEqual->type == LESS_EQUAL) {
                                // other constraint is LESS_EQUAL
                                if (doubleSmallerDouble(nonEqual->rhs,
                                        scaledEquationRHS)) {
                                    feasible = 0;
                                    if (printCases) {
                                        printf("11 infeasible \n");
                                    }
                                } else {
                                    // nonEquation is redundant
                                    deletedConsts[nEqual] = 1;
                                    if (printCases) {
                                        printf("12 Redundant Constraint %i\n",
                                                nEqual);
                                    }

                                }
                            } else {
                                // other constraint is GREATER_EQUAL
                                if (doubleGreaterDouble(nonEqual->rhs,
                                        scaledEquationRHS)) {
                                    feasible = 0;
                                    if (printCases) {
                                        printf("13 infeasible \n");
                                    }

                                } else {
                                    // nonEquation is redundant
                                    deletedConsts[nEqual] = 1;
                                    if (printCases) {
                                        printf("14 Redundant Constraint %i\n",
                                                nEqual);
                                    }

                                }
                            }
                        } else {
                            // one is less-equal, the other greater equal
                            Constraint* lessEqual = cI;
                            Constraint* greaterEqual = cJ;
                            int lessE = i;
                            int greaterE = j;
                            if (cI->type == GREATER_EQUAL) {
                                lessEqual = cJ;
                                greaterEqual = cI;
                                lessE = j;
                                greaterE = i;
                                // multiply always greaterEqualRhs
                                scalar = 1 / scalar;
                            }
                            if (doubleGreaterDouble(scalar, 0.0)) {
                                // positive scalar
                                if (doubleGreaterDouble(
                                        scalar * (greaterEqual->rhs),
                                        (double) lessEqual->rhs)) {
                                    feasible = 0;
                                    if (printCases) {
                                        printf("15 infeasible \n");
                                    }
                                } else if (doublesEqual(
                                        scalar * (greaterEqual->rhs),
                                        (double) lessEqual->rhs)) {
                                    // constraints can be transformed into an equation
                                    setTypeOfConstraint(cI, EQUAL);
                                    deletedConsts[j] = 1;
                                    if (printCases) {
                                        printf("16 Redundant Constraint %i\n",
                                                j);
                                    }
                                }
                                // if (doubleSmallerDouble(scalar*(greaterEqual->rhs),(double)lessEqual->rhs)), the constraints are feasible
                            } else {
                                //negative scalar
                                if (doubleSmallerDouble((double) lessEqual->rhs,
                                        scalar * (greaterEqual->rhs))) {
                                    // greaterEqual constraint redundant
                                    deletedConsts[greaterE] = 1;
                                    if (printCases) {
                                        printf("17 Redundant Constraint %i\n",
                                                greaterE);
                                    }

                                } else {
                                    // lessEqual constraint redundant
                                    deletedConsts[lessE] = 1;
                                    if (printCases) {
                                        printf("18 Redundant Constraint %i\n",
                                                lessE);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if (!feasible) {
        printNoFeasibleAssignment();
        deallocate(deletedConsts);
        return 1;
    }
    int newNConstr = 0;
    for (i = 0; i < cA->numberOfConstraints; i++) {
        if (!deletedConsts[i]) {
            cA->constraints[newNConstr] = cA->constraints[i];
            newNConstr++;
        }
        else{
            freeConstraint(cA->constraints[i]);
        }
    }
    if (newNConstr != cA->numberOfConstraints) {
        printf("Preprocessing deleted %i of %i constraints.\n",
                cA->numberOfConstraints - newNConstr, cA->numberOfConstraints);
    }
    //TODO resize the constraints-array in cA?
    setNumberOfConstrains(cA, newNConstr);
    deallocate(deletedConsts);

    return 0;
}

int calculateMaxActivityForConstraint(Constraint* c, Numb* ptrToNumb) {

    int i;
    Numb sum = 0;

    for (i = c->dim - 1; i >= 0; i--) {
        Numb value = c->coeffs[i];
        if (numbGreaterNumb(value, 0.0)) {
            // check for positive overflow
            if (sumHasPositiveOverflow(sum, value)) {
                return -1;
            }
            sum += value;
        }
    }
    *ptrToNumb = sum;
    return 1;
}

int calculateMinActivityForConstraint(Constraint* c, Numb* ptrToNumb) {
    int i;
    Numb sum = 0;

    for (i = c->dim - 1; i >= 0; i--) {
        Numb value = c->coeffs[i];
        if (numbSmallerNumb(value, 0.0)) {

            // check for double overflow
            if (sumHasNegativeOverflow(sum, value)) {
                return -1;
            }
            sum += value;
        }

    }
    *ptrToNumb = sum;
    return 1;
}

int checkAllConstraints(ConstArray* cA, Numb* lhss) {
    int i;
    for (i = 0; i < cA->numberOfConstraints; i++) {
        Constraint* c = cA->constraints[i];
        Numb lhs = lhss[i];
        switch (c->type) {
        case LESS_EQUAL:
            if (numbGreaterNumb(lhs, c->rhs)) {
                return 0;
            }
            break;
        case EQUAL:
            if (!numbsEqual(lhs, c->rhs)) {
                return 0;
            }
            break;
        default:
            if (numbSmallerNumb(lhs, c->rhs)) {
                return 0;
            }
        }
    }
    return 1;
}

void update(long* pCount, Numb** pLhss, int* pTestBinary, ConstArray* cA) {

// do not update if all binarys are already checked, just update count then
    if (*pCount < (1L << (cA->dim)) - 1) {

        int i, j;
        int mask = 1;
        // look for dimension 'i' of coefficient to change in the test binary. it is equal to the dimension of the first zero coefficient in the binary representation of count
        for (i = 0; i < cA->dim; i++) {
            if (0 == (mask & *pCount)) {
                break;
            } else {
                mask = mask << 1;
            }
        }
        // does the i'th coefficient in testBinary changes from 0 to 1?
        int addAOne = (0 == (mask & *pTestBinary));

        // update lhss
        Numb* lhss = *pLhss;
        for (j = 0; j < cA->numberOfConstraints; j++) {
//            if (cA->constraints[j]->nonZeros & mask) {
            Numb coeff = getCoefficient(cA->constraints[j], i);
            if (addAOne) {

                lhss[j] += coeff;
            } else {
                lhss[j] -= coeff;
            }
//            }
        }

        // update variables count and test binary
        *pTestBinary ^= (mask);
    }
    *pCount += 1;
}

void printAllFeasibleBinaryAssignments(ConstArray* c) {

    // assuming a 64bit system
    // use hamiltonian path in the cube of dimension 'c->dim' to change only one coefficient between two binary assignments
    long count = 0;
    long upperBound = 1L << (c->dim);
    int testBinary = 0;
    unsigned int feasSolutions = 0;
    // save lhs's for current testBinary
    Numb* lhss = allocate(c->numberOfConstraints, sizeof(*lhss));

    clock_t start = clock();

    for (; count < upperBound; update(&count, &lhss, &testBinary, c)) {
        // cast to int is feasible, since max dimension is 32. we only need the long for the abort of the 'for'-loop
        if (checkAllConstraints(c, lhss)) {
#ifdef PRINT_SOLUTIONS
            printFeasibleBinary(testBinary, c->dim);
#endif
            feasSolutions++;
        }
    }
    deallocate(lhss);

    clock_t end = clock();
    if (!feasSolutions) {
        printNoFeasibleAssignment();
    } else {
        printNumberOfFeasibleBinaries(feasSolutions);
    }
    double timeInS = (double) (end - start) / CLOCKS_PER_SEC;
    printf(
            "Time for enumerating all binaries, checking for feasibility and printing the feasible ones is: %f s\n",
            timeInS);
}

void printConstArray(ConstArray* c) {

    int i, j;

    printf("Printing constraint array:\n");
    printf("dimension: %i\n", c->dim);
    printf("number of constraints: %i\n", c->numberOfConstraints);
    printf("Constraints:\n");
    char* cString = allocate(MAX_NUMBER_LENGTH, sizeof(*cString));
    ;
    for (i = 0; i < c->numberOfConstraints; i++) {
        printf("%i: ", i);
        Constraint* constraint = c->constraints[i];
        char* typeString;
        switch (constraint->type) {
        case LESS_EQUAL:
            typeString = "<=";
            break;
        case EQUAL:
            typeString = "==";
            break;
        default:
            typeString = ">=";
        }
        for (j = 0; j < c->dim; j++) {
            Numb coeff = getCoefficient(constraint, j);
            parseNumbToString(&cString, coeff);
            char* positiveSpace = " ";
            if (0 > coeff) {
                positiveSpace = "";
            }
            printf(" + %s%s x%i", positiveSpace, cString, j);
        }

        parseNumbToString(&cString, constraint->rhs);
        char* positiveSpace = " ";
        if (0 > constraint->rhs) {
            positiveSpace = "";
        }
        printf(" %s %s%s\n", typeString, positiveSpace, cString);
    }
    deallocate(cString);
}

void createDummyConstArray(ConstArray** cA) {

    int maxDim = 5;
    int numberOfConstraints = 3;
    int debug;

    Numb* coeffs1 = (Numb*) allocate(maxDim, sizeof(*coeffs1));
    Numb* coeffs2 = (Numb*) allocate(maxDim, sizeof(*coeffs2));
    Numb* coeffs3 = (Numb*) allocate(maxDim, sizeof(*coeffs3));
    // coeffs 1:
    coeffs1[0] = 1.0;
    coeffs1[1] = 2.0;
    coeffs1[2] = 0.0;
    coeffs1[3] = -3.0;
    coeffs1[4] = 0.0;

    // coeffs 2:
    coeffs2[0] = -1.0;
    coeffs2[1] = 1.0;
    coeffs2[2] = -1.0;
    coeffs2[3] = 1.0;
    coeffs2[4] = -1.0;

    // coeffs 3:
    coeffs3[0] = 1.0;
    coeffs3[1] = -2.0;
    coeffs3[2] = 3.0;
    coeffs3[3] = -4.0;
    coeffs3[4] = 5.0;

    Numb rhs1 = 0.0;
    Numb rhs2 = 0.0;
    Numb rhs3 = -1.0;

    ConstArray* cArray = newConstArray(maxDim, numberOfConstraints);
    debug = setConstraint(cArray,
            newConstraint(maxDim, coeffs1, rhs1, LESS_EQUAL), 0);
    assert(0 == debug);
    debug = setConstraint(cArray,
            newConstraint(maxDim, coeffs2, rhs2, LESS_EQUAL), 1);
    assert(0 == debug);
    debug = setConstraint(cArray,
            newConstraint(maxDim, coeffs3, rhs3, LESS_EQUAL), 2);
    assert(0 == debug);
    *cA = cArray;
}
