#ifndef constraints__h
#define constraints__h

#include <limits.h>
#include <stdint.h>

#define MAX_VARIABLES 32
#define MAX_CONSTRAINTS INT_MAX

#ifdef USE_DBL
	typedef double NumberType;
	#define NUMBER_FORMAT (" %lf%n")
#else
	typedef long int NumberType;
	#define NUMBER_FORMAT (" %ld%n")
#endif

//
// linear inequality constraint
//


static const int COND_LESS_OR_EQUAL    = 0x01;
static const int COND_GREATER_OR_EQUAL = 0x02;
static const int COND_EQUAL            = 0x03; // COND_GREATER_OR_EQUAL | COND_LESS_OR_EQUAL;

struct IneqConstraint
{
	int condition;
	NumberType *coefficients;
	NumberType conditionValue;
};

struct IneqConstraint *AllocateIneqConstraints(int count, int variablesCount);
void DeallocateIneqConstraints(struct IneqConstraint *constraints, int count);

//
// linear inequality system
// 

struct IneqSystem
{
	struct IneqConstraint *constraints;
	int constraintsCount;
	int variablesCount;
};

struct IneqSystem *AllocateIneqSystem();
void DeallocateIneqSystem(struct IneqSystem *system);
int ReadIneqSystemData(FILE *file, struct IneqSystem **system);

//
// solution vector
//

struct BinaryVector
{
	uint32_t bitmap;
	int variablesCount;
};

typedef void (*SolutionCallback)(const struct BinaryVector *solution);
int GetBinaryVectorComponent(const struct BinaryVector *vector, const int index);	
int TestSolutionForConstraint(const struct IneqConstraint *constraint, const struct BinaryVector *solution, const unsigned int columnChanged, long double *workingSum);
int TestSolutionForSystem(const struct IneqSystem *system, const struct BinaryVector *solution, const SolutionCallback foundCallback, unsigned int *counter, const unsigned int columnChanged, long double *workingSums);
void PrintSolution(const struct BinaryVector *solution);

#endif
