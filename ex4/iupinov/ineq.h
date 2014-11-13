#ifndef constraints__h
#define constraints__h

#include <stdint.h>

#define MAX_VARIABLES 32
#define MAX_CONSTRAINTS 32

//
// linear inequality constraint
//

struct IneqConstraint
{
	int *coefficients;
	//int variablesCount;
	int condition;
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
struct IneqSystem *ReadIneqSystemData(FILE *file);

//
// solution vector
//

struct BinaryVector
{
	uint32_t bitmap;
	int variablesCount;
};

int GetBinaryVectorComponent(const struct BinaryVector *vector, int index);
int TestSolutionForConstraint(const struct IneqConstraint *constraint, const struct BinaryVector *solution);
int TestSolutionForSystem(const struct IneqSystem *system, const struct BinaryVector *solution);
void PrintSolution(const struct BinaryVector *solution);

#endif
