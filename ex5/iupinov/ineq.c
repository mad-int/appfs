#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ineq.h"
#define member_size(type, member) sizeof(((type *)0)->member)

#define MAX_LINE_LENGTH 8192  //yup!!!!s
static char lineBuffer[MAX_LINE_LENGTH];
 
//
// data input routines
// 

// read and prepare one line from a file
char *readLine(FILE *file)
{
	assert(file);	
	char *test = fgets(lineBuffer, sizeof(lineBuffer), file);
	if (test)
	{	
		char *commentStart = strchr(lineBuffer, '#');
		if (commentStart)
			*commentStart = 0;
	}
	return test;
}

// get number from a line, advance a pointer
int readInt(char **line)
{
	int result;	
	assert(line && *line);
	int offset = 0;
	assert(sscanf(*line, " %d%n", &result, &offset) == 1); // try strtol/strtod? nah, too lazy
	*line += offset;
	return result;
}

// slight copypasting crawl instead of C++ templates
// get number from a line, advance a pointer
NumberType readNumber(char **line)
{
	NumberType result;	
	assert(line && *line);
	int offset = 0;
	assert(sscanf(*line, NUMBER_FORMAT, &result, &offset) == 1);
	*line += offset;
	return result;
}

// read comparison type
int readConditionType(char **line)
{
	assert(line && *line);	
	char buffer[3];	
	while (isspace(**line))
		(*line)++;
	strncpy(buffer, *line, sizeof(buffer) - 1);
	buffer[sizeof(buffer) - 1] = 0;
	int result = 0;
	strtok(buffer, " \n\r\t");
	if (!strcmp(buffer, "<="))
	{	
		result = COND_LESS_OR_EQUAL;
		*line += strlen("<=");	
	}	
	else if (!strcmp(buffer, ">="))
	{	
		result = COND_GREATER_OR_EQUAL;
		*line += strlen(">=");	
	}	
	else if (!strcmp(buffer, "="))
	{	
		result = COND_EQUAL;
		*line += strlen("=");	
	}	
	else
		printf("//yup panic\n");
	return result;
}


//
// linear inequality constraint routines
//

struct IneqConstraint *AllocateIneqConstraints(int count, int variablesCount)
{
	struct IneqConstraint *result = calloc(count, sizeof(result[0]));
	int i;
	for (i = 0; i < count; ++i)
	{
		result[i].coefficients = calloc(variablesCount, member_size(struct IneqConstraint, coefficients[0]));
		//result[i].variablesCount = variablesCount;	
	}	
	return result;
}

void DeallocateIneqConstraints(struct IneqConstraint *constraints, int count)
{
	int i;
	for (i = 0; i < count; ++i)
		free(constraints[i].coefficients);
	free(constraints);
}

//
// linear inequality system routines
//

struct IneqSystem *AllocateIneqSystem()
{ 
	return calloc(1, sizeof(struct IneqSystem));
}

void DeallocateIneqSystem(struct IneqSystem *system)
{ 
	assert(system);	
	DeallocateIneqConstraints(system->constraints, system->constraintsCount); 
	free(system);
}		

// reading input data from a file
struct IneqSystem *ReadIneqSystemData(FILE *file)
{
	struct IneqSystem *system = AllocateIneqSystem();	

	char *line; 	
	line = readLine(file);
	system->variablesCount = readInt(&line);
	assert((system->variablesCount > 0) && (system->variablesCount <= MAX_VARIABLES));	
	line = readLine(file);
	system->constraintsCount = readInt(&line);
	assert((system->constraintsCount > 0) && (system->constraintsCount <= MAX_CONSTRAINTS));
	system->constraints = AllocateIneqConstraints(system->constraintsCount, system->variablesCount);
	int i, j;	
	for (i = 0; i < system->constraintsCount; ++i)
	{
		line = readLine(file);
		for (j = 0; j < system->variablesCount; ++j)
			system->constraints[i].coefficients[j] = readNumber(&line);
		system->constraints[i].condition = readConditionType(&line);
		system->constraints[i].conditionValue = readNumber(&line); 		
	}
	return system;
}

//
// solution routines
//

int GetBinaryVectorComponent(const struct BinaryVector *vector, int index)
{
	assert(vector);	
	assert((index >= 0) && (index < MAX_VARIABLES));	
	return (vector->bitmap >> index) & 1;
}

// 0 = success
int TestSolutionForConstraint(const struct IneqConstraint *constraint, const struct BinaryVector *solution)
{
	assert(constraint);
	assert(solution);	
	int j;
	double sum = 0;
	for (j = 0; j < solution->variablesCount; ++j)
		sum += GetBinaryVectorComponent(solution, j) * constraint->coefficients[j];
	sum -= (double)constraint->conditionValue; //yup rounding
	//printf("sum %lf\n", sum);	
	//printf("constraint->condition  %d\n", constraint->condition );		
	int result = 0;	
	if (constraint->condition & COND_LESS_OR_EQUAL)
		result |= (sum > 0);
	if (constraint->condition & COND_GREATER_OR_EQUAL)
		result |= (sum < 0);	
	//printf("%d %d\n", (sum > 0), (sum < 0));	
	return result;
}

// 0 - = success
int TestSolutionForSystem(const struct IneqSystem *system, const struct BinaryVector *solution)
{
	assert(system);
	assert(solution);	
	int i;
	int errorCode = 0;
	for (i = 0; i < system->constraintsCount; ++i)
	{
		errorCode = TestSolutionForConstraint(&system->constraints[i], solution);
		if (errorCode)
			break;
	}
	return errorCode;
}

void PrintSolution(const struct BinaryVector *solution)
{
	assert(solution);
	int i;
	for (i = 0; i < solution->variablesCount; ++i)
		printf("x%d=%d ", i + 1, GetBinaryVectorComponent(solution, i));
	printf("\n");
}


