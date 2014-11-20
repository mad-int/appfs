#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ineq.h"
#define member_size(type, member) sizeof(((type *)0)->member)

#define MAX_LINE_LENGTH 8192 
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

// get number from a line, advance the pointer if successful
// return value 0 means success, everything else - error
int readInt(char **line, int *result)
{
	assert(result);	
	int offset = 0;
	int r = 1;
	if (line && *line)
		r = (sscanf(*line, " %d%n", result, &offset) == 1) ? 0 : 1;
	char *test = *line + offset;
	if (!r && *test)
		r = !isspace(*test);
	if (!r)
		*line = test;
	return r;
}

// slight copypasting crawl instead of C++ templates
// get number from a line, advance the pointer if successful
// return value 0 means success, everything else - error
int readNumber(char **line, NumberType *result)
{
	assert(result);
	int offset = 0;
	int r = 1;
	if (line && *line)	
		r = (sscanf(*line, NUMBER_FORMAT, result, &offset) == 1) ? 0 : 1;
	char *test = *line + offset;
	if (!r && *test)
		r = !isspace(*test);
	if (!r)
		*line = test;
	return r;
}

// read comparison type, advance the pointer if successful
// return value 0 means success, everything else - error
int readConditionType(char **line, int *result)
{
	assert(result);
	int r = 1;
	if (!line || !*line)
		return r;
	while (**line && isspace(**line))
		(*line)++;
	if (!**line)
		return r;
	*result = 0;
	char buffer[3];	
	strncpy(buffer, *line, sizeof(buffer) - 1);
	buffer[sizeof(buffer) - 1] = 0;
	strtok(buffer, " \n\r\t"); // might be some special character problem here
	if (!strcmp(buffer, "<="))
	{	
		*result = COND_LESS_OR_EQUAL;
		*line += strlen("<=");	
	}	
	else if (!strcmp(buffer, ">="))
	{	
		*result = COND_GREATER_OR_EQUAL;
		*line += strlen(">=");	
	}	
	else if (!strcmp(buffer, "="))
	{	
		*result = COND_EQUAL;
		*line += strlen("=");	
	}
	r = (*result > 0) ? 0 : 1;
	return r;
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
// return value 0 means success, everything else - error
int ReadIneqSystemData(FILE *file, struct IneqSystem **system)
{
	assert(system);	
	*system = AllocateIneqSystem();	

	int result = 1;
	
	char *line; 	
	line = readLine(file);
	int r = readInt(&line, &(*system)->variablesCount);
	if (!r && ((*system)->variablesCount > 0) && ((*system)->variablesCount <= MAX_VARIABLES))
	{	
		line = readLine(file);
		r = readInt(&line, &(*system)->constraintsCount);
		if (!r && ((*system)->constraintsCount > 0) && ((*system)->constraintsCount <= MAX_CONSTRAINTS))		
		{
			int stop = 0;			
			(*system)->constraints = AllocateIneqConstraints((*system)->constraintsCount, (*system)->variablesCount);
			int i, j;	
			for (i = 0; i < (*system)->constraintsCount; ++i)
			{
				line = readLine(file);
				for (j = 0; j < (*system)->variablesCount; ++j)
				{
					stop = readNumber(&line, &(*system)->constraints[i].coefficients[j]);
					if (stop)
						break;				
				}
				if (!stop)
					stop = readConditionType(&line, &(*system)->constraints[i].condition);
				if (!stop)				
					stop = readNumber(&line, &(*system)->constraints[i].conditionValue);
				if (stop)					
					break; 		
			}
			result = stop;
			if (stop)
					DeallocateIneqSystem(*system);
		}	
	}
	return result;
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

// return value 0 means success, everything else - error
int TestSolutionForConstraint(const struct IneqConstraint *constraint, const struct BinaryVector *solution)
{
	assert(constraint);
	assert(solution);	
	int j;
	long double sum = 0;
	for (j = 0; j < solution->variablesCount; ++j)
		sum += GetBinaryVectorComponent(solution, j) * constraint->coefficients[j];
	sum -= (long double)constraint->conditionValue; // rounding?
	int result = 0;	
	if (constraint->condition & COND_LESS_OR_EQUAL)
		result |= (sum > 0);
	if (constraint->condition & COND_GREATER_OR_EQUAL)
		result |= (sum < 0);	
	return result;
}

// return value 0 means success, everything else - error
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


