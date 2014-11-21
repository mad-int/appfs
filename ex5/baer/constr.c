#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include <math.h>    // fabs
#include <stdbool.h> // bool
#include <stdio.h>   // fopen & printf
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk strncmp strtok_r

#include "constr.h"

// COMPILER SWITCH USE_INT
#ifdef USE_INT


#define DBLINT int

static const char* const DBLINT_STR = "int";

#define strtoX(PTR, ENDPTR)   strtol((PTR), (ENDPTR), 10)

#define ZERO 0

static bool lesseq(int sum, int rhs)  { return sum <= rhs; }
static bool greateq(int sum, int rhs) { return sum >= rhs; }
static bool equal(int sum, int rhs)   { return sum == rhs; }

#define FORMATSTR "%d"


#else // USE_INT


#define DBLINT double

static const char* const DBLINT_STR = "double";

#define strtoX(PTR, ENDPTR)   strtod((PTR), (ENDPTR))

#define ZERO 0.0
#define PRECISION (1E-5)

static bool lesseq(double sum, double rhs) { return (sum-rhs) <= PRECISION; }
static bool greateq(double sum, double rhs) { return (sum-rhs) >= PRECISION; }
static bool equal(double sum, double rhs) { return fabs(sum-rhs) <= PRECISION; }

#define FORMATSTR "%.2f"


#endif // USE_INT


//

static int read_cols(const char* filename, int lineno, const char* line, int* result);
static int read_rows(const char* filename, int lineno, const char* line, int* result);
static int read_constraint(const char* filename, int lineno, const char* line,
    int constrno, const constraints_t* cs);

static int read_cols_rows(const char* filename, int lineno, const char* line, int* result, bool readCols);

//

bool is_feasible(const constraints_t cs, const binvec_t x)
{
  assert(is_valid(cs));

  bool feasible = true;

  int row;
  for(row=0; row<cs.rows; row++)
  {
    DBLINT sum = ZERO;

    int col;
    for(col=0; col<cs.cols; col++)
      if(1 == get(x, col))
        sum += ((DBLINT*)cs.matrix)[row*cs.cols+col];

    switch(cs.cmp[row])
    {
      case LESSEQ:  feasible = lesseq(sum, ((DBLINT*)cs.rhs)[row]); break;
      case GREATEQ: feasible = greateq(sum, ((DBLINT*)cs.rhs)[row]); break;
      case EQUAL:   feasible = equal(sum, ((DBLINT*)cs.rhs)[row]); break;
    }

    if(!feasible)
      break;
  }

  return feasible;
}

void print_binvec(const binvec_t x, const int maxdim)
{
  assert(0 < maxdim);
  assert(maxdim <= 32);

  int i;
  printf("(");
  for(i=0; i<maxdim-1; i++)
    printf("%u, ", get(x, i));
  printf("%u)\n", get(x, i));
}

unsigned int get(const binvec_t x, const int index)
{
  assert(0 <= index);
  assert(index < 32);

  switch(index)
  {
    case  0: return x._0;
    case  1: return x._1;
    case  2: return x._2;
    case  3: return x._3;
    case  4: return x._4;
    case  5: return x._5;
    case  6: return x._6;
    case  7: return x._7;
    case  8: return x._8;
    case  9: return x._9;
    case 10: return x._10;
    case 11: return x._11;
    case 12: return x._12;
    case 13: return x._13;
    case 14: return x._14;
    case 15: return x._15;
    case 16: return x._16;
    case 17: return x._17;
    case 18: return x._18;
    case 19: return x._19;
    case 20: return x._20;
    case 21: return x._21;
    case 22: return x._22;
    case 23: return x._23;
    case 24: return x._24;
    case 25: return x._25;
    case 26: return x._26;
    case 27: return x._27;
    case 28: return x._28;
    case 29: return x._29;
    case 30: return x._30;
    case 31: return x._31;
    default: assert(0 != 0); // unreachable code
  }

  // unreachable code
  assert(0 != 0);
  return 0;
}


/*
 *
 */

constraints_t* malloc_constraints(const int n, const int m)
{
  constraints_t* ret = malloc(sizeof(*ret));

  ret->rows = n;
  ret->cols = m;
  ret->matrix = malloc(sizeof(DBLINT)*n*m);
  ret->cmp = malloc(sizeof(ret->cmp)*n);
  ret->rhs = malloc(sizeof(DBLINT)*n);

  assert(NULL != ret);

  /* initialise ret->cmp otherwise
   * valgrind complains about "Conditional jump or move depends on uninitialised value(s)"
   * in is_valid(). (And valgrind is right, of course.)
   */
  int row=0;
  for(row=0; row<ret->rows; row++)
    ret->cmp[row] = LESSEQ;

  assert(is_valid(*ret));

  return ret;
}

void free_constraints(constraints_t* cs)
{
  assert(NULL != cs);
  assert(is_valid(*cs));

  free(cs->matrix);
  free(cs->cmp);
  free(cs->rhs);

  free(cs);
}

bool is_valid(const constraints_t cs)
{
  bool valid = (0 < cs.rows) && (MAX_DIM > cs.rows)
    && (0 < cs.cols) && (MAX_DIM > cs.cols)
    && (cs.matrix != NULL) && (cs.rhs != NULL);

  int row=0;
  for(row=0; row<cs.rows; row++)
    if(valid)
      valid = ((LESSEQ == cs.cmp[row]) || (GREATEQ == cs.cmp[row]) || (EQUAL == cs.cmp[row]));
    else
      break;

  return valid;
}

void print_constraints(const constraints_t cs)
{
  assert(is_valid(cs));

  printf("%d\n", cs.cols);
  printf("%d\n", cs.rows);

  int row;
  for(row=0; row<cs.rows; row++)
  {
    int col;
    for(col=0; col<cs.cols; col++)
      printf(FORMATSTR" ",  ((DBLINT*)cs.matrix)[row*cs.cols+col]);

    switch(cs.cmp[row])
    {
      case LESSEQ:  printf("<= "); break;
      case GREATEQ: printf(">= "); break;
      case EQUAL:   printf("== "); break;
    }

    printf(FORMATSTR"\n", ((DBLINT*)cs.rhs)[row]);
  }
}

/**
 * Read the constraint-matrix and vector (A[x] [<=|>=|=] b) from a textfile.
 *
 * @param filename name of file to read
 * @return <0 on error and 0 on no error.
 *
 * I took the function process_file() from ex4_readline.c
 * and extended it. So the readline via fgets() is from there.
 */
constraints_t* malloc_constraints_from_file(const char* filename)
{
   assert(NULL != filename);
   assert(0    <  strlen(filename));

   /* We have 3 states here:
    *   read columns the initial state if it is done the next state is
    *   read rows if this is done we are in
    *   read constraint line.
    */
   typedef enum { READ_COLS, READ_ROWS, READ_CONSTR } state_t;

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;

   if (NULL == (fp = fopen(filename, "r")))
   {
      fprintf(stderr, "Error: Can't open file %s!\n", filename);
      return NULL;
   }

   // Dimensions of the matrix is rows x cols.
   state_t state = READ_COLS;
   int cols = -1;
   int rows = -1;
   constraints_t* ret = NULL;
   int constrno = 0;

   int error = 0;

   while(NULL != (s = fgets(buf, sizeof(buf), fp)))
   {
      char* t = strpbrk(s, "#\n\r");

      lines++;

      if (NULL != t) /* else line is not terminated or too long */
         *t = '\0';  /* clip comment or newline */

      /* Skip over leading space
       */
      while(isspace(*s))
         s++;

      /* Skip over empty lines
       */
      if (!*s)  /* <=> (*s == '\0') */
         continue;

      assert('\0' != *s);

      switch(state)
      {
        // 1. state: line with the number of columns need to be read.
        case READ_COLS: error = read_cols(filename, lines, s, &cols);
                        if(0 == error)
                          state = READ_ROWS;
                        break;

        // 2. state: line with the number of columns need to be read.
        case READ_ROWS: error = read_rows(filename, lines, s, &rows);
                        if(0 == error)
                          state = READ_CONSTR;
                        break;

        // 3. state: line with one constraint need to be read.
        case READ_CONSTR:
                        // On the first run ret is NULL.
                        if(NULL == ret)
                        {
                          ret = malloc_constraints(rows, cols);
                          if((NULL == ret) || !is_valid(*ret))
                          {
                            error = -10;
                            break;
                          }
                          constrno = 0;
                        }

                        error = read_constraint(filename, lines, s, constrno, ret);

                        if(0 == error)
                          constrno++;

                        break;
      }

      // On error we are out.
      if(0 != error)
        break;
   }

   if((0 == error) && (constrno < ret->rows))
   {
     assert(NULL != ret);
     fprintf(stderr, "Error %s(%d): "
        "Too few constraint-lines found in file!\n",
        filename, lines);
     error = -11;
   }

   assert((0 != error) || (constrno == ret->rows));

   if((0 != error) && (NULL != ret))
   {
     free_constraints(ret);
     ret = NULL;
   }

   fclose(fp);
   return ret;
}

/*
 *
 */

int read_cols(const char* filename, int lineno, const char* line, int* result)
{
  bool readCols = true;
  return read_cols_rows(filename, lineno, line, result, readCols);
}

int read_rows(const char* filename, int lineno, const char* line, int* result)
{
  bool readCols = false;
  return read_cols_rows(filename, lineno, line, result, readCols);
}

/*
 *
 */

int read_cols_rows(const char* filename, int lineno, const char* line, int* result, bool readCols)
{
  char* endptr;
  *result = strtol(line, &endptr, 10); // decimal base of 10

  // TODO: Range check: errno == ERANGE

  // Nothing was read.
  if(line == endptr)
  {
    fprintf(stderr, "Error %s(%d): "
        "Can't parse `%s' to int value: no valid characters found!\n",
        filename, lineno, line);
    return -1;
  }

  // If something was read ...
  if(line != endptr)
    // ... skip trailing spaces.
    while(('\0' != *endptr) && isspace(*endptr))
      endptr++;

  // Check if there are trailing stuff.
  if('\0' != *endptr)
  {
    fprintf(stderr, "Error %s(%d): "
        "Found trailing stuff on line `%s' after finishing parsing the int!\n",
        filename, lineno, line);
    return -2;
  }

  // Check if value is within valid range: 1-32.
  if((0 >= *result) || (32 < *result))
  {
    if(readCols)
      fprintf(stderr, "Error %s(%d): "
          "`%s' number of columns is invalid (has to be between 1 and %d)!\n",
          filename, lineno, line, MAX_DIM);
    else // !readCols == readRows
      fprintf(stderr, "Error %s(%d): "
          "`%s' number of rows is invalid (has to be between 1 and %d)!\n",
          filename, lineno, line, MAX_DIM);
    return -3;
  }

  return 0;
}

int read_constraint(const char* filename, int lineno, const char* line,
    int constrno, const constraints_t* cs)
{
  if(constrno >= cs->rows)
  {
    fprintf(stderr, "Error %s(%d): "
        "Found trailing lines `%s' after the last constraint-line!\n",
        filename, lineno, line);
    return -8;
  }

  // This saves the actual position on the line.
  const char *lineptr = line;

  //
  // First: read the `cs->cols` double-numbers.
  //
  int col=0;
  for(col=0; col<cs->cols; col++)
  {
    if('\0' == lineptr)
    {
      fprintf(stderr, "Error %s(%d): "
          "Not enough numbers found on line `%s': expected are %d!\n",
          filename, lineno, line, cs->cols);
      return -4;
    }

    /* strtoX is either strtod() or strtol()
     * depending on the type of cs->matrix (int* or double*).
     */
    char* endptr;
    ((DBLINT*)cs->matrix)[constrno*cs->cols+col] = strtoX(lineptr, &endptr);

    // TODO: Range check: errno == ERANGE

    if(lineptr == endptr)
    {
      fprintf(stderr, "Error %s(%d): "
          "Can't parse start of `%s' to %s value: invalid (or no) characters!\n",
          filename, lineno, lineptr, DBLINT_STR);
      return -5;
    }

    // Set actual position to the next value.
    lineptr = endptr;

    // Skip trailing spaces.
    while(('\0' != *lineptr) && isspace(*lineptr))
      lineptr++;
  }

  //
  // Second: read the cmp-symbol "<=", ">=" or "=".
  //
  if(0 == strncmp(lineptr, "<=", 2))
  {
    cs->cmp[constrno] = LESSEQ;
    lineptr += 2;
  }
  else if(0 == strncmp(lineptr, ">=", 2))
  {
    cs->cmp[constrno] = GREATEQ;
    lineptr += 2;
  }
  else if(0 == strncmp(lineptr, "==", 2))
  {
    cs->cmp[constrno] = EQUAL;
    lineptr += 2;
  }
  else
  {
    fprintf(stderr, "Error %s(%d): "
        "Excpected `<=', `>=' or `=' but got `%s' on line `%s'!\n",
        filename, lineno, lineptr, line);
    return -6;
  }

  //
  // Third: read the missing righthand-side number.
  //

  // For strtoX() see comment above.
  char* endptr;
  ((DBLINT*)cs->rhs)[constrno] = strtoX(lineptr, &endptr);

  // TODO: Range check: errno == ERANGE

  if(lineptr == endptr)
  {
    fprintf(stderr, "Error %s(%d): "
        "Can't parse start of `%s' to %s value: invalid (or no) characters!\n",
        filename, lineno, lineptr, DBLINT_STR);
    // Same error code as above when parsing numbers.
    return -5;
  }

  lineptr = endptr;

  // Skip trailing spaces.
  while(('\0' != *lineptr) && isspace(*lineptr))
    lineptr++;

  // Check for trailing stuff.
  if('\0' != *lineptr)
  {
    fprintf(stderr, "Error %s(%d): "
        "Found trailing stuff `%s' after finishing parsing the line: `%s'!\n",
        filename, lineno, lineptr, line);
    return -7;
  }

  return 0;
}

