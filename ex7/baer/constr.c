#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include <float.h>   // DBL_MIN, DBL_MAX
#include <math.h>    // fabs
#include <stdbool.h> // bool
#include <stdint.h>  // uint32_t
#include <stdio.h>   // fopen & printf
#include <stdlib.h>  // EXIT_* malloc free
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
static bool greateq(double sum, double rhs) { return (sum-rhs) >= -PRECISION; }
static bool equal(double sum, double rhs) { return fabs(sum-rhs) <= PRECISION; }

#define FORMATSTR "%.2f"


#endif // USE_INT

//

/**
 * This is the internal data structure.
 * The external has void* instead of int* or double*.
 */
typedef struct
{
  int rows;
  int cols;
  DBLINT* matrix;
  cmp_t* cmp;
  DBLINT* rhs; // righthand-side
} __constraints_intern_t;

//

static __constraints_intern_t cast_as_interal(const constraints_t cs);
static constraints_t          cast_as_external(const __constraints_intern_t cs);

static int read_cols(const char* filename, int lineno, const char* line, int* result);
static int read_rows(const char* filename, int lineno, const char* line, int* result);
static int read_constraint(const char* filename, int lineno, const char* line,
    int constrno, const constraints_t* cs);

static int read_cols_rows(const char* filename, int lineno, const char* line, int* result, bool readCols);

//

bool is_feasible(const constraints_t cs_, const binvec_t x)
{
  assert(is_valid(cs_));
  const __constraints_intern_t cs = cast_as_interal(cs_);

  bool feasible = true;

  int row;
  for(row=0; row<cs.rows; row++)
  {
    DBLINT sum = ZERO;

    int col;
    for(col=0; col<cs.cols; col++)
      if(1 == get(x, col))
        sum += cs.matrix[row*cs.cols+col];

    switch(cs.cmp[row])
    {
      case LESSEQ:  feasible = lesseq(sum, cs.rhs[row]); break;
      case GREATEQ: feasible = greateq(sum, cs.rhs[row]); break;
      case EQUAL:   feasible = equal(sum, cs.rhs[row]); break;
    }

    if(!feasible)
      break;
  }

  return feasible;
}

bool is_feasible_bitflip(const constraints_t cs_, const binvec_t x, const uint32_t updatemask,
    void** const state)
{
  assert(is_valid(cs_));
  assert(NULL != state);

  const __constraints_intern_t cs = cast_as_interal(cs_);

  // Handle state-pointer.

  DBLINT* actual_lhs = (DBLINT*)(*state);

  if(NULL == *state)
  {
    // Get rid of a compiler warning (-Wsign-conversion).
    const unsigned int cs_rows = (unsigned int)cs.rows;
    actual_lhs = malloc(sizeof(*actual_lhs)*cs_rows);

    // This seem to work for double, too.
    memset(actual_lhs, 0, sizeof(*actual_lhs)*cs_rows);

    *state = actual_lhs;

    // This is for the first run - the actual rhs is all zero.
    bool feasible = true;

    int row;
    for(row=0; row<cs.rows; row++)
    {
      switch(cs.cmp[row])
      {
        case LESSEQ:  feasible = lesseq(  ZERO, cs.rhs[row]); break;
        case GREATEQ: feasible = greateq( ZERO, cs.rhs[row]); break;
        case EQUAL:   feasible = equal(   ZERO, cs.rhs[row]); break;
      }

      if(!feasible)
        break;
    }

    return feasible;
  }

  /* Determine changed column by kind-of binary search.
   * I guess I should use the gcc-intrinsic:
   * > int col = __builtin_ctz(updatemask);
   */

  int col = 0;
  if((updatemask & 1) == 0)
  {
    if(updatemask & 0xffff0000)
      col += 16;
    if(updatemask & 0xff00ff00)
      col += 8;
    if(updatemask & 0xf0f0f0f0)
      col += 4;
    if(updatemask & 0xcccccccc)
      col += 2;
    if(updatemask & 0xaaaaaaaa)
      col += 1;
  }

  // Assert that only one bit was set in updatemask.
  assert(1 == (updatemask >> col));

  /*
   * We have to calculate each row to get a new clean state (rhs).
   * In is_feasible() we are out if we find one infeasible row,
   * that doesn't work here anymore.
   */

  if(x.data & updatemask) // bit set (1)
  {
    int row;
    for(row=0; row<cs.rows; row++)
    {
      actual_lhs[row] += cs.matrix[row*cs.cols+col];
    }
  }
  else // bit cleared (1)
  {
    int row;
    for(row=0; row<cs.rows; row++)
    {
      actual_lhs[row] -= cs.matrix[row*cs.cols+col];
    }
  }

  // Check for feasiblity.

  bool feasible = true;

  int row;
  for(row=0; row<cs.rows; row++)
  {
    switch(cs.cmp[row])
    {
      case LESSEQ:  feasible = lesseq(  actual_lhs[row], cs.rhs[row]); break;
      case GREATEQ: feasible = greateq( actual_lhs[row], cs.rhs[row]); break;
      case EQUAL:   feasible = equal(   actual_lhs[row], cs.rhs[row]); break;
    }

    if(!feasible)
      break;
  }

  return feasible;
}

/**
 * Check if row r2 is reduntant with respect to row r1.
 */
bool is_redunant(const constraints_t cs_, const int r1, const int r2)
{
  assert(is_valid(cs_));
  assert(0 <= r1);
  assert(r1 < cs_.rows);
  assert(0 <= r2);
  assert(r2 < cs_.rows);

  assert(r1 != r2);

  const __constraints_intern_t cs = cast_as_interal(cs_);

  if(cs.cmp[r1] != cs.cmp[r2])
    return false;

  // TODO: Support the other relation symbols, too.
  if((LESSEQ != cs.cmp[r1]) || (LESSEQ != cs.cmp[r2]))
    return false;

  // If we normalize with negative values, the relation sign could change.
  DBLINT rhs1 = 0.0 < cs.rhs[r1] ? cs.rhs[r1] : -cs.rhs[r1];
  DBLINT rhs2 = 0.0 < cs.rhs[r2] ? cs.rhs[r2] : -cs.rhs[r2];

  /* For r2 to be redunant
   * all coeffienct have to be smaller than the ones from r1
   * and the rhs has to bigger or equal (after normalization).
   */
  int col;
  for(col=0; col<cs.cols; col++)
    if(!greateq(cs.matrix[r1*cs.cols+col]*rhs2, cs.matrix[r2*cs.cols+col]*rhs1))
      return false;

  return lesseq(cs.rhs[r1], cs.rhs[r2]);
}

void delete_row(constraints_t* cs, const int row)
{
  assert(is_valid(*cs));
  assert(0 <= row);
  assert(row < cs->rows);

  swap_rows(cs, row, cs->rows-1);
  cs->rows--;
}

void swap_rows(constraints_t* cs_, const int r1, const int r2)
{
  assert(is_valid(*cs_));
  assert(0 <= r1);
  assert(r1 < cs_->rows);
  assert(0 <= r2);
  assert(r2 < cs_->rows);

  if(r1 == r2)
    return;

  DBLINT dummy;
  const __constraints_intern_t cs = cast_as_interal(*cs_);

  int col;
  for(col=0; col<cs.cols; col++)
  {
    dummy = cs.matrix[r1*cs.cols+col];
    cs.matrix[r1*cs.cols+col] = cs.matrix[r2*cs.cols+col];
    cs.matrix[r2*cs.cols+col] = dummy;
  }

  cmp_t cmp = cs.cmp[r1];
  cs.cmp[r1] = cs.cmp[r2];
  cs.cmp[r2] = cmp;

  dummy = cs.rhs[r1];
  cs.rhs[r1] = cs.rhs[r2];
  cs.rhs[r2] = dummy;
}

/*
 *
 */

typedef struct {
  int row;
  double rating;
} row_rating_t;

static int cmp(const void* e1, const void* e2)
{
  return (((const row_rating_t*)e1)->rating - ((const row_rating_t*)e2)->rating) <= PRECISION;
}

/**
 * I assume that with EQUAL rows there is a good chances of failing
 * I want them up -> high rating.
 * Then comes LESSEQ high coefficients with small rhs is good.
 * TODO: GREATEQ should be other way around, but I don't regard them here -> low rating.
 */
void reorder_rows_failfirst(constraints_t* cs_)
{
  assert(is_valid(*cs_));

  const __constraints_intern_t cs = cast_as_interal(*cs_);

  // Just to get rid of a compiler warning (-Wsign-conversion).
  // in the malloc()s below.
  unsigned int cs_rows = (unsigned int)cs.rows;
  row_rating_t* row_ratings = malloc(sizeof(*row_ratings)*cs_rows);

  int row, col;
  for(row=0; row<cs.rows; row++)
  {
    if(LESSEQ == cs.cmp[row])
    {
      row_rating_t rr = { row, 0.0 };
      for(col=0; col<cs.cols; col++)
        rr.rating += (double)cs.matrix[row*cs.cols+col];
      rr.rating /= (double)cs.rhs[row];

      row_ratings[row] = rr;
    }
    else if(EQUAL == cs.cmp[row])
    {
      row_rating_t rr = { row, DBL_MAX };
      row_ratings[row] = rr;
    }
    else // GREATEQ == cs.cmp[row] -> { row, 0.0 } is okay
    {
      row_rating_t rr = { row, DBL_MIN };
      row_ratings[row] = rr;
    }
  }

  qsort(row_ratings, cs_rows, sizeof(*row_ratings), cmp);

  /* TODO: Bad hacking.
   * Should work with a second buffer.
   * Here we swap rows and save the swappings in the row_ratings.
   */
  int row2;
  for(row=0; row<cs.rows; row++)
  {
    swap_rows(cs_, row, row_ratings[row].row);
    for(row2=0; row2<cs.rows; row2++)
      if(row_ratings[row2].row == row)
        row_ratings[row2].row = row_ratings[row].row;
  }

  free(row_ratings);
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
    case  0: return x.bits._0;
    case  1: return x.bits._1;
    case  2: return x.bits._2;
    case  3: return x.bits._3;
    case  4: return x.bits._4;
    case  5: return x.bits._5;
    case  6: return x.bits._6;
    case  7: return x.bits._7;
    case  8: return x.bits._8;
    case  9: return x.bits._9;
    case 10: return x.bits._10;
    case 11: return x.bits._11;
    case 12: return x.bits._12;
    case 13: return x.bits._13;
    case 14: return x.bits._14;
    case 15: return x.bits._15;
    case 16: return x.bits._16;
    case 17: return x.bits._17;
    case 18: return x.bits._18;
    case 19: return x.bits._19;
    case 20: return x.bits._20;
    case 21: return x.bits._21;
    case 22: return x.bits._22;
    case 23: return x.bits._23;
    case 24: return x.bits._24;
    case 25: return x.bits._25;
    case 26: return x.bits._26;
    case 27: return x.bits._27;
    case 28: return x.bits._28;
    case 29: return x.bits._29;
    case 30: return x.bits._30;
    case 31: return x.bits._31;
    default: assert(0 != 0); // unreachable code
  }

  // unreachable code
  assert(0 != 0);
  return 0;
}


/*
 *
 */

constraints_t malloc_constraints(const int n, const int m)
{
  assert(n > 0);
  assert(m > 0);

  // Just to get rid of a compiler warning (-Wsign-conversion).
  // in the malloc()s below.
  unsigned int n_ = (unsigned int)n;
  unsigned int m_ = (unsigned int)m;

  __constraints_intern_t cs = { n, m, // rows & cols
      malloc(sizeof(*cs.matrix)*n_*m_), // matrix
      malloc(sizeof(*cs.cmp)*n_), // cmp
      malloc(sizeof(*cs.rhs)*n_) // rhs
  };

  // assert() is probably wrong here, shouldn't I return some null-object instead?
  assert((NULL != cs.matrix) && (NULL != cs.cmp) && (NULL != cs.rhs));

  /* initialise cs.cmp otherwise
   * valgrind complains about "Conditional jump or move depends on uninitialised value(s)"
   * in is_valid(). (And valgrind is right, of course.)
   */
  int row=0;
  for(row=0; row<cs.rows; row++)
    cs.cmp[row] = LESSEQ;

  constraints_t ret = cast_as_external(cs);
  assert(is_valid(ret));
  return ret;
}

void free_constraints(constraints_t cs)
{
  assert(!is_null(cs));
  assert(is_valid(cs));

  free((double*)cs.matrix);
  free(cs.cmp);
  free(cs.rhs);
}

bool is_valid(const constraints_t cs)
{
  bool valid = (0 < cs.rows) && (MAX_DIM >= cs.rows)
    && (0 < cs.cols) && (MAX_DIM >= cs.cols)
    && (NULL != cs.matrix) && (NULL != cs.cmp) && (NULL != cs.rhs);

  int row=0;
  for(row=0; row<cs.rows; row++)
    if(valid)
      valid = ((LESSEQ == cs.cmp[row]) || (GREATEQ == cs.cmp[row]) || (EQUAL == cs.cmp[row]));
    else
      break;

  return valid;
}

bool is_null(const constraints_t cs)
{
  return (0 == cs.rows) || (0 == cs.cols)
    || (NULL == cs.matrix) || (NULL == cs.cmp) || (NULL == cs.rhs);
}

__constraints_intern_t cast_as_interal(const constraints_t cs)
{
  const __constraints_intern_t ret = { cs.rows, cs.cols, (DBLINT*)cs.matrix, cs.cmp, (DBLINT*)cs.rhs };
  return ret;
}

constraints_t cast_as_external(const __constraints_intern_t cs)
{
  const constraints_t ret = { cs.rows, cs.cols, (void*)cs.matrix, cs.cmp, (void*)cs.rhs };
  return ret;
}

void print_constraints(const constraints_t cs_)
{
  assert(is_valid(cs_));
  __constraints_intern_t cs = cast_as_interal(cs_);

  printf("%d\n", cs.cols);
  printf("%d\n", cs.rows);

  int row;
  for(row=0; row<cs.rows; row++)
  {
    int col;
    for(col=0; col<cs.cols; col++)
      printf(FORMATSTR" ",  cs.matrix[row*cs.cols+col]);

    switch(cs.cmp[row])
    {
      case LESSEQ:  printf("<= "); break;
      case GREATEQ: printf(">= "); break;
      case EQUAL:   printf("== "); break;
    }

    printf(FORMATSTR"\n", cs.rhs[row]);
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
constraints_t malloc_constraints_from_file(const char* filename)
{
  assert(NULL != filename);
  assert(0    <  strlen(filename));

  constraints_t ret = { -1, -1, NULL, NULL, NULL };

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
    return ret;
  }

  // Dimensions of the matrix is rows x cols.
  state_t state = READ_COLS;
  int cols = -1;
  int rows = -1;
  int constrno = 0;

  int error = 0;

  while(NULL != (s = fgets(buf, sizeof(buf), fp)))
  {
    error = 0;
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
                      if(is_null(ret))
                      {
                        ret = malloc_constraints(rows, cols);
                        if(is_null(ret) || !is_valid(ret))
                        {
                          error = -10;
                          break;
                        }
                        constrno = 0;
                      }

                      error = read_constraint(filename, lines, s, constrno, &ret);

                      if(0 == error)
                        constrno++;

                      break;
    }

    // On error we are out.
    if(0 != error)
      break;
  }

  // This can happen if we never been in state READ_CONSTR
  // e.g. because of premature end of file.
  if((0 == error) && is_null(ret))
  {
    fprintf(stderr, "Error %s(%d): "
        "No or not enough data found in file!\n",
        filename, lines);
    error = -11;
  }

  if((0 == error) && (constrno < ret.rows))
  {
    assert(!is_null(ret));
    fprintf(stderr, "Error %s(%d): "
        "Too few constraint-lines found in file!\n",
        filename, lines);
    error = -12;
  }

  assert((0 != error) || (constrno == ret.rows));

  if((0 != error) && !is_null(ret))
  {
    free_constraints(ret);
    const constraints_t dummy = { -1, -1, NULL, NULL, NULL };
    ret = dummy;
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
    int constrno, const constraints_t* cs_)
{
  assert(is_valid(*cs_));
  __constraints_intern_t cs = cast_as_interal(*cs_);

  if(constrno >= cs.rows)
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
  for(col=0; col<cs.cols; col++)
  {
    /* This may can't happen
     * except the functon is called wrongly with an empty line.
     * Which maybe is an error (assert())?!
     * I tried to make testcases that trigger this without success!
     * TODO: read docu of strtod() and try making an testcase that triggers this.
     */
    if('\0' == lineptr)
    {
      fprintf(stderr, "Error %s(%d): "
          "Not enough numbers found on line `%s': expected are %d!\n",
          filename, lineno, line, cs.cols);
      return -4;
    }

    /* strtoX is either strtod() or strtol()
     * depending on the type of cs->matrix (int* or double*).
     */
    char* endptr;
    cs.matrix[constrno*cs.cols+col] = strtoX(lineptr, &endptr);

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
    cs.cmp[constrno] = LESSEQ;
    lineptr += 2;
  }
  else if(0 == strncmp(lineptr, ">=", 2))
  {
    cs.cmp[constrno] = GREATEQ;
    lineptr += 2;
  }
  else if(0 == strncmp(lineptr, "==", 2))
  {
    cs.cmp[constrno] = EQUAL;
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
  cs.rhs[constrno] = strtoX(lineptr, &endptr);

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

