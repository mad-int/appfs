#include <assert.h>  // assert
#include <ctype.h>   // isspace
#include <math.h>    // pow
#include <stdio.h>   // fopen
#include <stdint.h>  // uint32_t
#include <stdlib.h>  // EXIT_*
#include <string.h>  // strpbrk strncmp strtok_r

#define MAX_LINE_LEN  512 // Maximum input line length
#define MAX_DIM       32  // Maximum number of dimensions

//

/**
 * The union of my 32-Bit binary-0-1-vector to enumerate over.
 *
 * This is a union of an uint32 and a bitfield struct
 * for accessing the single bits somewhat directly
 * without bit-operation obfuscation (shifting etc.).
 */
typedef union
{
  uint32_t data;
  struct
  {
    unsigned int _0 : 1; unsigned int _1 : 1; unsigned int _2 : 1; unsigned int _3 : 1;
    unsigned int _4 : 1; unsigned int _5 : 1; unsigned int _6 : 1; unsigned int _7 : 1;

    unsigned int  _8 : 1; unsigned int  _9 : 1; unsigned int _10 : 1; unsigned int _11 : 1;
    unsigned int _12 : 1; unsigned int _13 : 1; unsigned int _14 : 1; unsigned int _15 : 1;

    unsigned int _16 : 1; unsigned int _17 : 1; unsigned int _18 : 1; unsigned int _19 : 1;
    unsigned int _20 : 1; unsigned int _21 : 1; unsigned int _22 : 1; unsigned int _23 : 1;

    unsigned int _24 : 1; unsigned int _25 : 1; unsigned int _26 : 1; unsigned int _27 : 1;
    unsigned int _28 : 1; unsigned int _29 : 1; unsigned int _30 : 1; unsigned int _31 : 1;
  };
} binvec_t;

/**
 * This structs represents the inequalities A and b
 * in A x <= b.
 */
typedef struct
{
  int n;
  int m;
  double* matrix;
  double* rhs; // righthand-side
} ineq_t;


//

/**
 * Get the index-th component of the binary-vector x.
 */
unsigned int get(const binvec_t x, const int index);

/**
 * Check if Ax <= b (x is a feasible solution to ineq).
 */
int is_feasible(const ineq_t ineq, const binvec_t x);

/**
 * Print the binary-vector x to the stdout.
 *
 * maxdim is the maximal dimension to print
 * e.g. for maxdim 4 a possible output is "(0, 1, 1, 0)".
 */
void print_binvec(const binvec_t x, const int maxdim);

//

/**
 * Allocate memory for a struct inequality
 * according to n and m dimension.
 */
ineq_t* malloc_inequalities(const int n, const int m);

/**
 * Read inequality-matrix and vector (A[x] <= b) from a textfile.
 *
 * @param filename name of file to read
 * @return -1 on error and 0 on no error.
 */
ineq_t* malloc_inequalities_from_file(const char* filename);

/**
 * Free an inequalities-struct.
 */
void free_inequalities(ineq_t* ineq);

/**
 * Check if an inequalities-struct is valid.
 */
int is_valid(const ineq_t ineq);

/**
 * Print a inequalities-struct to stdout.
 *
 * This is kind of the reverse operation to
 * malloc_inequalities_from_file(), if one pipes
 * the output to a file, it can later loaded
 * with this function again.
 */
void print_inequalities(const ineq_t ineq);

//

/**
 * Read an int-value from the string s.
 *
 * @param n is the pointer the result is returned via.
 * @return -1 on error or 0 on no error.
 */
int read_int(const char* s, int* n);

/**
 * Read a double-value from the string s.
 *
 * @param d is the pointer the result is returned via.
 * @return -1 on error or 0 on no error.
 */
int read_double(const char* s, double* d);

//

int main(int argc, char** argv)
{
  { // Assert that the endianess is correct.
    binvec_t x = { .data = 0 };
    x._0 = 1;
    assert(x.data == 1);
  }

  if (argc < 2 || strlen(argv[1]) <= 0)
  {
    fprintf(stderr, "usage: %s filename\n", argv[0]);
    return EXIT_FAILURE;
  }

  // Read an inequality A[x] <= b from a file.
  ineq_t* Ab = malloc_inequalities_from_file(argv[1]);
  if(NULL == Ab)
    return EXIT_FAILURE;
  assert(is_valid(*Ab));

  //print_inequalities(*Ab);

  binvec_t x = { .data = 0 };

  /* Check x=0 "by hand" and
   * use it later to detect overflows.
   * (This can only happen when x has the maximal
   * 32 possible dimensions.)
   */
  if(is_feasible(*Ab, x))
      print_binvec(x, Ab->m);

  const uint32_t max = Ab->m < 32 ?
    (1 << Ab->m) // = (2^Ab->m)
    : 0;         // = 2^32 = 0x100000000 = 0 for 32-Bit

  // Enumerate all possible binary-vector values starting from 1.
  for(x.data=1; x.data<max; x.data++)
  {
    // Overflow happend we are out.
    if(0 == x.data)
      break;

    if(is_feasible(*Ab, x))
      print_binvec(x, Ab->m);
  }

  free_inequalities(Ab);

  return EXIT_SUCCESS;
}

/*
 *
 */

int is_feasible(const ineq_t ineq, const binvec_t x)
{
  assert(is_valid(ineq));

  int feasible = 1; // true

  int row;
  for(row=0; row<ineq.n; row++)
  {
    double sum = 0.0;

    int col;
    for(col=0; col<ineq.m; col++)
      if(1 == get(x, col))
        sum += ineq.matrix[row*ineq.m+col];

    feasible = (sum <= ineq.rhs[row]);

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

ineq_t* malloc_inequalities(const int n, const int m)
{
  ineq_t* ret = malloc(sizeof(*ret));

  ret->n = n;
  ret->m = m;
  ret->matrix = malloc(sizeof(ret->matrix)*n*m);
  ret->rhs = malloc(sizeof(ret->rhs)*n);

  assert(NULL != ret);
  assert(is_valid(*ret));

  return ret;
}

void free_inequalities(ineq_t* ineq)
{
  assert(NULL != ineq);
  assert(is_valid(*ineq));

  free(ineq->matrix);
  free(ineq->rhs);

  free(ineq);
}

int is_valid(const ineq_t ineq)
{
  return (ineq.n > 0) && (ineq.m >0)
    && (ineq.matrix != NULL) && (ineq.rhs != NULL);
}

void print_inequalities(const ineq_t ineq)
{
  assert(is_valid(ineq));

  printf("%d\n", ineq.m);
  printf("%d\n", ineq.n);

  int row;
  for(row=0; row<ineq.n; row++)
  {
    int col;
    for(col=0; col<ineq.m; col++)
      printf("%.2f ", ineq.matrix[row*ineq.m+col]);

    printf("<= %.2f\n", ineq.rhs[row]);
  }
}

/**
 * Read inequality-matrix and vector (A[x] <= b) from a textfile.
 *
 * @param filename name of file to read
 * @return -1 on error and 0 on no error.
 *
 * I took the function process_file() from ex4_readline.c
 * and extended it. So the readline via fgets() is from there.
 *
 * TODO: The function got kind of out-of-hand.
 * I should split the logical different functionality:
 * line reading and line parsing (according to
 * the actual state).
 */
ineq_t* malloc_inequalities_from_file(const char* filename)
{
   assert(NULL != filename);
   assert(0    <  strlen(filename));

   FILE* fp;
   char  buf[MAX_LINE_LEN];
   char* s;
   int   lines = 0;
   int   row = 0;

   if (NULL == (fp = fopen(filename, "r")))
   {
      fprintf(stderr, "Error: Can't open file %s!\n", filename);
      return NULL;
   }

   // Dimensions of the matrix nxm.
   int m = -1;
   int n = -1;
   ineq_t* ret = NULL;

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

      //
      // 1. (and 2.) state: line with number of columns (and rows) need to be read.
      //
      if((-1 == m) || (-1 == n))
      {
        int tmp;
        int error = read_int(s, &tmp);

        if(0 != error)
          fprintf(stderr, "Error %s(%d): "
              "Can't parse `%s' to int value: invalid (or no) characters!\n",
              filename, lines, s);

        // Check if value is within valid range: 1-32.
        if((error == 0) && ((0 == tmp) || (32 < tmp)))
        {
          if(-1 == m)
            fprintf(stderr, "Error %s(%d): "
                "`%s' number of columns is invalid (has to be between 1 and %d)!\n",
                filename, lines, s, MAX_DIM);
          else // -1 == n
            fprintf(stderr, "Error %s(%d): "
                "`%s' number of rows is invalid (has to be between 1 and %d)!\n",
                filename, lines, s, MAX_DIM);
          error = -1;
        }

        // on error
        if(0 != error)
        {
          fclose(fp);
          return NULL;
        }

        // Save read number according to the state in m or n.
        if(-1 == m)
          m = tmp;
        else // -1 == n
          n = tmp;

      }   //
      else// 3. state: a constraint line is read.
      {   //

        // On the first run ret is NULL.
        if(ret == NULL)
        {
          ret = malloc_inequalities(n, m);

          if((NULL == ret) || !is_valid(*ret))
          {
            fclose(fp);
            return NULL;
          }
        }

        if(row >= ret->n)
        {
          fprintf(stderr, "Error %s(%d): "
              "Found trailing lines `%s' after the last inequality!\n",
              filename, lines, s);
          free_inequalities(ret);
          fclose(fp);
          return NULL;
        }

        int error = 0;

        char *save;
        const char *tok = strtok_r(s, " ", &save);

        // First: read the m double-numbers.
        int col=0;
        for(col=0; col<m; col++)
        {
          if(NULL == tok)
          {
            fprintf(stderr, "Error %s(%d): "
                "Not enough numbers found on line `%s': expected are %d!\n",
                filename, lines, s, m);
            error = -1;
          }

          if(0 == error) // no error
          {
            error = read_double(tok, &ret->matrix[row*ret->m+col]);

            if(0 != error) // on error
              fprintf(stderr, "Error %s(%d): "
                  "Can't parse `%s' to double value: invalid (or no) characters!\n",
                  filename, lines, tok);
          }

          if(0 != error)
          {
            fclose(fp);
            return NULL;
          }

          tok = strtok_r(NULL, " ", &save);
        }

        // Second: read the less-or-equal "<="
        if((NULL == tok) || (0 != strncmp(tok, "<=", 3)))
        {
          fprintf(stderr, "Error %s(%d): "
              "Didn't find less-or-equal `<=' on the line!\n",
              filename, lines);
          error = -1;
        }

        if(0 == error) // no error
        {
          tok = strtok_r(NULL, " ", &save);

          if(NULL == tok)
          {
            fprintf(stderr, "Error %s(%d): "
                "Didn't find double after less-or-equal `<=' on the line!\n",
                filename, lines);
            error = -1;
          }
        }

        // Third: read the missing righthand-side number.
        if(0 == error)
        {
          error = read_double(tok, &ret->rhs[row]);

          if(0 != error)
            fprintf(stderr, "Error: %s(%d): "
                "Can't parse `%s' to double value: invalid (or no) characters!\n",
                filename, lines, tok);
        }

        // Check for trailing stuff.
        if(0 == error)
        {
          tok = strtok_r(NULL, " ", &save);

          if(NULL != tok)
          {
            fprintf(stderr, "Error %s(%d): "
                "Found trailing stuff `%s' after finishing parsing on the line!\n",
                filename, lines, tok);
            error = -1;
          }
        }

        if(0 != error)
        {
          fclose(fp);
          return NULL;
        }

        row++;
      }
   }
   fclose(fp);

   return ret;
}

/*
 *
 */

static int read_error(const char* s, const char* error_ptr);

/*
 *
 */
int read_int(const char* s, int* n)
{
  char* error_ptr;
  *n = strtol(s, &error_ptr, 10); // decimal base of 10

  return read_error(s, error_ptr);
}

/*
 *
 */
int read_double(const char* s, double* d)
{
  char *error_ptr;
  *d = strtod(s, &error_ptr);

  return read_error(s, error_ptr);
}

/*
 *
 */
int read_error(const char* s, const char* error_ptr)
{
  // If something was read ...
  if(s != error_ptr)
    // ... skip trailing spaces.
    while(('\0' != *error_ptr) && isspace(*error_ptr))
      error_ptr++;

  // Check for a reading error.
  if('\0' != *error_ptr)
    return -1;

  return 0;
}

