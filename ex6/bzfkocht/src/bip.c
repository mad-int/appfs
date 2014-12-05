/**@file   bip_enum.c
 * @brief  BIP Enumerator 
 * @author Thorsten Koch
 * @date   20Nov2014
 */  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <time.h>
#include <limits.h>
#include <math.h>
#include <errno.h>
#include <float.h>
#include <fenv.h>

#include "mshell.h"  // or by allocate?
#include "splitline.h"
#include "bip.h"

#define BIP_MAX_COLS 32
#define BIP_MAX_ROWS 128

typedef enum { READ_ERROR, READ_COLS, READ_ROWS, READ_COEF } LINE_MODE;

struct binary_program
{
   int    rows;                               ///< number of rows (constraints)
   int    read_rows;                          ///< rows read so far
   int    cols;                               ///< number of columns (variables)
   Numb   a    [BIP_MAX_ROWS][BIP_MAX_COLS];  ///< coefficient matrix
   Numb   rhs  [BIP_MAX_ROWS];                ///< right hand side
   Sense  sense[BIP_MAX_ROWS];                ///< equation sense   
};

#define MAX_LINE_LEN   512  ///< Maximum input line length

#define GET_SEC(a, b)  ((b - a) / (double)CLOCKS_PER_SEC) 

static Numb max_coef_val;
static Numb min_coef_val;
static bool int_coef;

/** Check whether BIP data structure is consistent.
 *  @return true if ok, false otherwise.
 */
static bool bip_is_valid(const BIP* bip)
{
   if (NULL == bip)
      return false;

   if (max_coef_val <=  min_coef_val) // bip_init not called.
      return false;
   
   if (bip->rows < 1 || bip->rows > BIP_MAX_ROWS)
      return false;
   if (bip->cols < 1 || bip->cols > BIP_MAX_COLS)
      return false;

   for(int r = 0; r < bip->rows; r++)
   {
      for(int c = 0; c < bip->cols; c++)
         if (bip->a[r][c] < min_coef_val || bip->a[r][c] > max_coef_val) // can only happen with floating point Numb
            return false;

      if (bip->rhs[r] < min_coef_val || bip->rhs[r] > max_coef_val) // can only happen with floating point Numb
         return false;
   }
   return true;
}

void bip_init()
{
#if defined(USE_INT)

   max_coef_val = INT_MAX;
   min_coef_val = INT_MIN;
   int_coef     = true;
   
#elif defined(USE_LONGLONG) 

   max_coef_val = LLONG_MAX;
   min_coef_val = LLONG_MIN;
   int_coef     = true;

#elif defined(USE_FLOAT) 

   max_coef_val =  powf(10, (float)FLT_DIG);
   min_coef_val = -powf(10, (float)FLT_DIG);
   int_coef     =  false;

#elif defined(USE_DOUBLE)

   max_coef_val =  pow(10, (double)DBL_DIG);
   min_coef_val = -pow(10, (double)DBL_DIG);
   int_coef     =  false;

#elif defined(USE_LONGDOUBLE)

   max_coef_val =  powl(10, (long double)LDBL_DIG);
   min_coef_val = -powl(10, (long double)LDBL_DIG);
   int_coef     =  false;

#else
#error "No number type defined"
#endif   

   //   printf("Min/Max coefficient values [%.0Lf,%.0Lf]\n",
   //      (long double)min_coef_val, (long double)max_coef_val);
}

/** Deallocate BIP data structure.
 */
void bip_free(BIP* bip)
{
   assert(bip_is_valid(bip));

   mem_check(bip);
   
   free(bip);
}

static bool bip_can_overflow(const BIP* bip)
{
   assert(bip_is_valid(bip));

   for(int r = 0; r < bip->rows; r++)
   {
      Numb row_max = 0;
      Numb row_min = 0;
      
      for(int c = 0; c < bip->cols; c++)
      {
         Numb val = bip->a[r][c];
         
         if (val > 0)
         {
            if (row_max < max_coef_val - val)
               row_max += val;
            else
               return fprintf(stderr, "Error: row %d numerical overflow\n", r), true;
         }
         else if (val < 0)
         {
            if (row_min > min_coef_val - val)
               row_min += val;
            else
               return fprintf(stderr, "Error: row %d numerical negative overflow\n", r), true;
         }
         else
         {
            assert(val == 0);
         }
      }
   }
   return false;
}


static LINE_MODE process_line(LINE_MODE mode, const LFS* lfs, const int lines, BIP* bip)
{
   /* If line is not empty
    */
   if (lfs_used_fields(lfs) > 0)
   {
      long double val;
      char*       t;
      
      /* do processing here:
       * mode tells which kind of line is next.
       */
      switch(mode)
      {
      case READ_COLS :
         if (lfs_used_fields(lfs) != 1)
            return fprintf(stderr, "Error line %d: Got %d fields, expected 1\n",
               lines, lfs_used_fields(lfs)),
               READ_ERROR;

         val = strtold(lfs_get_field(lfs, 0), &t);

         if (errno == ERANGE || isnan(val) || isinf(val) || rintl(val) != val || val < 1.0 || val > (long double)BIP_MAX_COLS || *t != '\0')
            return fprintf(stderr, "Error line=%d Number of cols %s=%Lf out of range or not integer %s\n",
               lines, lfs_get_field(lfs, 0), val, errno ? strerror(errno) : ""),
               READ_ERROR;

         bip->cols = (int)val;
         mode      = READ_ROWS;
         break;
      case READ_ROWS :
         if (lfs_used_fields(lfs) != 1)
            return fprintf(stderr, "Error line %d: Got %d fields, expected 1\n",
               lines, lfs_used_fields(lfs)),
               READ_ERROR;

         val = strtold(lfs_get_field(lfs, 0), &t);

         if (errno == ERANGE || isnan(val) || isinf(val) || rintl(val) != val || val < 1.0 || val > (long double)BIP_MAX_ROWS || *t != '\0')
            return fprintf(stderr, "Error line=%d Number of rows %s=%Lf out of range or not integer %s\n",
               lines, lfs_get_field(lfs, 0), val, errno ? strerror(errno) : ""),
               READ_ERROR;

         bip->rows = (int)val;
         mode          = READ_COEF;
         break;
      case READ_COEF :
         if (bip->read_rows >= bip->rows)
            return fprintf(stderr, "Error line=%d Expetced %d rows, got more\n",
               lines, bip->rows),
               READ_ERROR;

         if (lfs_used_fields(lfs) != bip->cols + 2)
            return fprintf(stderr, "Error line %d: Got %d fields, expected %d\n",
               lines, lfs_used_fields(lfs), bip->cols + 2),
               READ_ERROR;

         for(int col = 0; col < bip->cols + 2; col++)
         {
            if (col != bip->cols)
            {
               val = strtold(lfs_get_field(lfs, col), &t);

               if (errno == ERANGE || isnan(val) || isinf(val)
                  || (int_coef && rintl(val) != val)
                  || val > (long double)max_coef_val
                  || val < (long double)min_coef_val
                  || *t != '\0')
               return fprintf(stderr, "Error line=%d Number %s=%Lf out of range %s: %s\n",
                  lines, lfs_get_field(lfs, col), val, int_coef ? "or not integer" : "", errno ? strerror(errno) : ""),
                  READ_ERROR;

               if (col < bip->cols)
                  bip->a[bip->read_rows][col] = (Numb)val;
               else
                  bip->rhs[bip->read_rows] = (Numb)val;
            }
            else
            {
               const char* sense = lfs_get_field(lfs, bip->cols);
         
               if (!strcmp(sense, "<="))
                  bip->sense[bip->read_rows] = LE;
               else if (!strcmp(sense, ">="))
                  bip->sense[bip->read_rows] = GE;
               else if (!strcmp(sense, "=") || !strcmp(sense, "=="))
                  bip->sense[bip->read_rows] = EQ;
               else
                  return fprintf(stderr, "Error line=%d Expected <=, >=, ==, got \"%s\"\n",
                     lines, sense),
                     READ_ERROR;
            }
         }
         bip->read_rows++;
         break;
      default :
         abort();
      }
   }
   return mode;
}

/** Read a bip file.
 * Format example:
 *
 * 4 # cols (variables)
 * 3 # rows (constraints)
 * 2 3 5 4 <= 8
 * 3 6 0 8 <= 10
 * 0 0 1 1 <= 1
 *
 * comments (\#) and empty lines are ignored.
 *
 * @param filename name of file to read
 * @return ptr to BIP data structure
 */
BIP* bip_read(const char* filename)
{
   assert(NULL != filename);
   assert(0    <  strlen(filename));

   char      buf[MAX_LINE_LEN];
   char*     s;
   FILE*     fp;
   BIP*      bip   = calloc(1, sizeof(*bip));
   LFS*      lfs   = NULL;
   int       lines = 0;
   LINE_MODE mode  = READ_COLS;
   
   if (NULL == (fp = fopen(filename, "r")))
   {
      fprintf(stderr, "Can't open file %s\n", filename);
      free(bip);
      return NULL;
   }
   printf("Reading %s\n", filename);

   while(mode != READ_ERROR && NULL != (s = fgets(buf, sizeof(buf), fp)))
   {
      lines++;
      
      lfs  = lfs_split_line(lfs, s, "#");
      mode = process_line(mode, lfs, lines, bip);
   }
   fclose(fp);

   if (NULL != lfs)
      lfs_free(lfs);
   
   if (READ_ERROR == mode)
   {
      free(bip);

      return NULL;
   }
   if (bip->cols == 0 || bip->rows == 0 || bip->read_rows < bip->rows)
   {
      fprintf(stderr, "Error: unexpected EOF\n");
      free(bip);
      return NULL;
   }
   
   assert(bip->read_rows == bip->rows);

   assert(bip_is_valid(bip));
   
   printf("Read %d rows, %d cols\n", bip->read_rows, bip->cols);

   if (bip_can_overflow(bip))
   {
      bip_free(bip);

      return NULL;
   }   
   return bip;
}

/** Print Binary Program from BIP.
 */
void bip_print(const BIP* bip, FILE* fp)
{
   const char* sense[3] = { "<=", ">=", "==" };
      
   assert(NULL != fp);
   assert(bip_is_valid(bip));

   for(int r = 0; r < bip->rows; r++)
   {
      for(int c = 0; c < bip->cols; c++)
         fprintf(fp, "%Lg ", (long double)bip->a[r][c]);
      fprintf(fp, "%s %Lg\n", sense[bip->sense[r]], (long double)bip->rhs[r]);
   }
}

void bip_scale(BIP* bip)
{
   long double scale = 0.0;

   for(int r = 0; r < bip->rows; r++)
   {
      for(int c = 0; c < bip->cols; c++)
      {
         long double x = fabsl((long double)bip->a[r][c]) - floorl(fabsl((long double)bip->a[r][c]));

         if (x > 0.0)
         {
            printf("[%d,%d] %Lg\n", r, c, x);
         
            x = ceill(-log10l(x));
            
            if (x > scale)
               scale = x;
         }
      }
   }
   if (scale > 0)
   {
      scale = powl(10.0, scale);

      for(int r = 0; r < bip->rows; r++)
      {
         for(int c = 0; c < bip->cols; c++)
            bip->a[r][c] *= scale;

         bip->rhs[r] *= scale;
      }
      printf("Problem has been scaled with factor %Lg\n", scale);
   }
}

   
/** Check whether a particular vector is a feasible solution to a BIP.
 *  @return true if feasible, false otherwise.
 */
bool bip_is_feasible(const BIP* bip, const unsigned int x)
{
   assert(bip_is_valid(bip));
   
   int r;
   
   for(r = 0; r < bip->rows; r++)
   {
      unsigned int bit = 1;
      Numb         lhs = 0;
      
      for(int c = 0; c < bip->cols; c++)
      {
         if (x & bit)
            lhs += bip->a[r][c];

         assert(fetestexcept(FE_ALL_EXCEPT & ~FE_INEXACT) == 0);

         bit += bit;
      }
      assert(bip->sense[r] == EQ || bip->sense[r] == LE || bip->sense[r] == GE);
      
      if (  (bip->sense[r] == EQ && lhs != bip->rhs[r])
         || (bip->sense[r] == LE && lhs >  bip->rhs[r])
         || (bip->sense[r] == GE && lhs <  bip->rhs[r]))
         break;
   }
   return r == bip->rows;
}

int bip_rows(const BIP* bip)
{
   assert(bip_is_valid(bip));

   return bip->rows;
}

int bip_cols(const BIP* bip)
{
   assert(bip_is_valid(bip));

   return bip->cols;
}

Numb bip_a(const BIP* bip, int r, int c)
{
   assert(bip_is_valid(bip));
   assert(r >= 0);
   assert(r <  bip->rows);
   assert(c >= 0);
   assert(c <  bip->cols);

   return bip->a[r][c];
}

Numb bip_rhs(const BIP* bip, int r)
{
   assert(bip_is_valid(bip));
   assert(r >= 0);
   assert(r <  bip->rows);

   return bip->rhs[r];
}

Sense bip_sense(const BIP* bip, int r)
{
   assert(bip_is_valid(bip));
   assert(r >= 0);
   assert(r <  bip->rows);
   
   return bip->sense[r];
}
