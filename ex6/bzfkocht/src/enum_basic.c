/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*                                                                           */
/*   File....: enum_basic.c                                                  */
/*   Name....: Basic (plain C) enumeration routine                           */
/*   Author..: Thorsten Koch                                                 */
/*   Copyright by Author, All rights reserved                                */
/*                                                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Copyright (C) 2007 by Thorsten Koch <koch@zib.de>
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "mshell.h"
#include "bip.h"
#include "enum_basic.h"

#define USE_INTRINSIC    1
//#define USE_DEBRUIJN   1 
//#define USE_BITMASK    1
//#define USE_LOOP       1  // (default)

#define USE_TWO_PASS             1  // not one pass (default)
//#define USE_ONE_PASS           1  // test after loop
//#define USE_ONE_PASS           2  // Test in loop

#define UNROLL_UPDATE  1      



#ifdef USE_DEBRUIJN

#define DEBRUIJN32  0x077CB531U

static int index32[32] =
{ 
    0,  1, 28,  2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17,  4, 8,
   31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18,  6, 11,  5, 10, 9
}; 
#endif // USE_DEBRUIJN

/* Ax == b
 * Ax <= b
 * >= => * -1
 *
 * @return Number of feasible solutions.
 */
unsigned int enumerate_basic(
   const BIP*         bip,
   const int          cols,
   const int          rows,
   const int          equations,
   const Numb**       col, /* [cols] */
   const Numb*        rhs, /* [rows] */
   void  (*report_sol)(const BIP* bip, unsigned int x))
{
   assert(sizeof(int) == 4);
   assert(rows        >= 1);
   assert(rows % 4    == 0);
   assert(cols        >= 1);
   assert(cols        <= 32);
   assert(equations   >= 0);
   assert(equations   <= rows);
   assert(col         != NULL);
   assert(rhs         != NULL);

#if defined(USE_ONE_PASS)
   unsigned int invalid;
#endif
   unsigned int  sol_count = 0;
   unsigned int  x         = 0;  /* current bitvector */
   unsigned int  n         = 0;  /* node number  */
   unsigned int  negn;    /* negn = -n */
   const Numb*   modcol;
   int           k;
   Numb*         r = malloc((size_t)rows * sizeof(*r)); /* residium / activity */
   
   for(k = 0; k < rows; k++)
      r[k] = -rhs[k];

   /* Check whether 0 is a feasible solution
    */
   for(k = 0; (k < equations) && (r[k] == 0.0); k++)
      ;

   if (k == equations)
      for(; (k < rows) && (r[k] <= 0.0); k++)
         ;
   
   if (k == rows)
   {
      (*report_sol)(bip, x);
      sol_count++;
   }
   /* Starting with x = 0000, n = 0001, negn = 1111, the algorithm
    * enumerates all x vectors by always doing only one flip, which means
    * we only have to update the residium for a single column.
    * This is done as follows:
    *   1. updatemask = n & negn (always only one bit set!)
    *   2. modcol = bit number which is set in updatemask
    *   2. n++, negn--
    *   2. xor-bitflip: x ^= updatemask
    *   3. add or substract col[modcol] from activities and check feasibility
    *   4. as long as negn != 0, goto 1.
    * Thereby, the x vectors are scanned in the following order:
    *   0000, 0001, 0011, 0010, 0110, 0111, 0101, 0100, 
    *   1100, 1101, 1111, 1110, 1010, 1011, 1001, 1000
    */
   n     = 1;
   negn  = 1 << (cols - 1);
   negn += (negn - 1);
   
   while(negn != 0)
   {
      unsigned int updatemask = n & negn;

#if defined(USE_INTRINSIC)

      /* This builtin method by GCC determines the count of trailing zero in the binary representation of a number.
       * 
       * The Syntax:  int __builtin_ctz (unsigned int x)
       * The input parameter is the number for which the the count of trailing zero’s is to be determined.
       * It returns the count of trailing zero’s as expected.
       */
      unsigned int colidx = __builtin_ctz(updatemask);
      
#elif defined(USE_DEBRUIJN)

      /* http://en.wikipedia.org/wiki/De_Bruijn_sequence
       */
      unsigned int colidx = index32[(uint32_t)(updatemask * DEBRUIJN32) >> 27];

#elif defined(USE_BITMASK)
      
      unsigned int colidx = 0;  
      
      if ((updatemask & 1) == 0)
      {
         if (updatemask & 0xffff0000)
            colidx += 16;
         if (updatemask & 0xff00ff00)
            colidx += 8;
         if (updatemask & 0xf0f0f0f0)
            colidx += 4;
         if (updatemask & 0xcccccccc)
            colidx += 2;
         if (updatemask & 0xaaaaaaaa)
            colidx += 1;
      }
#else // USE_LOOP (standard approach)
      
      unsigned int colidx;  
      unsigned int bit = 1;

      for(colidx = 0; colidx < sizeof(updatemask) * 8; colidx++, bit += bit)
         if (updatemask & bit)
            break;
      
#endif // USE_LOOP

      /* ((n & negn) == (n & -n)) */
      assert(((n + negn) & ((1 << (cols - 1)) + ((1 << (cols - 1)) - 1))) == 0);

      ++n;
      --negn;
      x ^= updatemask;

      modcol = col[colidx];
      
#if defined(USE_ONE_PASS) 

      invalid = 0;

#if USE_ONE_PASS == 1
      
      /* check whether bit changed form 0 to 1
       */
      if (x & updatemask)
      {
         for(k = 0; k < equations; k++)
            invalid |= (r[k] += modcol[k]) != 0.0; 
         for(; k < rows; k++)
            invalid |= (r[k] += modcol[k]) > 0.0; 
      }
      else /* bit changed from 1 to 0 */
      {
         for(k = 0; k < equations; k++)
            invalid |= (r[k] -= modcol[k]) != 0.0; 
         for(; k < rows; k++)
            invalid |= (r[k] -= modcol[k]) > 0.0; 
      }
      if (!invalid)
      {
         (*report_sol)(bip, x);
         sol_count++;
      }

#else // USE_ONE_PASS == 2
      
      invalid = 0;
         
      /* check whether bit changed form 0 to 1
       */
      if (x & updatemask)
      {
         for(k = 0; k < equations && !invalid; k++)
            invalid |= (r[k] += modcol[k]) != 0.0; 
         for(; k < rows && !invalid; k++)
            invalid |= (r[k] += modcol[k]) > 0.0; 
      }
      else /* bit changed from 1 to 0 */
      {
         for(k = 0; k < equations && !invalid; k++)
            invalid |= (r[k] -= modcol[k]) != 0.0; 
         for(; k < rows && !invalid; k++)
            invalid |= (r[k] -= modcol[k]) > 0.0; 
      }
      if (!invalid)
      {
         (*report_sol)(bip, x);
         sol_count++;
      }

#endif // USE_ONE_PASS == 2
      
#else /* !ONEPASS */
      /* check wheather bit changed form 0 to 1
       */
#if !defined(UNROLL_UPDATE)
      
      if (x & updatemask)
      {
         for(k = 0; k < rows; k++)
            r[k] += modcol[k];
      }
      else /* bit changed from 1 to 0 */
      {
         for(k = 0; k < rows; k++)
            r[k] -= modcol[k];
      }
      
#else // UNROLL_UPDATE
      
      if (x & updatemask)
      {
         for(k = 0; k < rows; k += 4)
         {
            r[k + 0] += modcol[k + 0];
            r[k + 1] += modcol[k + 1];
            r[k + 2] += modcol[k + 2];
            r[k + 3] += modcol[k + 3];
         }
      }
      else /* bit changed from 1 to 0 */
      {
         for(k = 0; k < rows; k += 4)
         {
            r[k + 0] -= modcol[k + 0];
            r[k + 1] -= modcol[k + 1];
            r[k + 2] -= modcol[k + 2];
            r[k + 3] -= modcol[k + 3];
         }
      }
      
#endif // UNROLL_UPDATE
      
      /* Check feasibility
       */
      for(k = 0; (k < equations) && (r[k] == 0.0); k++)
         ;

      if (k == equations)
      {
         for(; (k < rows) && (r[k] <= 0.0); k++)
            ;

         if (k == rows)
         {
            (*report_sol)(bip, x);
            sol_count++;
         }
      }

#endif // ONEPASS
      
   }
   free(r);

   return sol_count;
}

