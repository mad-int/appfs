/**
 * @file   gray_code.c
 * @brief  Contains functions to deal with the standard gray code.
 * @author Hendrik Schrezenmaier
 * @date   02 Dez 2014
 */

#include <assert.h> // assert

/**
 * Calculates the bit that shifts when going to the code word with the given index.
 * 
 * @return the bit that shifts
 */
int gray_get_changed_bit(
      unsigned long index ///< the index of the new code word
                        )
{
   assert(index >= 1);
   
   return __builtin_ffsl(index) - 1;
}

/**
 * Decides whether a zero switches to a one or a one switches to a zero when going to the code word with
 * the given index.
 * 
 * @return 1 if a zero switches to a one, -1 otherwise
 */
int gray_get_sign(
      unsigned long index, ///< the index of the new code word
      int changed_bit ///< the bit that changes
                 )
{
   assert(index >= 1);
   assert(changed_bit == gray_get_changed_bit(index));
   
   if(((1ul << (changed_bit + 1)) & index) == 0ul)
      return 1;
   else
      return -1;
}

/**
 * Calculates the code word with the given index.
 * 
 * @return the code word with the given index
 */
unsigned long gray_get_code(
      unsigned long index ///< the index of the code word to be computed
                           )
{
   return index ^ (index >> 1);
}