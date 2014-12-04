/**
 * @file   gray_code.h
 * @brief  Contains functions to deal with the standard gray code.
 * @author Hendrik Schrezenmaier
 * @date   02 Dez 2014
 */

#ifndef GRAY_CODE_H
#define GRAY_CODE_H

/**
 * Calculates the bit that shifts when going to the code word with the given index.
 * 
 * @return the bit that shifts
 */
extern int gray_get_changed_bit(
      unsigned long index ///< the index of the new code word
                               );

/**
 * Decides whether a zero switches to a one or a one switches to a zero when going to the code word with
 * the given index.
 * 
 * @return 1 if a zero switches to a one, -1 otherwise
 */
extern int gray_get_sign(
      unsigned long index, ///< the index of the new code word
      int changed_bit ///< the bit that changes
                        );

/**
 * Calculates the code word with the given index.
 * 
 * @return the code word with the given index
 */
extern unsigned long gray_get_code(
      unsigned long index ///< the index of the code word to be computed
                                  );

#endif /* GRAY_CODE_H */
 
