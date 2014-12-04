/**
 * @file   linear_program.h
 * @brief  Contains a class for linear programs and functions to deal with them.
 * @author Hendrik Schrezenmaier
 * @date   02 Dez 2014
 */

#ifndef LINEAR_PROGRAM_H
#define LINEAR_PROGRAM_H

/**
 * Represents a linear program Ax <= b.
 */
typedef struct linear_program LinearProgram;

/**
 * Prints the given LP to the standard output.
 */
extern void print_lp(
      LinearProgram* lp ///< the LP to be printed
                    );

/** 
 * Enumerates all feasible solutions of the given LP and writes them to the given file.
 * 
 *  For faster evaluation gray codes are used. This might lead to numerical problems
 *  when floating point arithmetic is used.
 */
extern void print_feasible_binary_lp(
      LinearProgram* lp, ///< the LP whose solutions will be printed
      char* file_name ///< the path to the output file
                                    );

/**
 * Reads a LP from a text file.
 * 
 * @return the LP object
 */
extern LinearProgram* read_from_file_lp(
      const char* file_name ///< the path to the file
                                       );

/**
 * Deallocates the space of the given LP.
 */
extern void free_lp(
      LinearProgram* lp ///< the LP whose space will be deallocated
                   );

#endif /* LINEAR_PROGRAM_H */
