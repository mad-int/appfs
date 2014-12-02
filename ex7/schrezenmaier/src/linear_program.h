#ifndef LINEAR_PROGRAM_H
#define LINEAR_PROGRAM_H

typedef struct linear_program LinearProgram;

extern void print_lp(LinearProgram* lp);

extern void print_feasible_binary_lp(LinearProgram* lp, char* file_name);

extern LinearProgram* read_from_file_lp(const char* file_name);

#endif /* LINEAR_PROGRAM_H */
