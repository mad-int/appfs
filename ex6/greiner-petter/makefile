makefile:
all: clean BIPSolver

clean: 
	-rm BIPSolver.o BinaryProgram.o BIPSolver.exe

BIPSolver: BIPSolver.o BinaryProgram.o
	gcc -o BIPSolver BIPSolver.o BinaryProgram.o

BIPIterator.o: BIPSolver.c
	gcc -c BIPSolver.c

BinaryProg.o: BinaryProgram.c
	gcc -c BinaryProgram.c