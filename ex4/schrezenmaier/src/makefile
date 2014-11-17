CC = gcc
CFLAGS = -Wall -O2

prog: ex4.o linear_program.o allocate.o
	$(CC) $(CFLAGS) -o prog ex4.o linear_program.o allocate.o

ex4.o: ex4.c
	$(CC) -c ex4.c

linear_program.o: linear_program.c
	$(CC) -c linear_program.c

allocate.o: allocate.c
	$(CC) -c allocate.c