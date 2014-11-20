CC = gcc
CFLAGS = -Wall -O2
# INTEGER for integer arithmetic or DOUBLE for double arithmetic
MODE = DOUBLE

prog: ex4.o linear_program.o allocate.o
	$(CC) $(CFLAGS) -o prog ex4.o linear_program.o allocate.o

ex4.o: src/ex4.c
	$(CC) -c src/ex4.c

linear_program.o: src/linear_program.c
	$(CC) -D $(MODE) -c src/linear_program.c

allocate.o: src/allocate.c
	$(CC) -c src/allocate.c