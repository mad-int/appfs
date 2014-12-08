CC = gcc
CFLAGS = -Wall -O3 -fprofile-arcs -ftest-coverage
# INTEGER for integer arithmetic or DOUBLE for double arithmetic
MODE = DOUBLE

prog: ex4.o linear_program.o allocate.o
	$(CC) $(CFLAGS) -o prog ex4.o linear_program.o allocate.o
	ln -sf prog ex5

ex4.o: src/ex4.c
	$(CC) $(CFLAGS) -c src/ex4.c

linear_program.o: src/linear_program.c
	$(CC) $(CFLAGS) -D $(MODE) -c src/linear_program.c

allocate.o: src/allocate.c
	$(CC) $(CFLAGS) -c src/allocate.c

clean:
	@rm -v *.o ex5