SHELL = /bin/bash
CC = gcc -Wall -ftest-coverage -fprofile-arcs -p

OBJECTS = main.o allocate.o readfile.o solutions.o bp.o

ex5: $(OBJECTS)
	$(CC) -o ex5  $(OBJECTS)
	
# compilation commands
main.o: allocate.h readfile.h solutions.h bp.h
allocate.o: allocate.h
solutions.o: allocate.h solutions.h bp.h
readfile.o: allocate.h readfile.h bp.h def.h
bp.o: bp.h def.h

.PHONY: clean
clean:
	-rm ex5 $(OBJECTS) *.gcda *.gcno gmon.out

check:
	./check.sh

valgrind:
	./valgrind.sh