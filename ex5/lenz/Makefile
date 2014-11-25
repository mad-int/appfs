SHELL = /bin/bash
CC = gcc

OBJECTS = main.o allocate.o readfile.o solutions.o

ex5: $(OBJECTS)
	$(CC) -o ex5  $(OBJECTS)
	
# compilation commands
main.o: allocate.h readfile.h solutions.h
allocate.o: allocate.h readfile.h
solutions.o: allocate.h solutions.h
readfile.o: allocate.h readfile.h

.PHONY: clean
clean:
	-rm ex5 $(OBJECTS)

check:
	./check.sh
