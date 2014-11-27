P=ex6.pre
OBJECTS=ex5.o instance.o allocate.o
CFLAGS = -g -O3 -Wall -D PREORDER -std=c99 -fprofile-arcs -ftest-coverage
LDLIBS=
CC=gcc
$(P): $(OBJECTS)
	$(CC) -o $(P) $(CFLAGS)  $(OBJECTS)

$.o: $.c
	$(CC) -c $<

clean:
	rm -f *.o ex6