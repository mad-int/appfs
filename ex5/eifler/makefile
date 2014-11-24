P=ex5
OBJECTS=ex5.o instance.o allocate.o
CFLAGS = -g -O3 -D DOUBLE
LDLIBS=
CC=gcc
$(P): $(OBJECTS)
	$(CC) -o $(P) $(CFLAGS)  $(OBJECTS)

$.o: $.c
	$(CC) -c $<
