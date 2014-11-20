P=ex4
OBJECTS=ex4.o instance.o allocate.o permute.o
CFLAGS = -g -Wall -O3
LDLIBS=
CC=gcc
$(P): $(OBJECTS)
	$(CC) -o $(CFLAGS) -D DOUBLE $(P) $(OBJECTS)

$.o: $.c
	$(CC) -c $<