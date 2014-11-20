P=ex4
OBJECTS=ex4.o instance.o allocate.o permute.o
CFLAGS = -g -Wall -O3
LDLIBS=
CC=c99
$(P): $(OBJECTS)
	$(CC) -o $(P) $(CFLAGS) -D DOUBLE $(OBJECTS)

$.o: $.c
	$(CC) -c $<
