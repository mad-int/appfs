P=ex6
OBJECTS=ex6.o instance.o allocate.o
SOURCE=ex6.c instance.c	allocate.c
CFLAGS = -g -O3 -Wall -D PREORDER -std=c99 -fprofile-arcs -ftest-coverage
LDLIBS=
CC=gcc
$(P): $(OBJECTS)

int:
	$(CC) -c $(CFLAGS) $(SOURCE)
	$(CC) -o $(P) $(CFLAGS)  $(OBJECTS)

	
double:
	$(CC) -c $(CFLAGS) -D DOUBLE $(SOURCE)
	$(CC) -o $(P) $(CFLAGS) -D DOUBLE  $(OBJECTS)

clean:
	rm -f *.o ex6 *.gcda *.gcno
	rm -rf  cov

test:
	sh test.sh