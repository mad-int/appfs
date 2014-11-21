CC = gcc
CFLAGS = -Wall -g
#CFLAGS = -Wall -g -DUSE_INT

# default target
default: ex5

# ----

ex5: ex5.c constr.o
	$(CC) $(CFLAGS) ex5.c constr.o -o ex5

constr.o: constr.c constr.h
	$(CC) -c $(CFLAGS) constr.c -o constr.o

check:
	./run_checks.sh

clean:
	rm -f *.o ex5

