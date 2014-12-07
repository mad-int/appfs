CC = gcc
CFLAGS = -Wall -Wextra -O3 -g -std=c99 
MODE = DOUBLE

feasibles: ex5.o allocate.o ilpParser.o linearProgram.o
				gcc $(CFLAGS) -o feasibles ex5.o allocate.o ilpParser.o linearProgram.o

ex5.o:
		$(CC) $(CFLAGS) -c -D $(MODE) ex5.c
		
allocate.o: allocate.c
		$(CC) $(CFLAGS) -c -D $(MODE) allocate.c
		
ilpParser.o: ilpParser.c
		$(CC) $(CFLAGS) -c -D $(MODE) ilpParser.c
		
linearProgram.o: linearProgram.c
		$(CC) $(CFLAGS) -c -D $(MODE) linearProgram.c