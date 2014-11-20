CC = gcc
CFLAGS = -Wall -O3 -g
MODE = DOUBLE

feasibles: ex5.o allocate.o ilpParser.o linearProgram.o
				gcc -o feasibles ex5.o allocate.o ilpParser.o linearProgram.o

ex5.o:
		$(CC) -c -D $(MODE) ex5.c
		
allocate.o: allocate.c
		$(CC) -c -D $(MODE) allocate.c
		
ilpParser.o: ilpParser.c
		$(CC) -c -D $(MODE) ilpParser.c
		
linearProgram.o: linearProgram.c
		$(CC) -c -D $(MODE) linearProgram.c