CC=gcc -std=c99 -fprofile-arcs -ftest-coverage
int: 
	$(CC) allocate.c calculations.c checks.c logic.c main.c -lm
	ln -s a.out ex6
double: 
	$(CC) allocate.c calculations.c checks.c logic.c main.c -D USE_DBL -lm
	ln -s a.out ex6