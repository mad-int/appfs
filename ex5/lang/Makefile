CC=gcc -std=c99 -fprofile-arcs -ftest-coverage
int: 
	$(CC) allocate.c calculations.c checks.c logic_double.c logic_int.c main.c -lm
	ln -s a.out ex5
double: 
	$(CC) allocate.c calculations.c checks.c logic_double.c logic_int.c main.c -D USE_DBL -lm
	ln -s a.out ex5