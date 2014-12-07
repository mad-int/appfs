CC = gcc
GCCWARN		=	-Wall -Wextra -W -Wpointer-arith -Wcast-align -Wwrite-strings \
			-Wstrict-prototypes -Wmissing-prototypes \
			-Wmissing-declarations -Wshadow -Waggregate-return \
			-Wno-unknown-pragmas -Wuninitialized -Wbad-function-cast
			
CFLAGS = -O3 -std=c99 -fprofile-arcs -ftest-coverage $(WARNINGS) -pg
			
MODE = DOUBLE

TESTFILE_PATH = ../../data/
ERROR_TESTFILES = error_test1.dat error_test2.dat error_test3.dat error_test4.dat error_test5.dat error_test6.dat \
	error_test7.dat error_test8.dat error_test9.dat error_test10.dat error_test11.dat corrupted_noRHS.dat \
	corrupted_toFewRows.dat
CORRECT_TESTFILES = markshare_3_0.dat markshare_3_1.dat test1.dat test2.dat test3.dat onlyEq.dat geqTest.dat
CORRECT_SOLUTIONS_PATH = ../../data/solutions/

BIN_SRC = src/allocate.c src/ex7.c src/linearProgram.c src/ilpParser.c
OBJ = ex7.o ilpParser.o linearProgram.o allocate.o

BIN = ex7
COV = allocate.gcda allocate.gcno linearProgram.gcno ex7.gcda ex7.gcno ilpParser.gcno ilpParser.gcda linearProgram.gcda


all:
	$(CC) $(CFLAGS) $(BIN_SRC) -o $(BIN)
double:
	$(CC) $(CFLAGS) -D$(MODE) $(BIN_SRC) -o $(BIN)

		
check: $(BIN) #TODO 
#	for file in $(ERROR_TESTFILES); do \
#		echo $$file:; \
#		./$(BIN) $(TESTFILE_PATH)$$file > check_output.dat; \
		
	
clean:
	rm ex7
	rm $(COV)