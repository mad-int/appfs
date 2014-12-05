BIN = ex5
CC = gcc
WARNINGS = -Wall -Wextra -Wpedantic -Wshadow -Wpointer-arith -Wcast-align \
			-Wwrite-strings -Wredundant-decls -Wnested-externs -Winline -Wno-long-long \
			-Wuninitialized -Wconversion -Wbad-function-cast -Wcast-qual
CFLAGS_MIN = -O3 -std=c99 -DNDEBUG $(WARNINGS)
CFLAGS = -std=c99 -fprofile-arcs -ftest-coverage $(WARNINGS) -pg
BIN_SRC = src/allocate.c src/ex7_main.c src/linear_program.c src/num_type.c src/read_lp.c
TEST_SRC = src/allocate.c tests/test_linear_program.c src/num_type.c src/read_lp.c
TEST_OUT = test_linear_program
LFLAGS = -lcmocka

all:
	$(CC) $(CFLAGS) $(BIN_SRC) -o $(BIN)

check:
	$(CC) $(CFLAGS) $(LFLAGS) $(TEST_SRC) -o tests/$(TEST_OUT)
	(cd tests; ./$(TEST_OUT))

use_double:
	$(CC) $(CFLAGS) -DUSE_DOUBLE $(BIN_SRC) -o $(BIN)

minimal:
	$(CC) $(CFLAGS_MIN) $(BIN_SRC) -o $(BIN)

clean:
	rm ex5
