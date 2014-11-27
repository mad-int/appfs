BIN = ex5
CC = gcc
CFLAGS_MIN = -O3 -std=c99 -DNDEBUG
CFLAGS = -Wall -Wextra -O3 -std=c99 -fprofile-arcs -ftest-coverage
BIN_SRC = src/allocate.c src/ex5_main.c src/linear_program.c src/num_type.c
TEST_SRC = src/allocate.c tests/test_linear_program.c src/num_type.c
TEST_OUT = test_linear_program
LFLAGS = -lcmocka

all:
	$(CC) $(CFLAGS) $(BIN_SRC) -o $(BIN)

test:
	$(CC) $(LFLAGS) $(TEST_SRC) -o tests/$(TEST_OUT)
	(cd tests; ./$(TEST_OUT))

use_double:
	$(CC) $(CFLAGS) -DUSE_DOUBLE $(BIN_SRC) -o $(BIN)

minimal:
	$(CC) $(CFLAGS_MIN) $(BIN_SRC) -o $(BIN)

clean:
	rm ex5
