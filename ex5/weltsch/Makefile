BIN = ex4
CC = gcc
CFLAGS = -Wall -Wextra -O3 -std=c99
BIN_SRC = src/allocate.c src/ex4_main.c src/linear_program.c
TEST_SRC = src/allocate.c tests/test_linear_program.c
TEST_OUT = test_linear_program
LFLAGS = -lcmocka

all:
	$(CC) $(CFLAGS) $(BIN_SRC) -o $(BIN)

test:
	$(CC) $(LFLAGS) $(TEST_SRC) -o tests/$(TEST_OUT)
	(cd tests; ./$(TEST_OUT))
