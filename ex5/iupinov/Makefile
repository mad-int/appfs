TARGET = ex5
# LIBS = -lm
CC = gcc
CFLAGS = -g -std=c99 -Wall -Wextra -Wpedantic -O3 -DNDEBUG -DUSE_DBL --coverage

TESTS_DIR=../../data
TESTS_TEMPLATES=$(TESTS_DIR)/corrupt* $(TESTS_DIR)/error* $(TESTS_DIR)/test*
TESTS="" $(shell find $(TESTS_TEMPLATES)) 

.PHONY: default all clean

default: $(TARGET)
all: default

# OBJECTS = $(patsubst %.c, %.o, $(wildcard *.c))
OBJECTS = $(TARGET).c ineq.c
# HEADERS = $(wildcard *.h)
HEADERS = ineq.h

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) $(CFLAGS) $(LIBS) -o $@

check: clean all
	for test in $(TESTS) ; do \
		echo Testing $$test;\
		./$(TARGET) $$test ; \
	done
	lcov -d . -c -o coverage.info
	genhtml -o coverage coverage.info

clean:
	-rm -f *.o
	-rm -f *.gc*
	-rm -f $(TARGET)
	-rm -f coverage.info
	-rm -rf coverage
