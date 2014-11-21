TARGET = ex5
# LIBS = -lm
CC = gcc
# use -DUSE_DBL for double data type
CFLAGS = -g -Wall -O3 -DUSE_DBL

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

clean:
	-rm -f *.o
	-rm -f $(TARGET)
