BIN = ex1
SRC = print_numbers.c

CC	     = cc
CFLAGS   = -g -Wall -Wextra -O2
LFLAGS   = -lm
LFLAGS  += -lpthread

GDB    = gdb
RM     = rm -f

OBJS = $(SRC:%.c=%.o)

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LFLAGS) -o $@

clean:
	$(RM) $(BIN) $(OBJS) *.d
