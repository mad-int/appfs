CC = gcc -g -Wall -ftest-coverage -fprofile-arcs -p
#CFLAGS =

# directories
OBJDIR = obj
SRCDIR = src

# files to be built
OBJECTS = main.o allocate.o readfile.o solutions.o bp.o
MAINOBJFILES= $(addprefix $(OBJDIR)/, $(OBJECTS))

MAIN = ex5

# targets
.PHONY: all
$(MAIN): $(MAINOBJFILES)
	 @$(CC) $(CFLAGS) -o $(MAIN) $+

# create directory obj
$(OBJDIR):
	@mkdir -p $(OBJDIR)

# build object files
$(OBJDIR)/%.o:$(SRCDIR)/%.c $(OBJDIR)
	@echo "-> Building CC object $@"
	@$(CC) -c $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	rm ex5 $(MAINOBJFILES)
	rm $(OBJDIR)/*.gcno
	rm -rf $(OBJDIR)/*.gcda
	rmdir $(OBJDIR)

check:
	./check.sh

valgrind:
	./valgrind.sh