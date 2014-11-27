# compiler settings
CC = gcc
CFLAGS = -Wall -O1

# directories
SRCDIR = src
OBJDIR = obj

# files
OBJ = BIPSolver.o BinaryProgram.o
HEADERS = BinaryProgram.h
MAINOBJFILES = $(addprefix $(OBJDIR)/, $(OBJ))
MAINHEADERS = $(addprefix $(SRCDIR)/, $(HEADERS))

# name definitions
MAIN = BIPSolver

# clean -> build -> check results
.PHONY: all
all: clean $(MAIN) check

# link all object files
$(MAIN): $(MAINOBJFILES)
	@$(CC) $(CFLAGS) -o $(MAIN) $+
	@ln -sf $(MAIN) ex6

# build all object files
$(OBJDIR)/%.o: $(SRCDIR)/%.c $(OBJDIR)
	@echo "-> Compile $<"
	@$(CC) -c $(CFLAGS) $< -o $@

# if necessary create obj folder
$(OBJDIR):
	@mkdir -p $(OBJDIR)
	@echo "-> Create $(OBJDIR) directory."

# check execution file exists after build process
check: $(MAIN).exe
	@echo "-> Check"

# clean project, delete 
clean: 
	@rm -rf $(MAINOBJFILES);
	@rm -f $(MAIN).exe;
	@echo "-> Clean project."