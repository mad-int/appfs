OBJDIR = obj
SRCDIR = src
OBJ = bip.o allocate.o numb.o constraint.o constraintArray.o
MAINOBJFILES= $(addprefix $(OBJDIR)/, $(OBJ)) 
COVFLAGS= -fprofile-arcs -ftest-coverage
CCFLAGS = -I$(OBJDIR) -Wall -Werror $(COVFLAGS)
MAIN = ex5

all: $(MAIN)

cov: CCFLAGS += $(COVFLAGS)
cov: $(MAIN)

int_coeffs: CCFLAGS += -DINT_COEFFS
int_coeffs: $(MAIN)

debug: CCFLAGS += -g
debug: $(MAIN)

int_coeffs_debug: CCFLAGS += -DINT_COEFFS -g
int_coeffs_debug: $(MAIN)

$(MAIN): $(MAINOBJFILES)	
	gcc $(CCFLAGS) -o $(MAIN) $+
	
# % sind pattern ersetzungen, $< entspricht der ersten Abhaengigkeit
$(OBJDIR)/%.o: $(SRCDIR)/%.c $(OBJDIR) 
	gcc $(CCFLAGS) -c $< -o $@

$(OBJDIR):
	mkdir -p $(OBJDIR)

clean:
	rm $(MAINOBJFILES) -rf;
	rmdir $(OBJDIR);
	rm -f $(MAIN);