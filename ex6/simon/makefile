OBJDIR = obj
SRCDIR = src
CHECKDIR = checkData
OBJ = bip.o allocate.o numb.o constraintArray.o readConstArray.o
MAINOBJFILES= $(addprefix $(OBJDIR)/, $(OBJ)) 
COVFLAGS= -fprofile-arcs -ftest-coverage
CCFLAGS = -I$(OBJDIR) -Wall -Wextra -Wbad-function-cast -Wcast-qual -Wcast-align -Wfloat-equal -Wconversion -Wshadow #-Werror
CCEXTRAWARNS = -pedantic -Wwrite-strings 
MAIN = ex6
# flags
# OPT in { DEFAULT | DEBUG }
OPT = DEFAULT
# COEFFS in { DOUBLE | INT }
COEFFS = DOUBLE
# PRINT in { COUNT | SOLS }
PRINT = COUNT

all: $(MAIN)

cov: CCFLAGS += $(COVFLAGS)
cov: $(MAIN)

ifeq ($(COEFFS), INT)
CCFLAGS += -DINT_COEFFS
endif

ifeq ($(OPT), DEBUG)
CCFLAGS += -g
endif

ifeq ($(PRINT), SOLS)
CCFLAGS += -DPRINT_SOLUTIONS
endif

$(MAIN): $(MAINOBJFILES)
	gcc $(CCFLAGS) -o $(MAIN) $+
	
# % sind pattern ersetzungen, $< entspricht der ersten Abhaengigkeit
$(OBJDIR)/%.o: $(SRCDIR)/%.c $(OBJDIR) 
	gcc $(CCFLAGS) -c $< -o $@

$(OBJDIR):
	mkdir -p $(OBJDIR)

clean:
	rm $(MAINOBJFILES) -rf;
	rm $(OBJDIR)/*.gcno -f;
	rm $(OBJDIR)/*.gcda -f;
	rmdir $(OBJDIR);
	rm -f $(MAIN);
	#coverage output
	rm -rf cov;
	rm -rf *.png;
	rm -rf index*.html
	rm -rf $(SRCDIR)/*.html
	rm -rf gcov.css;

check: CCFLAGS += -DPRINT_SOLUTIONS
check: clean $(MAIN)
	./runTestFiles.sh $(MAIN) $(CHECKDIR)