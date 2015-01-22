#-----------------------------------------------------------------------------
# paths
#-----------------------------------------------------------------------------

SHELL		=	/bin/bash

OPT		=	opt
EX		=	ex9
COMPILER	=	gnat
#COVFLAGS	=	-fprofile-arcs -ftest-coverage
#COVLINKFLAGS    =	-fprofile-arcs

ifeq ($(OPT),dbg)
MAKE		=	-g
else
MAKE		=	-O3 -ffast-math -fomit-frame-pointer
# -O3
endif

ifneq ($(COMPILER),gcc)
COMPILER	=	gnat
endif

BASE		=	$(OPT)

OBJDIR		=	obj/O.$(BASE).$(COMPILER)
BINDIR		=	bin
SRCDIR		=	src

MAINNAME	=	$(EX).$(BASE).$(COMPILER)

MAINOBJ		=	main.o \
			binary_integer_program.o \
			linear_constraints.o \
			vector.o

MAIN		=	main

MAINOBJFILES	=	$(addprefix $(OBJDIR)/,$(MAINOBJ))

ifeq ($(COMPILER),gcc)
WFLAGS		=	-Wextra -Wpedantic -Wuninitialized -Wcast-align -Wstrict-overflow=0 -Wstrict-overflow=1 -Wstrict-overflow=2 -Wstrict-overflow=3 -Wstrict-overflow=4 -Wstrict-overflow=5 -Wsuggest-attribute=pure -Wsuggest-attribute=noreturn
#  -Wsuggest-attribute=const
COMP		=	gcc -Wall -Werror $(WFLAGS) -std=c99 -gnat2012
INCDIR		=	-I $(SRCDIR)
OUTPUT		=	-o $@
BIND		=	gnatbind
LINK		=	gnatlink
else
COMP		=	gnatmake -gnat2012
OUTPUT		=	-D $(OBJDIR)
INCDIR		=	-aI$(SRCDIR)
BIND		=	gnatmake -b -q
LINK		=	gnatmake -l -q -aO$(OBJDIR)
endif

TEST		=	big

#-----------------------------------------------------------------------------
# make
#-----------------------------------------------------------------------------

.PHONY: all
all:		$(EX)

$(OBJDIR):
		@echo "create folder $@"
		@-mkdir -p $(OBJDIR)

$(BINDIR):
		@echo "create folder $@"
		@-mkdir -p $(BINDIR)

$(OBJDIR)/%.o:	$(SRCDIR)/%.adb
		@echo "-> compiling $@"
		$(COMP) $(MAKE) $(COVFLAGS) $(INCDIR) -c $< $(OUTPUT)

.PHONY: $(EX)
$(EX):		$(OBJDIR) $(BINDIR) $(MAINOBJFILES)
		@echo "-> binding $@"
		$(BIND) $(MAKE) $(MAIN) -aO$(OBJDIR) -aI$(SRCDIR)
		@echo "-> linking $@"
		$(LINK) $(MAKE) $(COVLINKFLAGS)  $(OBJDIR)/$(MAIN) -o $(BINDIR)/$(MAINNAME)
		ln -sf $(MAINNAME) $(BINDIR)/$(EX)
		ln -sf $(BINDIR)/$(EX) $(EX)

.PHONY: clean
clean:		$(OBJDIR)
ifneq ($(OBJDIR),)
		@-(rm -f $(OBJDIR)/*.ali && rm -f $(OBJDIR)/*.o && rm -f $(OBJDIR)/*.gcda && rm -f $(OBJDIR)/*.gcno && rmdir $(OBJDIR));
		@echo "-> remove main objective files"
endif
ifneq ($(BINDIR),)
		@-(rm -f $(BINDIR)/* && rmdir $(BINDIR));
		@echo "-> remove binary"
		@echo "-> remove files"
endif

.PHONY: check
check:
		#$(SHELL) ./obj/main $(TEST)
		./ex9 $(TEST)

.PHONY: test
test:		check