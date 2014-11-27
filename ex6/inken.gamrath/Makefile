#-----------------------------------------------------------------------------
# paths
#-----------------------------------------------------------------------------

SHELL		=	/bin/bash

OPT		=	opt

WFLAGS		=	-Wextra -Wpedantic -Wuninitialized -Wbad-function-cast -Wcast-qual -Wcast-align -Wwrite-strings -Wstrict-overflow=0 -Wstrict-overflow=1 -Wstrict-overflow=2 -Wstrict-overflow=3 -Wstrict-overflow=4 -Wstrict-overflow=5
#-Wfloat-equal
#-Wsuggest-attribute=pure -Wsuggest-attribute=const -Wsuggest-attribute=noreturn
CC		=	gcc -Wall -Werror -fprofile-arcs -ftest-coverage $(WFLAGS)

CXX		=	g++ -Wall -Werror -fprofile-arcs -ftest-coverage $(WFLAGS)

OBJDIR		=	obj
BINDIR		=	bin
SRCDIR		=	src

ifeq ($(OPT),opt)
MAKE		=	-O3
else
ifeq ($(OPT),dbg)
MAKE		=	-g
else
MAKE		=	-O
endif
endif

ifeq ($(TYPE),int)
CFLAGS		=	-D INT
else
#ifeq ($(TYPE),double)
#CFLAGS		=	-D DOUBLE
#else
CFLAGS		=	-D DOUBLE
#endif
endif

MAINNAME	=	ex6


CMAINOBJ	=	allocate.o \
			bp.o \
			ex4.o \
			misc.o \
			reader.o

MAINOBJFILES	=	$(addprefix $(OBJDIR)/,$(CMAINOBJ))

TEST		=	big

#-----------------------------------------------------------------------------
# make
#-----------------------------------------------------------------------------

.PHONY: all
all:		ex6

$(OBJDIR):
		@-mkdir -p $(OBJDIR)

$(BINDIR):
		@-mkdir -p $(BINDIR)

$(OBJDIR)/%.o:	$(SRCDIR)/%.c
		@echo "-> compiling $@"
		@$(CC) -std=c99 $(MAKE) $(CFLAGS) -I $(SRCDIR) -c $< -o $@

.PHONY: ex6
ex6:		$(OBJDIR) $(BINDIR) $(MAINOBJFILES)
		@echo "-> linking $@"
		@$(CXX) $(MAKE) $(CFLAGS) -mtune=native -fomit-frame-pointer -m64 $(MAINOBJFILES) -o $(BINDIR)/$(MAINNAME)
		ln -sf $(BINDIR)/$(MAINNAME) ex6

.PHONY: clean
clean:		$(OBJDIR)
ifneq ($(OBJDIR),)
		@-(rm -f $(OBJDIR)/*.o && rm -f $(OBJDIR)/*.gcda && rm -f $(OBJDIR)/*.gcno && rmdir $(OBJDIR));
		@echo "-> remove main objective files"
endif
ifneq ($(BINDIR),)
		@-(rm -f $(BINDIR)/* && rmdir $(BINDIR));
		@echo "-> remove binary"
		@echo "-> remove files"
endif

.PHONY: check
check:
		$(SHELL) ./check.sh $(TEST)