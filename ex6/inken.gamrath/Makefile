#-----------------------------------------------------------------------------
# paths
#-----------------------------------------------------------------------------

SHELL		=	/bin/bash

OPT		=	opt
EX		=	ex6
TYPE		=	double

WFLAGS		=	-Wextra -Wpedantic -Wuninitialized -Wbad-function-cast -Wcast-qual -Wcast-align -Wwrite-strings -Wstrict-overflow=0 -Wstrict-overflow=1 -Wstrict-overflow=2 -Wstrict-overflow=3 -Wstrict-overflow=4 -Wstrict-overflow=5
#-Wfloat-equal
#-Wsuggest-attribute=pure -Wsuggest-attribute=const -Wsuggest-attribute=noreturn
CC		=	gcc -Wall -Werror -fprofile-arcs -ftest-coverage $(WFLAGS)

CXX		=	g++ -Wall -Werror -fprofile-arcs -ftest-coverage $(WFLAGS)

ARCH		:=	$(shell uname -m | \
			sed \
			-e 's/sun../sparc/' \
			-e 's/i.86/x86/' \
			-e 's/i86pc/x86/' \
			-e 's/[0-9]86/x86/' \
			-e 's/amd64/x86_64/' \
			-e 's/IP../mips/' \
			-e 's/9000..../hppa/' \
			-e 's/Power\ Macintosh/ppc/' \
			-e 's/00........../pwr4/'\
			-e 's/arm.*/arm/')
OSTYPE		:=	$(shell uname -s | tr '[:upper:]' '[:lower:]' | \
			sed \
			-e 's/cygwin.*/cygwin/' \
			-e 's/irix../irix/' \
			-e 's/windows.*/windows/' \
			-e 's/mingw.*/mingw/')

ifeq ($(OPT),dbg)
MAKE		=	-g
else
MAKE		=	-O3
endif

BRANCHING	=	true
CUTOFF		=	true
CFLAGS		=

ifeq ($(TYPE),int)
CFLAGS		+=	-D INT
else
#ifeq ($(TYPE),double)
#CFLAGS		=	-D DOUBLE
#else
CFLAGS		+=	-D DOUBLE
#endif
endif

BRANCH		=
ifeq ($(BRANCHING),true)
CFLAGS		+=	-D BRANCHING
BRANCH		=	.branching
endif

CUT		=
ifeq ($(CUTOFF),true)
CFLAGS		+=	-D CUTOFF
ifeq ($(BRANCHING),true)
CUT		=	.cutoff
endif
endif

BASE		=	$(TYPE)$(BRANCH)$(CUT).$(OPT)

OBJDIR		=	obj/O.$(BASE)
BINDIR		=	bin
SRCDIR		=	src


MAINNAME	=	$(EX).$(BASE)


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
all:		$(EX)

$(OBJDIR):
		@-mkdir -p $(OBJDIR)

$(BINDIR):
		@-mkdir -p $(BINDIR)

$(OBJDIR)/%.o:	$(SRCDIR)/%.c
		@echo "-> compiling $@"
		@$(CC) -std=c99 $(MAKE) $(CFLAGS) -I $(SRCDIR) -c $< -o $@

.PHONY: $(EX)
$(EX):		$(OBJDIR) $(BINDIR) $(MAINOBJFILES)
		@echo "-> linking $@"
		@$(CXX) $(MAKE) $(CFLAGS) -mtune=native -fomit-frame-pointer -m64 $(MAINOBJFILES) -o $(BINDIR)/$(MAINNAME)
		ln -sf $(MAINNAME) $(BINDIR)/$(EX)
		ln -sf $(BINDIR)/$(EX) $(EX)

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

.PHONY: test
test:		check
