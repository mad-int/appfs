#-----------------------------------------------------------------------------
# paths
#-----------------------------------------------------------------------------

SHELL		=	/bin/bash

OPT		=	opt
EX		=	ex9
COMPILER	=	gnat

# ARCH		:=	$(shell uname -m | \
# 			sed \
# 			-e 's/sun../sparc/' \
# 			-e 's/i.86/x86/' \
# 			-e 's/i86pc/x86/' \
# 			-e 's/[0-9]86/x86/' \
# 			-e 's/amd64/x86_64/' \
# 			-e 's/IP../mips/' \
# 			-e 's/9000..../hppa/' \
# 			-e 's/Power\ Macintosh/ppc/' \
# 			-e 's/00........../pwr4/'\
# 			-e 's/arm.*/arm/')
# OSTYPE		:=	$(shell uname -s | tr '[:upper:]' '[:lower:]' | \
# 			sed \
# 			-e 's/cygwin.*/cygwin/' \
# 			-e 's/irix../irix/' \
# 			-e 's/windows.*/windows/' \
# 			-e 's/mingw.*/mingw/')

ifeq ($(OPT),dbg)
MAKE		=	-g
else
MAKE		=	-O3
# -O3 -ffast-math -fomit-frame-pointer
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
			misc.o \
			binaryprogram.o \
			reader.o

MAIN		=	main

MAINOBJFILES	=	$(addprefix $(OBJDIR)/,$(MAINOBJ))

GCOVCOMPILE	=	-fprofile-arcs -ftest-coverage
GCOVLINK	=	-fprofile-arcs

ifeq ($(COMPILER),gcc)
WFLAGS		=	-Wextra -Wpedantic -Wuninitialized -Wcast-align -Wstrict-overflow=0 -Wstrict-overflow=1 -Wstrict-overflow=2 -Wsuggest-attribute=pure -Wsuggest-attribute=noreturn
#  -Wstrict-overflow=3 -Wstrict-overflow=4 -Wstrict-overflow=5 -Wsuggest-attribute=const
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

TEST		=	all

#-----------------------------------------------------------------------------
# make
#-----------------------------------------------------------------------------

.PHONY: all
all:		$(EX)

$(OBJDIR):
		@-mkdir -p $(OBJDIR)

$(BINDIR):
		@-mkdir -p $(BINDIR)

$(OBJDIR)/%.o:	$(SRCDIR)/%.adb
		@echo "-> compiling $@"
		@$(COMP) $(MAKE) $(INCDIR) $(GCOVCOMPILE) -c $< $(OUTPUT)

.PHONY: $(EX)
$(EX):		$(OBJDIR) $(BINDIR) $(MAINOBJFILES)
		@echo "-> binding $@"
		$(BIND) $(MAIN) -aO$(OBJDIR) -aI$(SRCDIR)
		@echo "-> linking $@"
		$(LINK) $(OBJDIR)/$(MAIN) $(GCOVLINK) -o $(BINDIR)/$(MAINNAME)
		ln -sf $(MAINNAME) $(BINDIR)/$(EX)
		ln -sf $(BINDIR)/$(EX) $(EX)

.PHONY: clean
clean:		$(OBJDIR)
ifneq ($(OBJDIR),)
		@-(rm -f $(OBJDIR)/*.ali && rm -f $(OBJDIR)/*.o && rm -f $(OBJDIR)/*.gcda && rm -f $(OBJDIR)/*.gcno && rmdir $(OBJDIR) && rm b~main.gcda && rm b~main.gcno);
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
