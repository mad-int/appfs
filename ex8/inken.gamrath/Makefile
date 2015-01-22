#-----------------------------------------------------------------------------
# paths
#-----------------------------------------------------------------------------

SHELL		=	/bin/bash

OPT		=	opt
EX		=	ex8
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
endif

ifneq ($(COMPILER),gcc)
COMPILER	=	gnat
endif

BASE		=	$(OPT)

OBJDIR		=	obj/O.$(BASE).$(COMPILER)
BINDIR		=	bin
SRCDIR		=	src

MAINNAME	=	$(EX).$(BASE)

MAINOBJ	=	ex8.o \

MAINOBJFILES	=	$(addprefix $(OBJDIR)/,$(MAINOBJ))

ifeq ($(COMPILER),gcc)
WFLAGS		=	-Wextra -Wpedantic -Wuninitialized -Wcast-align -Wstrict-overflow=0 -Wstrict-overflow=1 -Wstrict-overflow=2 -Wstrict-overflow=3 -Wstrict-overflow=4 -Wstrict-overflow=5 -Wsuggest-attribute=pure -Wsuggest-attribute=const -Wsuggest-attribute=noreturn
COMP		=	gcc -Wall -Werror $(WFLAGS)  -std=c99
INCDIR		=	-I $(SRCDIR)
OUTPUT		=	-o $@
BIND		=	gnatbind
LINK		=	gnatlink
else
COMP		=	gnatmake
OUTPUT		=	-D $(OBJDIR)
INCIDIR		=	-aI$(SRCDIR)
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
		@-mkdir -p $(OBJDIR)

$(BINDIR):
		@-mkdir -p $(BINDIR)

$(OBJDIR)/%.o:	$(SRCDIR)/%.adb
		@echo "-> compiling $@"
		@$(COMP) $(MAKE) $(INCDIR) -c $< $(OUTPUT)

.PHONY: $(EX)
$(EX):		$(OBJDIR) $(BINDIR) $(MAINOBJFILES)
		@echo "-> building $@"
		$(BIND) $(EX) -aO$(OBJDIR) -aI$(SRCDIR)
		@echo "-> linking $@"
		$(LINK) $(OBJDIR)/$(EX) -o $(BINDIR)/$(MAINNAME)
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
		$(SHELL) ./check.sh $(TEST)

.PHONY: test
test:		check
