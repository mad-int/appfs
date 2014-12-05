#makefile 

OBJSDIR = obj
SRCDIR = src
OBJS = main.o reader.o binPrg.o
CC = g++ 
CFLAGS = -Wall -O2 -fprofile-arcs -ftest-coverage
MAIN = ex6

.PHONY : all
all : $(MAIN)

$(MAIN) : $(addprefix $(OBJSDIR)/, $(OBJS))
	$(CC) $(CFLAGS) -o $(MAIN) $+
	
$(OBJSDIR)/%.o : $(SRCDIR)/%.cpp $(OBJSDIR)
	$(CC) $(CFLAGS) -c $< -o $@


.PHONY : clean
clean : 
	$(RM) $(MAIN).exe
	$(RM) $(OBJSDIR)/*.o
	$(RM) $(OBJSDIR)/*.gcda
	$(RM) $(OBJSDIR)/*.gcno

	
.PHONY : check
check :
	sh check_all
