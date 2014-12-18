CC = gcc
CFLAGS = -Wall -std=c99 -g
#CFLAGS = -Wall -g -DUSE_INT
CFLAGS += -fstrict-overflow -Wstrict-overflow=5
CFLAGS += -Wextra -Wfloat-equal -Wpedantic -Wsign-conversion -Wshadow
CFLAGS += -Wuninitialized -Wbad-function-cast -Wcast-qual -Wcast-align -Wwrite-strings
CFLAGS += -Wsuggest-attribute=pure -Wsuggest-attribute=const -Wsuggest-attribute=noreturn

# gconv
GCONVFLAGS = $(CFLAGS) -g -fprofile-arcs -ftest-coverage --coverage

# lcov
# lcov --capture --directory . --output-file coverage.info
# genhtml coverage.info --output-directory cov

# default target
default: ex7

# ----

ex7: ex7.c constr.o
	$(CC) $(CFLAGS) ex7.c constr.o -o ex7

constr.o: constr.c constr.h
	$(CC) -c $(CFLAGS) constr.c -o constr.o

check:
	./run_checks.sh

cov:
	$(CC) -c $(GCONVFLAGS) constr.c -o constr.o
	$(CC) $(GCONVFLAGS) ex7.c constr.o -o ex7
	./run_checks.sh
	mkdir cov
	lcov --capture --directory . --output-file cov/coverage.info
	genhtml cov/coverage.info --output-directory cov

clean:
	rm -f *.o ex7
	rm -f *.gcda *.gcno cov/coverage.info
	rm -rf cov/

