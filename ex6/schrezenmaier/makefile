CC = gcc
CFLAGS = -Wall -O3 -fprofile-arcs -ftest-coverage

# INTEGER for integer arithmetic or DOUBLE for double arithmetic
MODE = DOUBLE

OBJ = main.o linear_program.o allocate.o
BIN = prog
COV = allocate.gcda allocate.gcno main.gcda main.gcno linear_program.gcda linear_program.gcno

TESTFILE_PATH = ../../data/
ERROR_TESTFILES = error_test1.dat error_test2.dat error_test3.dat error_test4.dat error_test5.dat error_test6.dat \
	error_test7.dat error_test8.dat error_test9.dat error_test10.dat error_test11.dat corrupted_noRHS.dat \
	corrupted_toFewRows.dat
CORRECT_TESTFILES = markshare_3_0.dat markshare_3_1.dat test1.dat test2.dat test3.dat onlyEq.dat geqTest.dat
CORRECT_SOLUTIONS_PATH = ../../data/solutions/

.PHONY: clean

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) -o prog $(OBJ)
	@ln -sf $(BIN) ex6

clean:
	rm -f $(BIN) $(OBJ) $(COV)

check: $(BIN)
	@passed=0; \
	failed=0; \
	echo -e "*** Testing corrupted files: ***\n"; \
	for file in $(ERROR_TESTFILES); do \
		echo $$file:; \
		./$(BIN) $(TESTFILE_PATH)$$file dummy_sol.dat; \
		if [ $$? -eq 0 ]; then \
			let "failed+=1"; \
			echo "failed"; \
		else \
			let "passed+=1"; \
			echo "passed"; \
		fi; \
		echo; \
	done; \
	echo -e "*** Testing correct files: ***\n"; \
	for file in $(CORRECT_TESTFILES); do \
		echo $$file:; \
		./$(BIN) $(TESTFILE_PATH)$$file dummy_sol.dat; \
		while read p; do \
			grep "$$p" $(CORRECT_SOLUTIONS_PATH)$$file.sol > /dev/null; \
			if [ $$? -ne 0 ]; then \
				let "failed+=1"; \
				echo -e "failed\n"; \
				continue 2; \
			fi; \
		done < dummy_sol.dat; \
		while read p; do \
			grep "$$p" dummy_sol.dat > /dev/null; \
			if [ $$? -ne 0 ]; then \
				let "failed+=1"; \
				echo -e "failed\n"; \
				continue 2; \
			fi; \
		done < $(CORRECT_SOLUTIONS_PATH)$$file.sol; \
		let "passed+=1"; \
		echo -e "passed\n"; \
	done; \
	rm -f dummy_sol.dat; \
	echo "*** Test results: ***"; \
	echo -e "failed: $$failed, passed: $$passed"

main.o: src/main.c
	$(CC) $(CFLAGS) -c src/main.c

linear_program.o: src/linear_program.c
	$(CC) $(CFLAGS) -D $(MODE) -c src/linear_program.c

allocate.o: src/allocate.c
	$(CC) $(CFLAGS) -c src/allocate.c