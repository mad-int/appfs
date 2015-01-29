ADAFLAGS = -Wall -O3
#ADAFLAGS += -g

default: ex9 constraints

ex9: ex9.adb constraints.ads constraints.adb
	gnat make $(ADAFLAGS) ex9.adb

%: %.adb
	gnat make $(ADAFLAGS) $<

check: ex9
	./run_checks.sh

clean:
	gnat clean ex9.adb constraints.adb


