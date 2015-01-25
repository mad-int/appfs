ADAFLAGS = -Wall -O3
#ADAFLAGS += -g

default: ex9 constraints

%: %.adb
	gnat make $(ADAFLAGS) $<

clean:
	gnat clean ex9.adb constraints.adb


