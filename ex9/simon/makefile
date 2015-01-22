GPR=ex9.gpr
FLAGS=

# COEFFS in { FLOAT | INT }
COEFFS = FLOAT

ifeq ($(COEFFS), INT)
FLAGS += -Xcoeffs=integer
endif



all: dir
	gnatmake -P$(GPR) -Xmode=release $(FLAGS)
debug: dir
	gnatmake -P$(GPR) -Xmode=debug $(FLAGS)
clean:
	gnat clean -P$(GPR)
	rmdir obj
dir:
	mkdir -p obj