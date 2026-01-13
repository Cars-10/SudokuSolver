# Makefile for Fortran CP Sudoku Solver

FC = gfortran
FFLAGS = -O3
TARGET = cp_solver
SOURCE = cp.f90

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(FC) $(FFLAGS) -o $(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET) *.o *.mod

.PHONY: all clean
