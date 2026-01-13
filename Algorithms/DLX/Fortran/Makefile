# Makefile for Fortran DLX Sudoku Solver

FC = gfortran
FFLAGS = -O3
TARGET = dlx_solver
SOURCE = dlx.f90

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(FC) $(FFLAGS) -o $(TARGET) $(SOURCE)

clean:
	rm -f $(TARGET) *.o *.mod

.PHONY: all clean
