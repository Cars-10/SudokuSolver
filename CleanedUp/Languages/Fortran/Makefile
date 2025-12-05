# variables
FC=gfortran
#CFLAGS=-c -g -Og -Wall
CFLAGS=-c -Ofast -Wall

# linking
Sudoku: Sudoku.o
	$(FC) Sudoku.o -o Sudoku

# compiling
Sudoku.o: Sudoku.f90
	$(FC) $(CFLAGS) Sudoku.f90


# cleanup
clean:
	rm *.o Sudoku

# run
run:
	make
	./Sudoku