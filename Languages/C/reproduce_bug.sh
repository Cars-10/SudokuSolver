# Create a dummy matrix file
echo "9 2 0 0 0 0 5 8 4" > test.matrix
echo "0 0 0 5 0 0 0 0 3" >> test.matrix
echo "0 8 3 0 9 2 0 0 0" >> test.matrix
echo "2 6 0 8 5 4 0 0 1" >> test.matrix
echo "0 0 5 3 6 1 0 9 0" >> test.matrix
echo "1 0 0 0 0 9 0 0 0" >> test.matrix
echo "8 5 0 2 0 3 0 1 0" >> test.matrix
echo "4 1 2 9 8 0 0 3 0" >> test.matrix
echo "3 9 0 0 0 6 8 0 0" >> test.matrix

# Compile Sudoku.c
gcc -o Sudoku Sudoku.c

# Run it
./Sudoku test.matrix
