# Sudoku Solver

This project aims to be a teaching tool to understand how different computer programming languages work to solve the same problem.  Additionally it should provde a rough comparison of the solution speeds and memory usage.

## Programming Languages to Explore

Language | Status
---|---
Python | Finished
Tcl | Finished
C | Finished
Rust | Started
Java | Finished
Kotlin |
JavaScript |
TypeScript |
Ruby |
Julia |
Go |
Fortran |

## Performance Results (5 Matrices)

  C       Seconds 0.035
  Java    Seconds 0.274
  Python  Seconds 7.031
  Tcl     Seconds 6.668

## Special Notes

* Check every solution to insure that solve returns to stack 0 for performance.

## Program Goals

1. Read a unsolved sudoku matrix from a CSV file
2. Find a solution to the matrix
3. Count the number of recursive iterations to solve matrix
4. Find multiple solutions (if any) to the matrix
5. Print solution and time to calculate result
6. Print memory usage
7. Solution should be optimized for efficiency by utilizing things like memoization

## Development Goals

1. Each Solution should be complete with build files, etc.
2. Each Solution should also include a test suite
3. Solution should be able to solve several supplied matrices
4. Project to be managed in GITHUB

## Implementation Details

### Input matrices

Each matrix should be read from a simple CSV file with .matrix file extension in the following format.

    # Comment
    9 2 0 0 0 0 5 8 4
    0 0 0 5 0 0 0 0 3
    0 8 3 0 9 2 0 0 0
    2 6 0 8 5 4 0 0 1
    0 0 5 3 6 1 0 9 0
    1 0 0 0 0 9 0 0 0
    8 5 0 2 0 3 0 1 0
    4 1 2 9 8 0 0 3 0
    3 9 0 0 0 6 8 0 0

The program should take as it's input one or more .matrix files

## Language Notes

### Python

* The sudoku puzzle is handled as a Numpy Matrix (ndarray) which provides more efficient handling of 2D data.
* <https://docs.python.org/3/reference/index.html>  
* <https://numpy.org/doc/stable/index.html>

Timing the Solver
>time python3 Python/Sudoku.py Matrices/*.matrix

### Tcl

* <https://docs.activestate.com/activetcl/8.6/pkg/>
* <https://docs.activestate.com/activetcl/8.6/tcl/TclCmd/contents.html>

Needed to return to stack level 0 after solve() was completed, else it continued to run through the rest of the stack.

### C

To compile in VSC press _Alt-B_ then choose clang complier

### JAVA

* Reference for setting up log4j2.xml:
  * <https://javabeat.net/log4j-2-example/>
  * <http://logging.apache.org/log4j/2.x/index.html>

---

## Sample Output

    ~/iCloud/Programming/SudokuSolver: C/RunMe.sh
    ../Matrices/1.matrix

    Puzzle:
    9 2 0 0 0 0 5 8 4
    0 0 0 5 0 0 0 0 3
    0 8 3 0 9 2 0 0 0
    2 6 0 8 5 4 0 0 1
    0 0 5 3 6 1 0 9 0
    1 0 0 0 0 9 0 0 0
    8 5 0 2 0 3 0 1 0
    4 1 2 9 8 0 0 3 0
    3 9 0 0 0 6 8 0 0

    Puzzle:
    9 2 1 6 3 7 5 8 4
    6 7 4 5 1 8 9 2 3
    5 8 3 4 9 2 1 6 7
    2 6 9 8 5 4 3 7 1
    7 4 5 3 6 1 2 9 8
    1 3 8 7 2 9 6 4 5
    8 5 6 2 7 3 4 1 9
    4 1 2 9 8 5 7 3 6
    3 9 7 1 4 6 8 5 2

    Solved in Iterations=656
    ../Matrices/2.matrix

    Puzzle:
    0 0 3 0 0 5 0 0 4
    5 0 0 9 8 1 0 0 0
    0 0 0 0 0 0 0 2 0
    2 0 0 7 0 0 9 0 0
    0 8 0 0 9 0 0 3 0
    0 0 9 0 0 2 0 0 1
    0 3 0 0 0 0 0 0 0
    0 0 0 1 4 9 0 0 5
    9 0 0 3 0 0 8 0 0

    Puzzle:
    8 6 3 2 7 5 1 9 4
    5 4 2 9 8 1 6 7 3
    1 9 7 4 3 6 5 2 8
    2 5 4 7 1 3 9 8 6
    6 8 1 5 9 4 2 3 7
    3 7 9 8 6 2 4 5 1
    4 3 5 6 2 8 7 1 9
    7 2 8 1 4 9 3 6 5
    9 1 6 3 5 7 8 4 2

    Solved in Iterations=439269
    ../Matrices/3.matrix

    Puzzle:
    0 9 0 6 0 0 0 7 0
    0 5 2 0 7 0 0 4 8
    0 8 0 0 0 1 0 2 0
    0 0 5 0 0 0 0 0 3
    0 1 0 0 0 0 0 6 0
    3 0 0 0 0 0 4 0 0
    0 4 0 2 0 0 0 1 0
    2 6 0 0 4 0 5 3 0
    0 3 0 0 0 6 0 9 0

    Puzzle:
    4 9 3 6 8 2 1 7 5
    1 5 2 3 7 9 6 4 8
    7 8 6 4 5 1 3 2 9
    6 7 5 1 2 4 9 8 3
    8 1 4 5 9 3 7 6 2
    3 2 9 8 6 7 4 5 1
    9 4 7 2 3 5 8 1 6
    2 6 1 9 4 8 5 3 7
    5 3 8 7 1 6 2 9 4

    Solved in Iterations=98847
    Seconds to process 0.020988 Seconds
    ./Sudoku ../Matrices/*.matrix  0.02s user 0.00s system 93% cpu 0.024 total
