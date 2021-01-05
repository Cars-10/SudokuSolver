# Sudoku Solver
This project aims to be a teaching tool to understand how different computer programming languages work to solve the same problem.  Additionally it should provde a rough comparison of the solution speeds and memory usage.

## Programming Languages to Explore
* Python (Started) 

    https://docs.python.org/3/reference/index.html
    https://numpy.org/doc/stable/index.html

* Tcl
* C
* Rust
* Java
* Kotlin
* JavaScript
* TypeScript
* Ruby

## Program Goals
1. Read a unsolved sudoku matrix from a CSV file
2. Find a solution to the matrix
3. Count the number of recursive iterations to solve matrix
4. Find multiple solutions (if any) to the matrix
5. Print solution and time to calculate result
5. Print memory usage
6. Solution should be optimized for efficiency by utilizing things like memoization


## Development Goals
1. Each Solution should be complete with build files, etc.
2. Each Solution should also include a test suite
3. Solution should be able to solve several supplied matrices
4. Project to be managed in GITHUB

## Implementation Details
### Input matrices
Each matrix should be read from a simple CSV file with .matrix file extension in the following format.


    # Comment
    9, 2, 0, 0, 0, 0, 5, 8, 4
    0, 0, 0, 5, 0, 0, 0, 0, 3
    0, 8, 3, 0, 9, 2, 0, 0, 0
    2, 6, 0, 8, 5, 4, 0, 0, 1
    0, 0, 5, 3, 6, 1, 0, 9, 0
    1, 0, 0, 0, 0, 9, 0, 0, 0
    8, 5, 0, 2, 0, 3, 0, 1, 0
    4, 1, 2, 9, 8, 0, 0, 3, 0
    3, 9, 0, 0, 0, 6, 8, 0, 0

The program should take as it's input one or more .matrix files

## Language Notes
### Python
* The sudoku puzzle is handled as a Numpy Matrix (ndarray) which provides more efficient handling of 2D data.



