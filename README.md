# Sudoku Solver

This project aims to be a teaching tool to understand how different computer programming languages
work to solve the same problem, in this case Sudoku puzzles. Why create a Suduko Solver? It is small
enough to be fun while also touching enough functionality such as parsing commandline arguments,
 reading a file of data, using two dimensional arrays, performing several loops as well as recursion.
 The aim is not specifically to write the quickest solver as this version uses only a brute force
 approach.  Puzzle #6 is extremely hard and requires over 622 Million iterations to solve!

## Goals

* Learn the nuances of different programming languages
* Compare their performance by solving the same problem
* Configure and use Microsoft Visual Studio Code Editor as the IDE for all languages

## Programming Languages to Explore

Language | Status
---|---
BASH |
BASIC? |
C | Completed
C# | Completed
C++ | Completed
ERLANG? |
F #|
Fortran | Completed
Go | Completed
Java | Completed
JavaScript | Completed
Julia | Completed
Kotlin |
Lisp (racket) | Completed
PHP |
Python | Completed
R | Completed
Rust | Completed
Ruby | Completed
Scala |
Tcl | Completed
TypeScript | Completed

## Performance Results (6 Matrices)

Tests run on a Macbook Pro with 2.7 GHz Quad-Core Intel Core i7

    Go          Seconds 9.000
    Rust        Seconds 9.78
    TypeScript  Seconds 19.436
    JavaScript  Seconds 22.500
    C++         Seconds 23.064
    C           Seconds 22.944
    Java        Seconds 42.465
    C_Sharp     Seconds 57.142
    Julia       Seconds 61.362
    Fortran     Seconds 613.763
    Ruby        Seconds 1035.392
    Tcl         Seconds 4398.989
    Python      Seconds 5114.008
    R	        Hours!  2.751 hours

## Findings

* Rust is fast but had a bit of a learning curve (for me at least)
* Go is just as fast as rust and took me only a few hours to code (my first time using go)
* I was surprised how fast JavaScript in Node.js performed
* Visual Code development speed was increased by using the tabnine extension
* JavaScript development did not have the best visual feedback in Visual Code
* C# was very easy to pick up
* Fortran was pretty slow.  (Maybe there is some optimization possible)
* Tcl (my all time favorite language) is still faster than Python (which seems to have won the popularity contest)
* WOW R was really slow

## Program Goals

1. Read a unsolved sudoku matrix from a file
2. Find a solution to the matrix using two methods
    1. Brute Force Solution
    2. Human Solution (TODO)
3. Count the number of recursive iterations to solve matrix
4. Print solution and time to calculate result
5. Print memory usage (TODO)
6. Solution should be compiled with optimization
7. Create on Object Oriented Implementation where possible and compare effect on perfromance (TODO)

## Development Goals

1. Each Solution should be complete with build files, etc.
2. Each Solution should also include a test suite
3. Solution should be able to solve several supplied matrices
4. Project to be managed in GITHUB
5. Develop launch.json and tasks.json file for handling all languages

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

### Racket

Install packages via

    raco pkg install global
### Julia

To add packages, like ArgParse, run the following in a julia interpreter.

    julia
    using Pkg
    Pkg.add("ArgParse")

### Python

* The sudoku puzzle is handled as a Numpy Matrix (ndarray) which provides more efficient handling of 2D data.
* <https://docs.python.org/3/reference/index.html>  
* <https://numpy.org/doc/stable/index.html>

Timing the Solver
>time python3 Python/Sudoku.py Matrices/*.matrix

### Rust

* <https://doc.rust-lang.org/std/index.html>
* <https://doc.rust-lang.org/stable/rust-by-example/index.html>
* <https://fasterthanli.me/articles/a-half-hour-to-learn-rust>

### Ruby

* <https://www.tutorialspoint.com/ruby/ruby_methods.htm>

Running ruby with --jit flag to enable Just-In-Time compiling

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

### JavaScript

Using Node.js

Note: JavaScript only has One Dimensional Arrays.  So you need to create an Array of Arrays! Also be wary
of variable scope, as i & j values of the calling function were used if not excplicityly set via
let in the subroutine.

* <https://nodejs.org/api/>

## FORTRAN 90

Using gfortran.

* <https://gcc.gnu.org/wiki/GFortran>
* Array indexes start with 1 be default. Need to dimension as 0:8 for zero index.

## BASIC

* <https://www.qb64.org/portal/>

## Object Oriented Thoughts

* Puzzle Class
  * ReadFile (Initialize?)
  * Print
  * Solve(Method: Human| Brute)
  * IsPossible

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
