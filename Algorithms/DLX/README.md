# DLX (Dancing Links) Sudoku Solvers

This directory contains Sudoku solver implementations using Donald Knuth's **Dancing Links (DLX)** technique for his **Algorithm X**.

## Philosophy

Algorithm X is a recursive, non-deterministic, depth-first, backtracking algorithm that finds all solutions to an exact cover problem. Dancing Links is a technique for implementing Algorithm X efficiently using quadruply linked lists.

## Implementations

Every implementation in this directory follows the same exact logic:
1.  Model Sudoku as an Exact Cover problem with 324 constraints.
2.  Represent the matrix as a sparse linked structure.
3.  Solve using Algorithm X.

## Matrix Constraints (324 total)
- 81 Row-Column constraints (each cell must have a digit).
- 81 Row-Digit constraints (each row must have every digit).
- 81 Column-Digit constraints (each column must have every digit).
- 81 Box-Digit constraints (each box must have every digit).
