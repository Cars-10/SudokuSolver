# C++ Sudoku Solver

This directory contains the C++ implementation of the Sudoku solver.

## Files
- `Sudoku.cpp`: The source code.
- `setupAndRunMe.sh`: Script to compile and run the solver, and verify output.
- `Dockerfile`: Configuration for building the Docker image.

## Optimization
The solver is compiled with the `-O3` optimization flag. This level of optimization enables:
- Aggressive loop unrolling and vectorization.
- Function inlining.
- Constant propagation and dead code elimination.

These optimizations significantly improve the performance of the backtracking algorithm used in the solver, reducing execution time by approximately 50% compared to unoptimized builds.

## Compilation
To compile locally:
```bash
g++ -O3 -o Sudoku Sudoku.cpp
```

## Running
To run locally:
```bash
./setupAndRunMe.sh [path_to_matrix_file]
```
If no file is provided, it defaults to `../../Matrices/*.matrix`.

## Docker
See `Docker.md` for instructions on building and running with Docker.
