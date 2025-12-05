# C Sudoku Solver

This directory contains the C implementation of the Sudoku Solver benchmark.

## Optimization
The solver is compiled with the `-O3` optimization flag. This level of optimization enables:
- Aggressive loop unrolling and vectorization.
- Function inlining.
- Constant propagation and dead code elimination.

These optimizations significantly improve the performance of the backtracking algorithm used in the solver, reducing execution time by approximately 50% compared to unoptimized builds.

## Prerequisites

- GCC compiler
- `time` command (BSD `time` on macOS, GNU `time` on Linux)

## Running Locally

1.  Navigate to this directory.
2.  Run the setup and execution script:
    ```bash
    ./setupAndRunMe.sh [path_to_matrix_files]
    ```
    If no arguments are provided, it defaults to `../../Matrices/*.matrix`.

## Running with Docker

See [Docker.md](Docker.md) for instructions on running this solver in a container.
