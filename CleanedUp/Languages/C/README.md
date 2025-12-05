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

To run the benchmark using the shared `sudoku-content-server` Docker image (which includes the necessary C build tools and runtime):

1.  Ensure you have built the content server image:
    ```bash
    docker build -t sudoku-content-server -f ../../../Metrics/ContentServer/Dockerfile ../../../Metrics/ContentServer
    ```

2.  Run the solver from this directory:
    ```bash
    docker run --rm -v "$(pwd)/../../..:/data" -w /data/CleanedUp/Languages/C sudoku-content-server ./setupAndRunMe.sh [path_to_matrix_files]
    ```

    Example for a single matrix:
    ```bash
    docker run --rm -v "$(pwd)/../../..:/data" -w /data/CleanedUp/Languages/C sudoku-content-server ./setupAndRunMe.sh ../../../Matrices/1.matrix
    ```
