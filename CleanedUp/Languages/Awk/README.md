# Awk Sudoku Solver

This directory contains the Awk implementation of the Sudoku solver.

## Files
- `Sudoku.awk`: The source code.
- `setupAndRunMe.sh`: Script to run the solver and verify output.

## Running

### Locally
To run locally:
```bash
./setupAndRunMe.sh [path_to_matrix_file]
```
If no file is provided, it defaults to `../../Matrices/*.matrix`.

### Docker
To run this benchmark using the standard `sudoku-content-server` image:
```bash
docker run --rm -v "$(pwd):/data" -w /data/CleanedUp/Languages/Awk sudoku-content-server ./setupAndRunMe.sh
```
