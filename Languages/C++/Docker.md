# Docker Instructions for C++ Solver

## Build
To build the optimized Docker image:
```bash
docker build -t sudoku-cpp-opt .
```

## Run
To run the solver with a specific matrix:
```bash
docker run --rm -v /absolute/path/to/Matrices:/usr/Matrices sudoku-cpp-opt /usr/Matrices/1.matrix
```

## Output Verification
The container automatically verifies the output against `ReferenceForAllMatrixRun.txt` (which should be in the mounted Matrices directory).
If the output matches (ignoring timing), the status will be `pass`. If not, it will be `suspect`.
