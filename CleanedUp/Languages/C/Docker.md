# Docker Instructions for C Solver

## Build the Image

Run the following command from the `CleanedUp/Languages/C` directory:

```bash
docker build -t sudoku-c .
```

## Run the Container

To run the benchmark, you need to mount the `Matrices` directory into the container. Assuming you are in the `CleanedUp/Languages/C` directory and the matrices are in `../../Matrices` (relative to the project root structure):

```bash
docker run --rm -v "$(pwd)/../../../Matrices:/Matrices" sudoku-c /Matrices/1.matrix
```

Or to run all matrices:

```bash
docker run --rm -v "$(pwd)/../../../Matrices:/Matrices" sudoku-c /Matrices/*.matrix
```

## Output

The container will output the metrics in JSON format to stdout (via the script's `cat` command at the end).
