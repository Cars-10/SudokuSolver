# Common Lisp Sudoku Solver

## Implementation

This is a brute-force backtracking Sudoku solver written in Common Lisp (SBCL - Steel Bank Common Lisp) that exactly matches the C reference implementation's algorithm.

### Language Features
- **Lisp Heritage**: Classic Lisp dialect with powerful macro system
- **Mutable State**: Uses `defvar` for global state and `setf` for mutation
- **Array Operations**: 2D arrays with `make-array` and `aref`
- **SBCL Runtime**: Steel Bank Common Lisp provides native compilation and good performance

### Algorithm Match
- Row-major cell iteration (0-8 rows, 0-8 cols within each row)
- Candidate values tried in ascending order (1-9)
- Every placement attempt increments counter
- Exact backtracking behavior matches C reference

## Requirements

- SBCL (Steel Bank Common Lisp)

### Installation

**Ubuntu/Debian:**
```bash
sudo apt-get install sbcl
```

**macOS:**
```bash
brew install sbcl
```

**Fedora:**
```bash
sudo dnf install sbcl
```

## Usage

### Direct Execution
```bash
sbcl --script Sudoku.lisp -- ../../Matrices/1.matrix
```

### Benchmark Script
```bash
./runMe.sh ../../Matrices/1.matrix
```

## Performance

Common Lisp with SBCL provides good performance due to native compilation, though typically slower than static compiled languages like C or Go.

## Validation Status

Validated against all 5 reference matrices with exact iteration count matches:
- Matrix 1: 656 iterations
- Matrix 2: 439,269 iterations
- Matrix 3: 98,847 iterations
- Matrix 4: 9,085 iterations
- Matrix 5: 445,778 iterations

## Implementation Notes

- Uses `defvar` for global puzzle array and counter
- `dotimes` loops for row/column iteration
- `return-from` for early exit from validation functions
- `format` for formatted output matching C reference
- Command-line argument handling via `sb-ext:*posix-argv*`
