# Fortran Sudoku Solver

Brute-force backtracking Sudoku solver implemented in Modern Fortran (F90), matching the C reference algorithm exactly.

## Algorithm

- **Search order**: Row-major (top-to-bottom, left-to-right)
- **Value order**: 1-9 ascending
- **Counting**: Every placement attempt is counted (algorithm fingerprint)

## Implementation Notes

### 0-Based Indexing

Unlike some Fortran programs that use 1-based indexing, this implementation explicitly uses 0-based arrays to match the C reference more closely:

```fortran
INTEGER, DIMENSION(0:8,0:8) :: puzzle

! Cell scanning: 0:8 (matches C's 0-8)
DO j = 0, 8
    DO i = 0, 8
        ! ...
    END DO
END DO

! Box calculation (same as C due to 0-based indexing)
x0 = floor(x/3.0)*3
y0 = floor(y/3.0)*3
```

### Column-Major vs Row-Major

Fortran arrays are stored column-major by default, but the iteration order explicitly uses row-major traversal (`DO j=0,8; DO i=0,8`) to match the C algorithm exactly.

### Compiled Performance

Fortran is a compiled language optimized for numerical computing, providing performance comparable to C.

## Validation Status

| Matrix | Expected Iterations | Actual | Status |
|--------|---------------------|--------|--------|
| 1      | 656                 | 656    | PASS   |
| 2      | 439,269             | 439,269| PASS   |
| 3      | 98,847              | 98,847 | PASS   |
| 4      | 9,085               | 9,085  | PASS   |
| 5      | 445,778             | 445,778| PASS   |

## Running

```bash
# Direct execution
gfortran -O2 -o Sudoku Sudoku.f90
./Sudoku ../../Matrices/1.matrix

# Via benchmark script
./runMe.sh ../../Matrices/1.matrix
```

## Requirements

- gfortran (GCC Fortran compiler)
- GNU Make (optional, for Makefile)
