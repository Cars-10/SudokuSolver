# Haskell Sudoku Solver

Pure functional implementation of the brute-force Sudoku solver in Haskell.

## Algorithm Details

This implementation uses explicit recursion with pure functional state threading to match the C reference algorithm exactly:
- Row-major cell scanning (0-8 rows, 0-8 columns)
- Tries candidates 1-9 in ascending order
- Counts every placement attempt before validity check
- Immutable board representation with copy-on-write semantics

## Functional Programming Approach

**Iteration Counting:** Uses pure recursion by threading the counter through function calls:
```haskell
solve :: Board -> Int -> (Bool, Board, Int)
```

**State Management:**
- Board is immutable `[[Int]]` (list of lists)
- Each placement creates a new board
- Counter threaded through recursive calls
- Returns tuple `(solved, board, iterations)`

**Key Challenge:** Ensuring lazy evaluation doesn't affect iteration order
- All counting happens strictly before recursion
- Pattern matching forces evaluation at each step

## Compilation & Execution

Compile:
```bash
ghc -O2 -o Sudoku Sudoku.hs
```

Run:
```bash
./Sudoku ../../Matrices/1.matrix
```

Or use the benchmark script:
```bash
./runMe.sh ../../Matrices/1.matrix
```

## Validation Results

All 5 matrices validated with exact C reference iteration counts:
- Matrix 1: 656 iterations
- Matrix 2: 439,269 iterations
- Matrix 3: 98,847 iterations
- Matrix 4: 9,085 iterations
- Matrix 5: 445,778 iterations

## Requirements

- GHC (Glasgow Haskell Compiler) 8.0 or later
- Standard Haskell libraries (Data.List, System.Environment, Data.Time.Clock)

## Performance Notes

Haskell's lazy evaluation and immutable data structures add overhead compared to imperative implementations. The `-O2` optimization flag is essential for reasonable performance. Despite the functional style, iteration counts match exactly because:
1. Recursion order is explicitly controlled
2. Counter updates happen before recursive calls
3. Pattern matching forces strict evaluation where needed
