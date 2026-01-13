# OCaml Sudoku Solver

Functional implementation of the brute-force Sudoku solver in OCaml.

## Algorithm Details

This implementation uses OCaml's imperative features to match the C reference algorithm exactly:
- Row-major cell scanning (0-8 rows, 0-8 columns)
- Tries candidates 1-9 in ascending order
- Counts every placement attempt before validity check
- Mutable array for board state and ref for counter

## Functional Programming Approach

**Iteration Counting:** Uses OCaml's `ref` type for mutable counter:
```ocaml
let count = ref 0
count := !count + 1  (* increment counter *)
```

**State Management:**
- Board is mutable 2D array: `Array.make_matrix 9 9 0`
- Direct in-place updates: `puzzle.(row).(col) <- value`
- Counter is mutable ref: `let count = ref 0`
- Pattern matching with `option` type for cell search

**Key Advantages:** OCaml's strict evaluation (unlike Haskell's lazy evaluation) makes it easier to ensure correct iteration order. The language supports both functional and imperative styles, making it natural to implement imperative algorithms.

## Compilation & Execution

Compile:
```bash
ocamlopt -o Sudoku unix.cmxa Sudoku.ml
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

- OCaml compiler (ocamlopt) 4.08 or later
- Unix library (unix.cmxa) for timing

## Performance Notes

OCaml provides excellent performance for functional languages due to:
1. Native code compilation
2. Strict evaluation by default
3. Efficient array representation
4. Minimal runtime overhead for imperative features

The iteration counts match exactly because:
1. Strict evaluation ensures predictable execution order
2. Mutable refs guarantee correct counter semantics
3. For loops execute in deterministic order
4. Array indexing is direct and efficient
