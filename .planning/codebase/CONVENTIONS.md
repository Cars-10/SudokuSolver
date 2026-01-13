# Conventions

## Benchmarking Standards
- **"Matrix 1" Rule:** All implementations must first pass `1.matrix` with exactly **656** iterations.
- **Output Format:** strict stdout requirements:
  ```
  [Board State]
  Solved in Iterations=656
  ```
- **Byte-for-Byte:** Output must match the C reference implementation.

## Coding Style
- **Languages:** Idiomatic style for the specific language is preferred, *except* where it conflicts with the recursive backtracking algorithm structure.
- **Comments:** Minimal. Focus on code clarity.
- **Naming:**
  - Entry script: `runMe.sh` (or `setupAndRunMe.sh` for compilation).
  - Main source: `Sudoku.[ext]`.

## Process
- **Adding a Language:**
  1. Create directory.
  2. Implement `Sudoku.[ext]`.
  3. Create `runMe.sh` sourcing `../common.sh`.
  4. Verify against Matrix 1.
