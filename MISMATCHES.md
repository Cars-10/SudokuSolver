# Sudoku Benchmark Mismatches

This list compares the iteration counts of each solver against the C reference implementation for the same algorithm (Brute Force, DLX, or Constraint Propagation).

## Algorithm: CP (Constraint Propagation)
*Baseline: Algorithms/CP/C*

| Solver | Matrix | Iterations | Expected | Diff |
|--------|--------|------------|----------|------|
| **AppleScript** | 1 | 0 | 67 | -67 (Failed) |
| **AppleScript** | 2 | 0 | 87,180 | -87,180 (Failed) |
| **AppleScript** | 3 | 0 | 4,241 | -4,241 (Failed) |
| **AppleScript** | 4 | 0 | 1,787 | -1,787 (Failed) |
| **AppleScript** | 5 | 0 | 31,430 | -31,430 (Failed) |
| **Awk** | 2 | 0 | 87,180 | -87,180 (Failed) |
| **Awk** | 3 | 0 | 4,241 | -4,241 (Failed) |
| **Awk** | 4 | 0 | 1,787 | -1,787 (Failed) |
| **Awk** | 5 | 0 | 31,430 | -31,430 (Failed) |
| **Awk** | 6 | 0 | 69,497,705 | -69M (Failed) |
| **Cobol** | 1 | 0 | 67 | -67 (Failed) |
| **Cobol** | 2 | 0 | 87,180 | -87,180 (Failed) |
| **Cobol** | 3 | 0 | 4,241 | -4,241 (Failed) |
| **Cobol** | 4 | 0 | 1,787 | -1,787 (Failed) |
| **Cobol** | 5 | 0 | 31,430 | -31,430 (Failed) |
| **CommonLisp** | 1 | 84 | 67 | +17 |
| **CommonLisp** | 6 | 81 | 69,497,705 | -69M (Failure/Timeout?) |
| **Elixir** | 2 | 86,098 | 87,180 | -1,082 |
| **Elixir** | 3 | 4,134 | 4,241 | -107 |
| **Elixir** | 4 | 1,790 | 1,787 | +3 |
| **Elixir** | 5 | 31,680 | 31,430 | +250 |
| **Erlang** | 2 | 87,694 | 87,180 | +514 |
| **Erlang** | 3 | 4,285 | 4,241 | +44 |
| **Erlang** | 4 | 1,778 | 1,787 | -9 |
| **Erlang** | 5 | 31,727 | 31,430 | +297 |
| **Pike** | 2 | 0 | 87,180 | -87,180 (Failed) |
| **Pike** | 3 | 0 | 4,241 | -4,241 (Failed) |
| **Pike** | 4 | 0 | 1,787 | -1,787 (Failed) |
| **Pike** | 5 | 0 | 31,430 | -31,430 (Failed) |
| **PowerShell** | 2 | 166 | 87,180 | -87,014 |
| **PowerShell** | 3 | 222 | 4,241 | -4,019 |
| **PowerShell** | 4 | 100 | 1,787 | -1,687 |
| **PowerShell** | 5 | 3,273 | 31,430 | -28,157 |
| **Prolog** | 1 | 0 | 67 | -67 (Zero-search solve) |
| **Prolog** | 2 | 46 | 87,180 | -87,134 (Optimization) |
| **Prolog** | 3 | 0 | 4,241 | -4,241 (Zero-search solve) |
| **Prolog** | 4 | 0 | 1,787 | -1,787 (Zero-search solve) |
| **Prolog** | 5 | 192 | 31,430 | -31,238 (Optimization) |
| **Prolog** | 6 | 0 | 69,497,705 | -69M (Failure/Timeout?) |
| **Racket** | 2 | 87,138 | 87,180 | -42 |
| **Racket** | 3 | 4,223 | 4,241 | -18 |
| **Racket** | 4 | 1,786 | 1,787 | -1 |
| **Racket** | 5 | 31,405 | 31,430 | -25 |
| **SML** | 1 | 94 | 67 | +27 |
| **SML** | 2 | 96,142 | 87,180 | +8,962 |
| **SML** | 3 | 8,606 | 4,241 | +4,365 |
| **SML** | 4 | 2,602 | 1,787 | +815 |
| **SML** | 5 | 37,454 | 31,430 | +6,024 |
| **Scheme** | 6 | 81 | 69,497,705 | -69M (Failure?) |
| **Smalltalk** | 2 | 237,921 | 87,180 | +150,741 |
| **Smalltalk** | 3 | 44,215 | 4,241 | +39,974 |
| **Smalltalk** | 4 | 6,403 | 1,787 | +4,616 |
| **Smalltalk** | 5 | 100,918 | 31,430 | +69,488 |

## Algorithm: DLX (Dancing Links)
*Baseline: Algorithms/DLX/C*

| Solver | Matrix | Iterations | Expected | Diff |
|--------|--------|------------|----------|------|
| **AppleScript** | 2 | 135 | 111 | +24 |
| **AppleScript** | 3 | 158 | 131 | +27 |
| **AppleScript** | 4 | 95 | 70 | +25 |
| **AppleScript** | 5 | 1,492 | 1,472 | +20 |
| **Prolog** | 1 | 81 | 43 | +38 |
| **Prolog** | 2 | 81 | 111 | -30 |
| **Prolog** | 3 | 81 | 131 | -50 |
| **Prolog** | 4 | 81 | 70 | +11 |
| **Prolog** | 5 | 84 | 1,472 | -1,388 |
| **Prolog** | 6 | 81 | 65 | +16 |

## Algorithm: BruteForce
*Baseline: Algorithms/BruteForce/C*

No mismatches found (all successful runs match C exactly).
