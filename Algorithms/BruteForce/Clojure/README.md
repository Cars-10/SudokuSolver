# Clojure Sudoku Solver

## Implementation

This is a brute-force backtracking Sudoku solver written in Clojure that exactly matches the C reference implementation's algorithm.

### Language Features
- **Modern Lisp**: JVM-based Lisp dialect with functional programming emphasis
- **Immutable Data**: Uses persistent vectors for puzzle state with `assoc-in` for updates
- **Atoms for State**: Uses atoms for mutable counter tracking
- **Functional Style**: Uses `loop`/`recur` for iteration, `some` for searching
- **JVM Integration**: Runs on Java Virtual Machine for cross-platform compatibility

### Algorithm Match
- Row-major cell iteration (0-8 rows, 0-8 cols within each row)
- Candidate values tried in ascending order (1-9)
- Every placement attempt increments counter
- Exact backtracking behavior matches C reference

## Requirements

- Clojure 1.11.1 or later

### Installation

**Ubuntu/Debian:**
```bash
curl -O https://download.clojure.org/install/linux-install-1.11.1.1413.sh
chmod +x linux-install-1.11.1.1413.sh
sudo ./linux-install-1.11.1.1413.sh
```

**macOS:**
```bash
brew install clojure/tools/clojure
```

**Fedora:**
```bash
sudo dnf install clojure
```

## Usage

### Direct Execution
```bash
clojure -M Sudoku.clj ../../Matrices/1.matrix
```

### Benchmark Script
```bash
./runMe.sh ../../Matrices/1.matrix
```

## Performance

Clojure provides moderate performance through JVM JIT compilation. The immutable data structures have some overhead compared to mutable arrays, but the JVM's optimization helps maintain reasonable speed.

## Validation Status

Validated against all 5 reference matrices with exact iteration count matches:
- Matrix 1: 656 iterations
- Matrix 2: 439,269 iterations
- Matrix 3: 98,847 iterations
- Matrix 4: 9,085 iterations
- Matrix 5: 445,778 iterations

## Implementation Notes

- Uses atoms (`(atom 0)`) for mutable iteration counter
- Uses persistent vectors for immutable puzzle state
- `assoc-in` creates new puzzle versions with updated cells
- `loop`/`recur` for tail-recursive iteration
- `some` for searching collections
- `for` comprehensions for nested loops in validation
- Functional approach while maintaining algorithm fidelity
