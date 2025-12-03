# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SudokuSolver is a polyglot programming project implementing the same Sudoku puzzle solver algorithm across 79+ programming languages. It serves as an educational tool and performance benchmark comparing manual implementations vs AI-generated (ChatGPT) implementations.

The solver uses brute-force backtracking with constraint checking (rows, columns, 3x3 boxes), counting recursive iterations and measuring execution time.

## Project Structure

- `AI-2025/` - 79 AI-generated language implementations (each has `Sudoku.*`, `RunMe.sh`, `setup.sh`)
- `Manual/` - 15 manually written implementations
- `Matrices/` - Input test puzzles (1.matrix=easy to 6.matrix=extremely hard with 622M+ iterations)
- `ChatGPT/` - Historical ChatGPT responses archive

## Common Commands

### Run individual solver
```bash
cd AI-2025/Python
./setup.sh              # First time: install dependencies
./RunMe.sh              # Run solver on all matrices
```

### Run full benchmark suite
```bash
./runAll.sh             # Benchmarks all solvers on matrices 1-5, ordered by speed
```

### Generate benchmark report
```bash
python3 generate_report.py              # Generate HTML report
python3 generate_report.py --save-history  # Also save to benchmark_history.json
python3 generate_report.py --highlight Go  # Highlight specific solver
```

### Utility scripts
```bash
python3 get_solver_order.py     # Get solvers sorted by performance (fast first)
./sanity_check.sh               # Verify solver compilation/basic functionality
./verify_iterations.sh          # Check iteration counts match expected values
```

## Adding a New Language Solver

1. Create directory `AI-2025/<LanguageName>/`
2. Add `Sudoku.<ext>` - must read `.matrix` files from command line args, print solution and iteration count
3. Add `setup.sh` - install dependencies/compile
4. Add `RunMe.sh` - run solver with `../Matrices/*.matrix`
5. Solver must output: `Solved in Iterations=<N>` and `Seconds to process <T> Seconds`

## Matrix File Format

```
# Comment line (ignored)
9 2 0 0 0 0 5 8 4
0 0 0 5 0 0 0 0 3
...
```
9x9 space-separated values, 0 = empty cell.

## Architecture

```
Input Matrices → RunMe.sh (compile & run with /usr/bin/time) → run.txt
                                    ↓
                    generate_report.py parses run.txt files
                                    ↓
                benchmark_history.json + benchmark_report.html
```

The benchmark pipeline (`runAll.sh`) uses `get_solver_order.py` to run fastest solvers first for quicker feedback. Each solver's `RunMe.sh` is executed with `/usr/bin/time -l` to capture CPU/memory metrics, output goes to `run.txt`.

## Expected Iteration Counts

All correct implementations should produce identical iteration counts:
- Matrix 1: 656 iterations
- Matrix 2: 439,269 iterations
- Matrix 3: 98,847 iterations
- Matrix 4: 9,085 iterations
- Matrix 5: 445,778 iterations
- Matrix 6: 622M+ iterations (extremely slow, excluded from standard benchmarks)
