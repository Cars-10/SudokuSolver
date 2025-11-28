# AI-2025 Sudoku Solvers

This directory contains Sudoku solvers in various programming languages, migrated from the `Manual` directory.

## Setup and Usage

For each language directory, there is a `setup.sh` script to prepare the environment and a `RunMe.sh` script to execute the solver.

### General Steps
1. Navigate to the language directory: `cd AI-2025/<Language>`
2. Run setup: `./setup.sh`
3. Run solver: `./RunMe.sh`

### Specific Notes

- **Python/Jupyter**: Uses a virtual environment. `setup.sh` creates it.
- **JavaScript/TypeScript**: Uses `npm`. `setup.sh` runs `npm install`.
- **Go/Rust**: `setup.sh` handles module download or compilation.
- **C/C++/Fortran**: `setup.sh` compiles the source code.
- **BASIC**: Requires `qb64` executable in the root directory.
- **Tcl**: Requires `tcl-tk` (TclX package).

## Languages Included
- BASH
- BASIC
- C
- C++
- C_CPT
- C_Sharp
- F_Sharp
- Fortran
- Go
- Go_CPT
- JavaScript
- Julia
- Jupyter
- Python
- Python_CPT
- R
- Racket
- Ruby
- Rust
- Tcl
- TypeScript
