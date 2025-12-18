#!/bin/bash
# Languages/OCaml/runMe.sh - Benchmark OCaml Sudoku solver
# Created: 2025-12-18

# Configure language-specific settings
LANGUAGE="OCaml"
COMPILE_CMD="ocamlopt -o Sudoku unix.cmxa Sudoku.ml"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"

# Source common benchmark library
source ../common.sh

# Check if OCaml compiler is available
check_toolchain "ocamlopt"

# Run main benchmark wrapper
main "$@"
