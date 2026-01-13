#!/bin/bash
# Languages/Haskell/runMe.sh - Benchmark Haskell Sudoku solver
# Created: 2025-12-18

# Configure language-specific settings
LANGUAGE="Haskell"
COMPILE_CMD="ghc -O2 -o Sudoku Sudoku.hs"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"

# Source common benchmark library
source ../../common.sh

# Check if GHC is available
check_toolchain "ghc"

# Run main benchmark wrapper
main "$@"
