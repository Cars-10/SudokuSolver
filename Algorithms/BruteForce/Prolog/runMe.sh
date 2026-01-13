#!/bin/bash
# Prolog Sudoku Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="Prolog"
SOLVER_BINARY="swipl -q -s sudoku.pl --"  # Multi-word command
COMPILE_CMD=""  # No compilation needed

# Source common functions
source ../../common.sh

# Check for SWI-Prolog
check_toolchain "swipl"

# Run benchmarks
main "$@"
