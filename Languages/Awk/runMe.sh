#!/bin/bash
# Awk Sudoku Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="Awk"
SOLVER_BINARY="gawk -f Sudoku.awk"  # Multi-word command
COMPILE_CMD=""  # No compilation needed

# Source common functions
source ../common.sh

# Check for gawk
check_toolchain "gawk"

# Run benchmarks
main "$@"
