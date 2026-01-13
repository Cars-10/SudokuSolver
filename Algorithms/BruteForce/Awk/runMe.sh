#!/bin/bash
# Awk Sudoku Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="Awk"

# Choose gawk if available, otherwise awk
if command -v gawk &> /dev/null; then
    AWK_CMD="gawk"
else
    AWK_CMD="awk"
fi

SOLVER_BINARY="$AWK_CMD -f Sudoku.awk"  # Multi-word command
COMPILE_CMD=""  # No compilation needed

# Source common functions
source ../../common.sh

# Check for awk tool
check_toolchain "$AWK_CMD"

# Run benchmarks
main "$@"
