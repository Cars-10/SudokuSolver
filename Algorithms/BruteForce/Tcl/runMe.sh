#!/bin/bash
# Tcl Sudoku Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="Tcl"
SOLVER_BINARY="tclsh Sudoku.tcl"  # Multi-word command
COMPILE_CMD=""  # No compilation needed

# Source common functions
source ../../common.sh

# Check for tclsh
check_toolchain "tclsh"

# Run benchmarks
main "$@"
