#!/bin/bash
# Erlang Sudoku Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="Erlang"
SOLVER_BINARY="escript sudoku.erl"  # Multi-word command
COMPILE_CMD=""  # No compilation needed (escript)

# Source common functions
source ../common.sh

# Check for Erlang escript
check_toolchain "escript"

# Run benchmarks
main "$@"
