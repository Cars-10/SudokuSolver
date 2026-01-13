#!/bin/bash
# Scheme (Guile) CP Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="Scheme"
SOLVER_BINARY="guile cp.scm"  # Multi-word command
COMPILE_CMD=""  # No compilation needed

# Source common functions
source ../../common.sh

# Check for Guile
check_toolchain "guile"

# Run benchmarks
main "$@"
