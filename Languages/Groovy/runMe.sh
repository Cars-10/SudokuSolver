#!/bin/bash
# Groovy Sudoku Solver Benchmark Runner
# Uses common.sh for shared benchmark functionality

LANGUAGE="Groovy"
SOLVER_BINARY="groovy Sudoku.groovy"

# Source common functions
source ../common.sh

# Check for Groovy
check_toolchain "groovy"

# Override compile (Groovy is interpreted/JIT compiled)
compile() {
    echo "Groovy is JIT-compiled, no explicit compilation needed" >&2
}

# Run main
main "$@"
