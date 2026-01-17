#!/bin/bash
# Algorithms/CP/Forth/runMe.sh - Forth CP Sudoku solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Forth"
ALGORITHM="CP"
SOLVER_BINARY="gforth cp.fs"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
# Path is relative to this script: Algorithms/CP/Forth/ -> ../../common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check if gforth is available
    if ! command -v gforth &> /dev/null; then
        if [ -x "/usr/bin/gforth" ]; then
            SOLVER_BINARY="/usr/bin/gforth cp.fs"
        else
            report_env_error "gforth not found"
        fi
    fi
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
# Call main function from common.sh, which handles compilation and benchmarking
main "$@"
