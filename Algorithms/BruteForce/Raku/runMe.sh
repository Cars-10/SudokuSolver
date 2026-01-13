#!/bin/bash
# Languages/Raku/runMe.sh - Raku Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Raku"
SOLVER_BINARY="raku Sudoku.raku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Raku - interpreted language)
# ============================================================================
compile() {
    # Check Raku availability
    check_toolchain raku

    # Check source file exists
    if [ ! -f "Sudoku.raku" ]; then
        report_env_error "Sudoku.raku not found"
    fi

    echo "Raku solver ready: Sudoku.raku"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
