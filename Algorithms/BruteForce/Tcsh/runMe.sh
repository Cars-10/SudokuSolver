#!/bin/bash
# Languages/Tcsh/runMe.sh - Tcsh Sudoku solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Tcsh"
SOLVER_BINARY="tcsh Sudoku.tcsh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Tcsh - interpreted language)
# ============================================================================
compile() {
    check_toolchain tcsh

    if [ ! -f "Sudoku.tcsh" ]; then
        report_env_error "Sudoku.tcsh not found"
    fi

    echo "Tcsh solver ready: Sudoku.tcsh ($(tcsh --version 2>&1 | head -1))"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
