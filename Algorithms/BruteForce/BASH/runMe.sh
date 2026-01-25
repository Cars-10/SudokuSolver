#!/bin/bash
# Algorithms/BruteForce/BASH/runMe.sh - Bash Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="BASH"
SOLVER_BINARY="bash Sudoku.sh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Bash - interpreted language)
# ============================================================================
compile() {
    # Check Bash availability (should always exist)
    check_toolchain bash

    # Check source file exists
    if [ ! -f "Sudoku.sh" ]; then
        report_env_error "Sudoku.sh not found"
    fi

    # Verify Bash can parse the script (syntax check)
    bash -n Sudoku.sh > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Sudoku.sh has syntax errors"
    fi

    echo "Bash solver ready: Sudoku.sh ($(bash --version | head -1))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
