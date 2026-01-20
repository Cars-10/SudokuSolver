#!/bin/bash
# Algorithms/BruteForce/JavaScript/runMe.sh - JavaScript Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="JavaScript"
SOLVER_BINARY="node Sudoku.js"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for JavaScript - interpreted language)
# ============================================================================
compile() {
    # Check Node.js availability
    check_toolchain node

    # Check source file exists
    if [ ! -f "Sudoku.js" ]; then
        report_env_error "Sudoku.js not found"
    fi

    # Verify Node.js can parse the script (syntax check)
    node --check Sudoku.js > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Sudoku.js has syntax errors"
    fi

    echo "JavaScript solver ready: Sudoku.js (Node.js $(node --version))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
