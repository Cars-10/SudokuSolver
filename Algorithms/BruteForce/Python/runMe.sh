#!/bin/bash
# Algorithms/BruteForce/Python/runMe.sh - Python Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Python"
SOLVER_BINARY="./Sudoku.py"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Python - interpreted language)
# ============================================================================
compile() {
    # Check Python3 availability
    check_toolchain python3

    # Check source file exists
    if [ ! -f "Sudoku.py" ]; then
        report_env_error "Sudoku.py not found"
    fi

    # Verify Python can import the script (syntax check)
    python3 -m py_compile Sudoku.py 2>/dev/null
    if [ $? -ne 0 ]; then
        report_env_error "Sudoku.py has syntax errors"
    fi

    echo "Python solver ready: Sudoku.py"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
