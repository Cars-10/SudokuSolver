#!/bin/bash
# Languages/Ruby/runMe.sh - Ruby Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Ruby"
SOLVER_BINARY="./Sudoku.rb"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Ruby - interpreted language)
# ============================================================================
compile() {
    # Check Ruby availability
    check_toolchain ruby

    # Check source file exists
    if [ ! -f "Sudoku.rb" ]; then
        report_env_error "Sudoku.rb not found"
    fi

    # Verify Ruby can parse the script (syntax check)
    ruby -c Sudoku.rb > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Sudoku.rb has syntax errors"
    fi

    echo "Ruby solver ready: Sudoku.rb"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
