#!/bin/bash
# Languages/Perl/runMe.sh - Perl Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Perl"
SOLVER_BINARY="./Sudoku.pl"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Perl - interpreted language)
# ============================================================================
compile() {
    # Check Perl availability
    check_toolchain perl

    # Check source file exists
    if [ ! -f "Sudoku.pl" ]; then
        report_env_error "Sudoku.pl not found"
    fi

    # Verify Perl can parse the script (syntax check)
    perl -c Sudoku.pl > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Sudoku.pl has syntax errors"
    fi

    echo "Perl solver ready: Sudoku.pl"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
