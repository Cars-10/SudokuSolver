#!/bin/bash
# Languages/PHP/runMe.sh - PHP Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="PHP"
SOLVER_BINARY="./Sudoku.php"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for PHP - interpreted language)
# ============================================================================
compile() {
    # Check PHP availability
    check_toolchain php

    # Check source file exists
    if [ ! -f "Sudoku.php" ]; then
        report_env_error "Sudoku.php not found"
    fi

    # Verify PHP can parse the script (syntax check)
    php -l Sudoku.php 2>/dev/null > /dev/null
    if [ $? -ne 0 ]; then
        report_env_error "Sudoku.php has syntax errors"
    fi

    echo "PHP solver ready: Sudoku.php"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
