#!/bin/bash
# Languages/R/runMe.sh - R Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="R"
SOLVER_BINARY="Rscript Sudoku.R"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for R - interpreted language)
# ============================================================================
compile() {
    # Check R availability
    check_toolchain Rscript

    # Check source file exists
    if [ ! -f "Sudoku.R" ]; then
        report_env_error "Sudoku.R not found"
    fi

    echo "R solver ready: Sudoku.R ($(Rscript --version 2>&1))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
