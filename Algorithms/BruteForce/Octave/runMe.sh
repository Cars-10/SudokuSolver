#!/bin/bash
# Algorithms/BruteForce/Octave/runMe.sh - Octave Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Octave"
SOLVER_BINARY="octave --no-gui --no-window-system Sudoku.m"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Octave - interpreted language)
# ============================================================================
compile() {
    # Check Octave availability
    check_toolchain octave

    # Check source file exists
    if [ ! -f "Sudoku.m" ]; then
        report_env_error "Sudoku.m not found"
    fi

    echo "Octave solver ready: Sudoku.m"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
