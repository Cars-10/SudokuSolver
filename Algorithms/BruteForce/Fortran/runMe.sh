#!/bin/bash
# Languages/Fortran/runMe.sh - Fortran Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Fortran"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain gfortran

    # Check source file exists
    if [ ! -f "Sudoku.f90" ]; then
        report_env_error "Sudoku.f90 not found"
    fi

    echo "Compiling Fortran solver with -O2 optimization..."
    gfortran -O2 -o Sudoku Sudoku.f90

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    if [ ! -f "$SOLVER_BINARY" ]; then
        report_env_error "Binary not found after compilation"
    fi

    echo "Compilation successful: $SOLVER_BINARY"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
