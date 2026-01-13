#!/bin/bash
# Algorithms/CP/Fortran/runMe.sh - Fortran CP solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Fortran"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain gfortran

    # Check source file exists
    if [ ! -f "cp.f90" ]; then
        report_env_error "cp.f90 not found"
    fi

    echo "Compiling Fortran CP solver with -O3 optimization..."
    make clean > /dev/null 2>&1
    make

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
