#!/bin/bash
# Algorithms/DLX/C/runMe.sh - C DLX Sudoku solver build script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="C"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
# Path is relative to this script: Algorithms/DLX/C/ -> ../../common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain gcc

    # Check source files exist
    if [ ! -f "dlx_core.c" ] || [ ! -f "dlx_sudoku.c" ]; then
        report_env_error "Source files not found"
    fi

    echo "Compiling C DLX solver..."
    gcc -O3 -o dlx_solver dlx_sudoku.c dlx_core.c

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: ./dlx_solver"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
# For now, just compile as per plan
compile
