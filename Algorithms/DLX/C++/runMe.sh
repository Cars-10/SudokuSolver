#!/bin/bash
# Algorithms/DLX/C++/runMe.sh - C++ DLX Sudoku solver build and benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="C++"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
# Path is relative to this script: Algorithms/DLX/C++/ -> ../../common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain g++

    # Check source files exist
    if [ ! -f "dlx_core.cpp" ] || [ ! -f "dlx_sudoku.cpp" ]; then
        report_env_error "Source files not found"
    fi

    echo "Compiling C++ DLX solver..."
    g++ -O2 -o dlx_solver dlx_sudoku.cpp dlx_core.cpp

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: ./dlx_solver"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
# Call main function from common.sh, which handles compilation and benchmarking
main "$@"
