#!/bin/bash
# Algorithms/CP/C++/runMe.sh - C++ CP Sudoku solver build and benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="C++"
ALGORITHM="CP"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
# Path is relative to this script: Algorithms/CP/C++/ -> ../../common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain g++

    # Check source files exist
    if [ ! -f "cp_core.cpp" ] || [ ! -f "cp_sudoku.cpp" ]; then
        report_env_error "Source files not found"
    fi

    echo "Compiling C++ CP solver..."
    g++ -O2 -o cp_solver cp_sudoku.cpp cp_core.cpp

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: ./cp_solver"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
# Call main function from common.sh, which handles compilation and benchmarking
main "$@"
