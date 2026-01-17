#!/bin/bash
# Algorithms/DLX/Rust/runMe.sh - Rust DLX Sudoku solver build and benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Rust"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
# Path is relative to this script: Algorithms/DLX/Rust/ -> ../../common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain rustc

    # Check source files exist
    if [ ! -f "dlx.rs" ]; then
        report_env_error "Source file dlx.rs not found"
    fi

    echo "Compiling Rust DLX solver..."
    rustc -O -o dlx_solver dlx.rs

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
