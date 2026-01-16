#!/bin/bash
# Algorithms/DLX/CoffeeScript/runMe.sh - CoffeeScript DLX Sudoku solver build and benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="CoffeeScript"
SOLVER_BINARY="node dlx.js"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check toolchain availability
    check_toolchain coffee
    check_toolchain node

    # Check source files exist
    if [ ! -f "dlx.coffee" ]; then
        report_env_error "Source file dlx.coffee not found"
    fi

    echo "Compiling CoffeeScript DLX solver..."
    coffee -c dlx.coffee

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: dlx.js"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
