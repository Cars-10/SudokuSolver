#!/bin/bash
# Algorithms/CP/CoffeeScript/runMe.sh - CoffeeScript CP Sudoku solver build and benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="CoffeeScript"
SOLVER_BINARY="node cp.js"
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
    if [ ! -f "cp.coffee" ]; then
        report_env_error "Source file cp.coffee not found"
    fi

    echo "Compiling CoffeeScript CP solver..."
    coffee -c cp.coffee

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: cp.js"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
