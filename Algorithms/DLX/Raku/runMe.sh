#!/bin/bash
# Algorithms/DLX/Raku/runMe.sh - Raku DLX Sudoku solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Raku"
SOLVER_BINARY="raku dlx.raku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check toolchain availability
    check_toolchain raku

    # Check source files exist
    if [ ! -f "dlx.raku" ]; then
        report_env_error "Source file dlx.raku not found"
    fi

    echo "No compilation needed for Raku (interpreted)"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
