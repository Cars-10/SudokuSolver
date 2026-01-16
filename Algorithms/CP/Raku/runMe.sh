#!/bin/bash
# Algorithms/CP/Raku/runMe.sh - Raku CP Sudoku solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Raku"
SOLVER_BINARY="raku cp.raku"
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
    if [ ! -f "cp.raku" ]; then
        report_env_error "Source file cp.raku not found"
    fi

    echo "No compilation needed for Raku (interpreted)"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
