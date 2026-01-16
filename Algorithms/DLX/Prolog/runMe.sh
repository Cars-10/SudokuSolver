#!/bin/bash
# Algorithms/DLX/Prolog/runMe.sh - Prolog DLX Sudoku solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Prolog"
SOLVER_BINARY="swipl -q -t main -s dlx.pl --"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check toolchain availability
    check_toolchain swipl

    # Check source files exist
    if [ ! -f "dlx.pl" ]; then
        report_env_error "Source file dlx.pl not found"
    fi

    echo "No compilation needed for Prolog (interpreted)"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
