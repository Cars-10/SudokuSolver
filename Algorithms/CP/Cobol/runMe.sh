#!/bin/bash
# Algorithms/CP/Cobol/runMe.sh - Cobol CP Sudoku solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Cobol"
ALGORITHM="CP"
SOLVER_BINARY="./cp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source shared functions from common.sh
# Path is relative to this script: Algorithms/CP/Cobol/ -> ../../common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Prefer /usr/bin/cobc as specified
    COBC="/usr/bin/cobc"
    if [ ! -x "$COBC" ]; then
        if command -v cobc &> /dev/null; then
            COBC="cobc"
        else
            report_env_error "GnuCOBOL compiler (cobc) not found"
        fi
    fi

    echo "Compiling COBOL CP solver..."
    # -x: Build executable
    # Note: cp.cob has >SOURCE FORMAT FREE directive, so no -free flag needed
    "$COBC" -x -o cp cp.cob

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: ./cp"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
# Call main function from common.sh, which handles compilation and benchmarking
main "$@"
