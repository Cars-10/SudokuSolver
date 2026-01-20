#!/bin/bash
# Algorithms/BruteForce/Julia/runMe.sh - Julia Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Julia"
SOLVER_BINARY="julia Sudoku.jl"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Julia - JIT compiled)
# ============================================================================
compile() {
    # Check Julia availability
    check_toolchain julia

    # Check source file exists
    if [ ! -f "Sudoku.jl" ]; then
        report_env_error "Sudoku.jl not found"
    fi

    echo "Julia solver ready: Sudoku.jl ($(julia --version 2>&1))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
