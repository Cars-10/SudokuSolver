#!/bin/bash
# Languages/Lua/runMe.sh - Lua Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Lua"
SOLVER_BINARY="lua Sudoku.lua"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Lua - interpreted language)
# ============================================================================
compile() {
    # Check Lua availability
    check_toolchain lua

    # Check source file exists
    if [ ! -f "Sudoku.lua" ]; then
        report_env_error "Sudoku.lua not found"
    fi

    echo "Lua solver ready: Sudoku.lua ($(lua -v 2>&1 | head -1))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
