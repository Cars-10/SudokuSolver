#!/bin/bash
# Languages/Elixir/runMe.sh - Elixir Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Elixir"
SOLVER_BINARY="elixir Sudoku.exs"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# Check toolchain
check_toolchain elixir

# ============================================================================
# CUSTOM MATRIX EXECUTION (override common.sh for elixir execution)
# ============================================================================

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================
main "$@"
