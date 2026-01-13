#!/bin/bash
# Algorithms/CP/Julia/runMe.sh - Julia CP solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Julia"
SOLVER_BINARY="julia cp.jl"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Julia - JIT compiled)
# ============================================================================
compile() {
    # Check Julia availability
    check_toolchain julia

    # Check source file exists
    if [ ! -f "cp.jl" ]; then
        report_env_error "cp.jl not found"
    fi

    echo "Julia CP solver ready: cp.jl ($(julia --version 2>&1))"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
