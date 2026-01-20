#!/bin/bash
# Algorithms/DLX/Julia/runMe.sh - Julia DLX solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Julia"
SOLVER_BINARY="julia dlx.jl"
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
    if [ ! -f "dlx.jl" ]; then
        report_env_error "dlx.jl not found"
    fi

    echo "Julia DLX solver ready: dlx.jl ($(julia --version 2>&1))"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
