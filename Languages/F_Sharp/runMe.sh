#!/bin/bash
# Languages/F_Sharp/runMe.sh - F# Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="F#"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check dotnet availability
    check_toolchain dotnet

    echo "Building F# project..." >&2
    dotnet build --configuration Release -verbosity:quiet 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "F# build failed"
    fi
    echo "F# build successful" >&2
}

# ============================================================================
# CUSTOM MATRIX EXECUTION (override common.sh for dotnet execution)
# ============================================================================

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================
main "$@"
