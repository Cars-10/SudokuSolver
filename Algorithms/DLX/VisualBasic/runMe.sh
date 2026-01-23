#!/bin/bash
# Algorithms/BruteForce/VisualBasic/runMe.sh - Visual Basic Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="VisualBasic"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check dotnet availability
    check_toolchain dotnet

    echo "Building Visual Basic project..." >&2
    dotnet build --configuration Release -verbosity:quiet 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Visual Basic build failed"
    fi
    echo "Visual Basic build successful" >&2
}

# ============================================================================
# CUSTOM MATRIX EXECUTION (override common.sh for dotnet execution)
# ============================================================================

# We need to find the compiled binary, as dotnet build puts it in a subdir
find_binary() {
    # Prefer running the DLL with dotnet command as it is more robust across environments
    # (AppHost executables can fail if DOTNET_ROOT is not set correctly)

    # Look for the DLL in bin/Release/net*
    # Prefer net9.0 over net8.0 if both exist (sort -r reverses order, so net9.0 comes first)
    local binary=$(find bin/Release -name "Sudoku.dll" -type f | sort -r | head -n 1)

    if [ -n "$binary" ]; then
        SOLVER_BINARY="dotnet $binary"
        return
    fi

    # Fallback to executable if DLL not found (unlikely)
    binary=$(find bin/Release -name "Sudoku" -type f | sort -r | head -n 1)
    if [ -n "$binary" ]; then
        SOLVER_BINARY="./$binary"
        return
    fi

    report_env_error "Could not find compiled binary (Sudoku.dll or Sudoku)"
}

# Override main to find binary before running
main() {
    compile
    find_binary
    run_benchmarks "$@"
}

main "$@"
