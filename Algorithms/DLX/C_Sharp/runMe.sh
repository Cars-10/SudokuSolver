#!/bin/bash
# Algorithms/DLX/C_Sharp/runMe.sh - C# DLX solver benchmark script

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="C_Sharp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check dotnet availability
    check_toolchain dotnet

    echo "Building C# project..." >&2
    dotnet build --configuration Release -verbosity:quiet 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "C# build failed"
    fi
    echo "C# build successful" >&2
}

# ============================================================================
# CUSTOM MATRIX EXECUTION (override common.sh for dotnet execution)
# ============================================================================

# We need to find the compiled binary, as dotnet build puts it in a subdir
find_binary() {
    # Prefer running the DLL with dotnet command as it is more robust across environments
    # Look for the DLL in bin/Release/net*
    local binary=$(find bin/Release -name "DLX.dll" -type f | sort -r | head -n 1)

    if [ -n "$binary" ]; then
        SOLVER_BINARY="dotnet $binary"
        return
    fi

    # Fallback to executable if DLL not found
    binary=$(find bin/Release -name "DLX" -type f | sort -r | head -n 1)
    if [ -n "$binary" ]; then
        SOLVER_BINARY="./$binary"
        return
    fi

    report_env_error "Could not find compiled binary (DLX.dll or DLX)"
}

# Override main to find binary before running
main() {
    compile
    find_binary
    run_benchmarks "$@"
}

main "$@"
