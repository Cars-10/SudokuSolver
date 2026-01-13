#!/bin/bash
# Languages/F_Sharp/runMe.sh - F# Sudoku solver benchmark script

cd "$(dirname "$0")"

LANGUAGE="F_Sharp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../common.sh

compile() {
    check_toolchain dotnet

    echo "Building F# project..." >&2
    dotnet build --configuration Release -verbosity:quiet 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "F# build failed"
    fi
    echo "F# build successful" >&2
}

# Find the compiled DLL after build
find_binary() {
    local binary=$(find bin/Release -name "Sudoku.dll" -type f 2>/dev/null | sort -r | head -n 1)
    if [ -n "$binary" ]; then
        SOLVER_BINARY="dotnet $binary"
        return
    fi
    report_env_error "Could not find compiled Sudoku.dll"
}

main() {
    compile
    find_binary
    run_benchmarks "$@"
}

main "$@"
