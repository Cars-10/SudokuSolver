#!/bin/bash
# Languages/Swift/runMe.sh - Swift Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Swift"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain swiftc

    # Check source file exists
    if [ ! -f "Sudoku.swift" ]; then
        report_env_error "Sudoku.swift not found"
    fi

    echo "Compiling Swift solver..."
    swiftc -O -o Sudoku Sudoku.swift 2>&1

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: ./Sudoku"
}

# ============================================================================
# COMPILER VARIANTS (for future use)
# ============================================================================
get_compile_flags() {
    local variant="${1:-default}"
    case "$variant" in
        debug)    echo "-g" ;;
        release)  echo "-O" ;;
        unchecked) echo "-O -Ounchecked" ;;
        default)  echo "-O" ;;
        *)        echo "-O" ;;
    esac
}

# Optional: Support VARIANT environment variable
if [ -n "$VARIANT" ]; then
    COMPILE_FLAGS=$(get_compile_flags "$VARIANT")
    echo "Using variant: $VARIANT ($COMPILE_FLAGS)"

    compile() {
        check_toolchain swiftc
        if [ ! -f "Sudoku.swift" ]; then
            report_env_error "Sudoku.swift not found"
        fi

        echo "Compiling Swift solver with $COMPILE_FLAGS..."
        swiftc $COMPILE_FLAGS -o Sudoku Sudoku.swift 2>&1

        if [ $? -ne 0 ]; then
            report_env_error "Compilation failed with $COMPILE_FLAGS"
        fi

        echo "Compilation successful"
    }
fi

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
