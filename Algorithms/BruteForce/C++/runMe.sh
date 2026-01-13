#!/bin/bash
# Algorithms/BruteForce/C++/runMe.sh - C++ Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="C++"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain g++

    # Check source file exists
    if [ ! -f "Sudoku.cpp" ]; then
        report_env_error "Sudoku.cpp not found"
    fi

    echo "Compiling C++ solver with -O3 optimization..."
    g++ -O3 -o Sudoku Sudoku.cpp

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
        O0) echo "-O0" ;;
        O1) echo "-O1" ;;
        O2) echo "-O2" ;;
        O3|default) echo "-O3" ;;
        Ofast) echo "-Ofast" ;;
        Os) echo "-Os" ;;
        *)  echo "-O3" ;;
    esac
}

# Optional: Support VARIANT environment variable
if [ -n "$VARIANT" ]; then
    COMPILE_FLAGS=$(get_compile_flags "$VARIANT")
    echo "Using variant: $VARIANT ($COMPILE_FLAGS)"

    compile() {
        check_toolchain g++
        if [ ! -f "Sudoku.cpp" ]; then
            report_env_error "Sudoku.cpp not found"
        fi

        echo "Compiling C++ solver with $COMPILE_FLAGS..."
        g++ $COMPILE_FLAGS -o Sudoku Sudoku.cpp

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
