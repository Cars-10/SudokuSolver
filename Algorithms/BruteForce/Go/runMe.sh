#!/bin/bash
# Languages/Go/runMe.sh - Go Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Go"
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
    check_toolchain go

    # Check source file exists
    if [ ! -f "Sudoku.go" ]; then
        report_env_error "Sudoku.go not found"
    fi

    echo "Compiling Go solver..."
    go build -o Sudoku Sudoku.go

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful: ./Sudoku"
}

# ============================================================================
# COMPILER VARIANTS (for future use)
# ============================================================================
get_compile_flags() {
    # Go doesn't have optimization flags like C/C++
    # Could add build tags or ldflags in future
    # -ldflags="-s -w" strips debug info for smaller binary
    local variant="${1:-default}"
    case "$variant" in
        stripped) echo "-ldflags=-s -w" ;;
        race)     echo "-race" ;;
        default)  echo "" ;;
        *)        echo "" ;;
    esac
}

# Optional: Support VARIANT environment variable
if [ -n "$VARIANT" ]; then
    COMPILE_FLAGS=$(get_compile_flags "$VARIANT")
    echo "Using variant: $VARIANT ($COMPILE_FLAGS)"

    compile() {
        check_toolchain go
        if [ ! -f "Sudoku.go" ]; then
            report_env_error "Sudoku.go not found"
        fi

        echo "Compiling Go solver with $COMPILE_FLAGS..."
        go build $COMPILE_FLAGS -o Sudoku Sudoku.go

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
