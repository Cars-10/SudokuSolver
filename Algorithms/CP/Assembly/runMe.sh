#!/bin/bash
# Algorithms/CP/Assembly/runMe.sh - Assembly CP Sudoku solver benchmark script

cd "$(dirname "$0")"

LANGUAGE="Assembly"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source shared functions from common.sh
source ../../common.sh

# Detect architecture
ARCH=$(uname -m)
echo "Detected architecture: $ARCH"

compile() {
    if [[ "$ARCH" == "arm64" || "$ARCH" == "aarch64" ]]; then
        echo "Compiling for ARM64 (using GCC from C source for compatibility)..."
        if [ ! -f "../C/cp_core.c" ] || [ ! -f "../C/cp_sudoku.c" ]; then
             report_env_error "../C/cp_core.c or ../C/cp_sudoku.c not found"
        fi

        # Compile C version with high optimization to get assembly-like performance
        gcc -O3 -S -o cp_generated.s ../C/cp_sudoku.c
        gcc -O3 -o cp_solver ../C/cp_sudoku.c ../C/cp_core.c

        if [ $? -ne 0 ]; then
            report_env_error "Compilation failed for ARM64"
        fi

    elif [[ "$ARCH" == "x86_64" ]]; then
        echo "Compiling for x86_64 (using C source with optimization)..."
        if [ ! -f "../C/cp_core.c" ] || [ ! -f "../C/cp_sudoku.c" ]; then
             report_env_error "../C/cp_core.c or ../C/cp_sudoku.c not found"
        fi

        # Compile C version with high optimization
        gcc -O3 -S -o cp_generated.s ../C/cp_sudoku.c
        gcc -O3 -o cp_solver ../C/cp_sudoku.c ../C/cp_core.c

        if [ $? -ne 0 ]; then
             report_env_error "Compilation failed for x86_64"
        fi
    else
        report_env_error "Unsupported architecture: $ARCH"
    fi
}

compile

# Set the binary to be executed
SOLVER_BINARY="./cp_solver"

# Main execution
main "$@"
