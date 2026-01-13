#!/bin/bash
# Languages/Assembly/runMe.sh - Assembly Sudoku solver benchmark script

cd "$(dirname "$0")"

LANGUAGE="Assembly"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions from common.sh
source ../../common.sh

# Detect architecture
ARCH=$(uname -m)
echo "Detected architecture: $ARCH"

compile() {
    if [[ "$ARCH" == "arm64" || "$ARCH" == "aarch64" ]]; then
        echo "Compiling for ARM64..."
        if [ ! -f "Sudoku_arm64.s" ]; then
             report_env_error "Sudoku_arm64.s not found"
        fi
        
        # Use GCC to assemble and link
        gcc -o sudoku Sudoku_arm64.s
        
        if [ $? -ne 0 ]; then
            report_env_error "Compilation failed for ARM64"
        fi
        
    elif [[ "$ARCH" == "x86_64" ]]; then
        echo "Compiling for x86_64..."
        check_toolchain nasm
        
        if [ ! -f "Sudoku.asm" ]; then
             report_env_error "Sudoku.asm not found"
        fi

        nasm -f elf64 Sudoku.asm -o Sudoku.o
        gcc Sudoku.o -o sudoku
        
        if [ $? -ne 0 ]; then
             report_env_error "Compilation failed for x86_64"
        fi
    else
        report_env_error "Unsupported architecture: $ARCH"
    fi
}

compile

# Set the binary to be executed
SOLVER_BINARY="./sudoku"

# Main execution
main "$@"
