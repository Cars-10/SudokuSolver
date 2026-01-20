#!/bin/bash
# Algorithms/DLX/Objective-C/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Objective-C"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source shared functions
source ../../common.sh

compile() {
    echo "Compiling Objective-C DLX solver..."

    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS native compilation
        check_toolchain clang
        clang -O2 -framework Foundation -o dlx_solver dlx_core.m dlx_sudoku.m
    else
        # Linux requires GNUstep
        if ! command -v gnustep-config &> /dev/null; then
            report_env_error "GNUstep not found. Install with: apt install gnustep-devel gnustep-make"
        fi
        OBJC_FLAGS=$(gnustep-config --objc-flags 2>/dev/null)
        OBJC_LIBS=$(gnustep-config --objc-libs 2>/dev/null)
        if [ -z "$OBJC_FLAGS" ]; then
            report_env_error "gnustep-config failed. GNUstep may not be properly installed"
        fi
        gcc -std=c99 -O2 -o dlx_solver dlx_core.m dlx_sudoku.m $OBJC_FLAGS $OBJC_LIBS -lgnustep-base
    fi

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Execute benchmarks
main "$@"
