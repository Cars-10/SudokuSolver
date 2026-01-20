#!/bin/bash
# Algorithms/BruteForce/Ada/runMe.sh - Ada Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# Add GNAT to PATH (Alire installation on macOS)
if [ -d "$HOME/.local/share/alire/toolchains" ]; then
    GNAT_DIR=$(find "$HOME/.local/share/alire/toolchains" -name "gnat_native*" -type d | head -1)
    [ -n "$GNAT_DIR" ] && export PATH="$GNAT_DIR/bin:$PATH"
fi

# Add Alire to PATH
[ -x /Users/vibe/Downloads/bin/alr ] && export PATH="/Users/vibe/Downloads/bin:$PATH"

# Configure SDK for macOS linker
export SDKROOT="/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
export LIBRARY_PATH="$SDKROOT/usr/lib"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Ada"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain gnatmake

    # Check source file exists
    if [ ! -f "Sudoku.adb" ]; then
        report_env_error "Sudoku.adb not found"
    fi

    echo "Compiling Ada solver with -O2 optimization..."
    gnatmake -O2 -o Sudoku Sudoku.adb

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    if [ ! -f "$SOLVER_BINARY" ]; then
        report_env_error "Binary not found after compilation"
    fi

    echo "Compilation successful: $SOLVER_BINARY"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
