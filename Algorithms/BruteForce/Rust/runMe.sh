#!/bin/bash
# Languages/Rust/runMe.sh - Rust Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Rust"
SOLVER_BINARY="./Sudoku_bin"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check compiler availability
    check_toolchain cargo

    # Check source directory exists
    if [ ! -d "Sudoku" ]; then
        report_env_error "Sudoku directory not found"
    fi

    if [ ! -f "Sudoku/Cargo.toml" ]; then
        report_env_error "Cargo.toml not found"
    fi

    echo "Compiling Rust solver with --release optimization..."
    (cd Sudoku && cargo build --release)

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    # Copy binary to expected location
    cp Sudoku/target/release/Sudoku ./Sudoku_bin

    if [ ! -f "$SOLVER_BINARY" ]; then
        report_env_error "Binary not found after compilation"
    fi

    echo "Compilation successful: $SOLVER_BINARY"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
