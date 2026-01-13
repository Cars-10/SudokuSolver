#!/bin/bash
# Languages/Java/runMe.sh - Java Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Java"
SOLVER_BINARY="java Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Setup Java environment (macOS Homebrew)
if [ -d "/opt/homebrew/opt/openjdk" ]; then
    export JAVA_HOME="/opt/homebrew/opt/openjdk"
    export PATH="$JAVA_HOME/bin:$PATH"
fi

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check Java availability
    check_toolchain java
    check_toolchain javac

    # Check source file exists
    if [ ! -f "Sudoku.java" ]; then
        report_env_error "Sudoku.java not found"
    fi

    # Compile Java
    echo "Compiling Java..."
    javac Sudoku.java 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Java compilation failed"
    fi

    # Verify class file exists
    if [ ! -f "Sudoku.class" ]; then
        report_env_error "Compiled class not found: Sudoku.class"
    fi

    echo "Java solver ready: Sudoku.class ($(java -version 2>&1 | head -1))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
