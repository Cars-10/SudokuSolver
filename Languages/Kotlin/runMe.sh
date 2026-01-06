#!/bin/bash
# Languages/Kotlin/runMe.sh - Kotlin Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Kotlin"
SOLVER_BINARY="java -jar Sudoku.jar"
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
    # Check Kotlin availability
    check_toolchain kotlinc
    check_toolchain java

    # Check source file exists
    if [ ! -f "Sudoku.kt" ]; then
        report_env_error "Sudoku.kt not found"
    fi

    # Compile Kotlin to standalone JAR
    echo "Compiling Kotlin..."
    kotlinc Sudoku.kt -include-runtime -d Sudoku.jar 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Kotlin compilation failed"
    fi

    # Verify JAR file exists
    if [ ! -f "Sudoku.jar" ]; then
        report_env_error "Compiled JAR not found: Sudoku.jar"
    fi

    echo "Kotlin solver ready: Sudoku.jar ($(kotlinc -version 2>&1 | head -1))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
