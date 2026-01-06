#!/bin/bash
# Languages/Scala/runMe.sh - Scala Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Scala"
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
    # Check Scala availability
    check_toolchain scalac

    echo "Compiling Scala..." >&2
    scalac Sudoku.scala
    if [ $? -ne 0 ]; then
        report_env_error "Scala compilation failed"
    fi
    echo "Scala compilation successful" >&2
}

# ============================================================================
# CUSTOM MATRIX EXECUTION (override common.sh for Scala runner)
# ============================================================================

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================
main "$@"
