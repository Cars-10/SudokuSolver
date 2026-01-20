#!/bin/bash
# Algorithms/DLX/Java/runMe.sh - Java DLX solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Java"
SOLVER_BINARY="java DLX"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes

# Setup Java environment (macOS Homebrew)
if [ -d "/opt/homebrew/opt/openjdk" ]; then
    export JAVA_HOME="/opt/homebrew/opt/openjdk"
    export PATH="$JAVA_HOME/bin:$PATH"
fi

# Source shared functions from common.sh (DLX is 2 levels deep)
source ../../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check Java availability
    check_toolchain java
    check_toolchain javac

    # Check source file exists
    if [ ! -f "DLX.java" ]; then
        report_env_error "DLX.java not found"
    fi

    # Compile Java
    echo "Compiling Java DLX..."
    javac DLX.java 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Java compilation failed"
    fi

    # Verify class file exists
    if [ ! -f "DLX.class" ]; then
        report_env_error "Compiled class not found: DLX.class"
    fi

    echo "Java DLX solver ready: DLX.class ($(java -version 2>&1 | head -1))"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
