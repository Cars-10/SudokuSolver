#!/bin/bash
# Algorithms/DLX/Python/runMe.sh - Python DLX solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Python"
SOLVER_BINARY="python3 dlx.py"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../../common.sh

# ============================================================================
# COMPILATION (not needed for Python - interpreted language)
# ============================================================================
compile() {
    # Check Python3 availability
    check_toolchain python3

    # Check source file exists
    if [ ! -f "dlx.py" ]; then
        report_env_error "dlx.py not found"
    fi

    # Verify Python can import the script (syntax check)
    python3 -m py_compile dlx.py 2>/dev/null
    if [ $? -ne 0 ]; then
        report_env_error "dlx.py has syntax errors"
    fi

    echo "Python DLX solver ready: dlx.py"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
