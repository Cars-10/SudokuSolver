#!/bin/bash
# Algorithms/CP/Python/runMe.sh - Python CP solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Python"
SOLVER_BINARY="python3 cp.py"
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
    if [ ! -f "cp.py" ]; then
        report_env_error "cp.py not found"
    fi

    # Verify Python can import the script (syntax check)
    python3 -m py_compile cp.py 2>/dev/null
    if [ $? -ne 0 ]; then
        report_env_error "cp.py has syntax errors"
    fi

    echo "Python CP solver ready: cp.py"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
