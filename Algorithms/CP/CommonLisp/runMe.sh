#!/bin/bash
# Common Lisp CP Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="CommonLisp"
ALGORITHM="CP"
SOLVER_BINARY="sbcl --script cp.lisp --"
COMPILE_CMD=""  # No compilation needed
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source common functions
source ../../common.sh

# Check toolchain
check_toolchain() {
    if ! command -v sbcl &> /dev/null; then
        if ! command -v ccl &> /dev/null; then
            report_env_error "Common Lisp compiler not found. Install SBCL or CCL."
        else
            # Use CCL if SBCL not available
            SOLVER_BINARY="ccl --load cp.lisp --"
        fi
    fi
}

# Check for Common Lisp
check_toolchain

# Run benchmarks
main "$@"
