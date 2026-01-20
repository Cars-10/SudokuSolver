#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="D"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

# Override matrix path for CP (one level deeper than BruteForce)
if [ $# -eq 0 ]; then
    set -- ../../../Matrices/*.matrix
fi

compile() {
    # Check for D compilers (prefer ldc2)
    if command -v ldc2 >/dev/null 2>&1; then
        ldc2 -O3 -release -of=cp_solver cp.d
    elif command -v dmd >/dev/null 2>&1; then
        dmd -O -release -of=cp_solver cp.d
    else
        report_env_error "No D compiler found (need dmd or ldc2)"
    fi
}

main "$@"
