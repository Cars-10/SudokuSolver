#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Crystal"
ALGORITHM="CP"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain crystal
    crystal build --release -o cp_solver cp.cr
}

main "$@"
