#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="V"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain v
    v -enable-globals -prod cp.v -o cp_solver
}

main "$@"
