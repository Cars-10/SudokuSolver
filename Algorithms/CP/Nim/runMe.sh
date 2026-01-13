#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Nim"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain nim
    nim c -d:release -o:cp_solver cp.nim
}

main "$@"
