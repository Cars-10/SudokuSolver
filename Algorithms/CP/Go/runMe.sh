#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Go"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain go
    go build -o cp_solver cp.go
}

main "$@"
