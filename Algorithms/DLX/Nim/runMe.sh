#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Nim"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain nim
    nim c -d:release -o:dlx_solver dlx.nim
}

main "$@"
