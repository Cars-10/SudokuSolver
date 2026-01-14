#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="V"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain v
    v -enable-globals -prod dlx.v -o dlx_solver
}

main "$@"
