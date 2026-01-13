#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Crystal"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain crystal
    crystal build --release -o dlx_solver dlx.cr
}

main "$@"
