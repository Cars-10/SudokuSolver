#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Swift"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain swiftc
    swiftc -O dlx.swift -o dlx_solver
}

main "$@"
