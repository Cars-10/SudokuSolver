#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Swift"
ALGORITHM="CP"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain swiftc
    swiftc -O cp.swift -o cp_solver
}

main "$@"
