#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Ada"
SOLVER_BINARY="./dlx"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain gnatmake
    echo "Compiling Ada DLX solver..."
    gnatmake -O3 dlx.adb -o dlx || return 1
}

main "$@"
