#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Ada"
SOLVER_BINARY="./cp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain gnatmake
    echo "Compiling Ada CP solver..."
    gnatmake -O3 cp.adb -o cp || return 1
}

main "$@"
