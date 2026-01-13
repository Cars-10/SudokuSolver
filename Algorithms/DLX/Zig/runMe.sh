#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Zig"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain zig
    zig build-exe -O ReleaseFast -femit-bin=dlx_solver dlx.zig
}

main "$@"
