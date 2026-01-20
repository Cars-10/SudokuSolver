#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Octave"
SOLVER_BINARY="octave --no-gui --no-window-system dlx.m"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain octave
}

main "$@"
