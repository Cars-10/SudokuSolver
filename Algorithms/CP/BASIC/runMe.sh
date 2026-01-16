#!/bin/bash
cd "$(dirname "$0")"

[ -x /Users/vibe/Downloads/FB-Mojave/local/bin/fbc ] && export PATH="/Users/vibe/Downloads/FB-Mojave/local/bin:$PATH"

LANGUAGE="BASIC"
SOLVER_BINARY="./cp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain fbc
    fbc cp.bas -x cp
}

main "$@"
