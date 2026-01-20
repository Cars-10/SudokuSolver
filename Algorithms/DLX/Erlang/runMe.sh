#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Erlang"
ALGORITHM="DLX"
SOLVER_BINARY="./dlx.erl"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain escript

    # Make script executable
    chmod +x dlx.erl
    return 0
}

main "$@"
