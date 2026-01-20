#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Elixir"
ALGORITHM="DLX"
SOLVER_BINARY="elixir dlx.exs"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain elixir
}

main "$@"
