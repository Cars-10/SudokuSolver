#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Elixir"
ALGORITHM="CP"
SOLVER_BINARY="elixir cp.exs"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain elixir
}

main "$@"
