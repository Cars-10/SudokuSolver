#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Ruby"
SOLVER_BINARY="ruby cp.rb"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

main "$@"
