#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="JavaScript"
SOLVER_BINARY="node dlx.js"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

main "$@"
