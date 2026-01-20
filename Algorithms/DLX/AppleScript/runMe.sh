#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="AppleScript"
ALGORITHM="DLX"
SOLVER_BINARY="osascript dlx.applescript"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-600}"

source ../../common.sh

main "$@"
