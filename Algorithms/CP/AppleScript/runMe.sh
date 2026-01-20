#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="AppleScript"
ALGORITHM="CP"
SOLVER_BINARY="osascript cp.applescript"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-600}" # AppleScript is slow

source ../../common.sh

main "$@"
