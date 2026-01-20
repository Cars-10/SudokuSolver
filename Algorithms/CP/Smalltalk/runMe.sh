#!/bin/bash
cd "$(dirname "$0")"

[ -d /home/linuxbrew/.linuxbrew/bin ] && export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
[ -d /opt/homebrew/bin ] && export PATH="/opt/homebrew/bin:$PATH"

LANGUAGE="Smalltalk"
SOLVER_BINARY="gst cp.st -a"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain gst
    if [ ! -f "cp.st" ]; then
        report_env_error "Source file cp.st not found"
    fi
    echo "No compilation needed for Smalltalk (interpreted)"
}

main "$@"
