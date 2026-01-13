#!/bin/bash
# Languages/Smalltalk/runMe.sh

cd "$(dirname "$0")"

# Add gst to PATH if available (linuxbrew or macOS Homebrew)
[ -d /home/linuxbrew/.linuxbrew/bin ] && export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
[ -d /opt/homebrew/bin ] && export PATH="/opt/homebrew/bin:$PATH"

LANGUAGE="Smalltalk"
SOLVER_BINARY="gst Sudoku.st -a"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../../common.sh

compile() {
    check_toolchain gst
}

# Execute benchmarks
main "$@"
