#!/bin/bash
# Algorithms/BruteForce/AppleScript/runMe.sh

LANGUAGE="AppleScript"

# Source shared functions
# We need to make sure common.sh works locally or provide local alternatives
source ../../common.sh

# Detect OS
OS="$(uname)"
if [ "$OS" != "Darwin" ]; then
    report_env_error "AppleScript requires macOS (Darwin). Current OS: $OS"
fi

# AppleScript uses log for stderr and do shell script echo for stdout
# We'll use a wrapper to handle this if needed, or just let common.sh capture it.
# Note: osascript 'log' goes to stderr.

SOLVER_BINARY="osascript Sudoku.applescript"

# Execute benchmarks
main "$@"
