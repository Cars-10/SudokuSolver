#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Dart"
SOLVER_BINARY="dart run dlx.dart"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain dart
    # Dart is interpreted, no compilation needed
    return 0
}

main "$@"
