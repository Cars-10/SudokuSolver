#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Haxe"
SOLVER_BINARY="./DLX"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"
source ../../common.sh

compile() {
    check_toolchain haxe
    haxe --main DLX --cpp out -D HXCPP_OPTIMIZATION_LEVEL=2
    cp out/DLX ./DLX
}

main "$@"
