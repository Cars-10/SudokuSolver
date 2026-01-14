#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Haxe"
SOLVER_BINARY="./CP"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain haxe
    haxe --main CP --cpp out -D HXCPP_OPTIMIZATION_LEVEL=2
    cp out/CP ./CP
}

main "$@"
