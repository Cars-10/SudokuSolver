#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="OCaml"
SOLVER_BINARY="./dlx"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain ocamlopt
    ocamlopt -o dlx unix.cmxa dlx.ml
}

main "$@"
