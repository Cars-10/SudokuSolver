#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="OCaml"
SOLVER_BINARY="./cp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain ocamlopt
    ocamlopt -o cp unix.cmxa cp.ml
}

main "$@"
