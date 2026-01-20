#!/opt/homebrew/bin/bash
cd "$(dirname "$0")"

LANGUAGE="BASH"
SOLVER_BINARY="/opt/homebrew/bin/bash cp.sh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    # Check bash version >= 4.0 for bitwise operations
    if [[ "${BASH_VERSINFO[0]}" -lt 4 ]]; then
        report_env_error "BASH 4.0+ required for bitwise operations (current: ${BASH_VERSION})"
        return 1
    fi

    # Check if bc is available for floating point math
    if ! command -v bc &> /dev/null; then
        report_env_error "bc command required for time calculations"
        return 1
    fi

    echo "BASH version: ${BASH_VERSION}"
    return 0
}

main "$@"
