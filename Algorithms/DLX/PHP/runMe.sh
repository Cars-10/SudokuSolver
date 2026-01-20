#!/bin/bash
cd "$(dirname "$0")"

# Configuration
LANGUAGE="PHP"
SOLVER_BINARY="php dlx.php"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source common benchmark functions
source ../../common.sh

# Compilation function (syntax check for interpreted language)
compile() {
    check_toolchain php

    # Syntax check
    php -l dlx.php > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "PHP syntax check failed"
        return 1
    fi

    return 0
}

# Run main benchmark
main "$@"
