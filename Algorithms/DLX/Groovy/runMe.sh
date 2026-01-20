#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Groovy"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain groovy

    # Create a wrapper script that runs the Groovy script
    cat > dlx_solver << 'EOF'
#!/bin/bash
exec groovy DLX.groovy "$@"
EOF
    chmod +x dlx_solver
}

main "$@"
