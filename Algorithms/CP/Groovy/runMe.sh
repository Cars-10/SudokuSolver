#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Groovy"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain groovy

    # Create a wrapper script that runs the Groovy script
    cat > cp_solver << 'EOF'
#!/bin/bash
exec groovy CP.groovy "$@"
EOF
    chmod +x cp_solver
}

main "$@"
