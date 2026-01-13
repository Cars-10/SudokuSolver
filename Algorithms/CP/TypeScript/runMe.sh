#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="TypeScript"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain tsc
    check_toolchain node

    log "Compiling cp.ts..."
    tsc --strict --target ES2020 --module commonjs cp.ts || return 1

    log "Creating cp_solver wrapper..."
    cat > cp_solver << 'EOF'
#!/bin/bash
node "$(dirname "$0")/cp.js" "$@"
EOF
    chmod +x cp_solver

    return 0
}

main "$@"
