#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="TypeScript"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain tsc
    check_toolchain node

    log "Compiling dlx.ts..."
    tsc --strict --target ES2020 --module commonjs dlx.ts || return 1

    log "Creating dlx_solver wrapper..."
    cat > dlx_solver << 'EOF'
#!/bin/bash
node "$(dirname "$0")/dlx.js" "$@"
EOF
    chmod +x dlx_solver

    return 0
}

main "$@"
