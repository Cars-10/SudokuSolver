#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="SML"
SOLVER_BINARY="./cp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    # Check for SML compiler (prefer mlton, fall back to polyml, then smlnj)
    if command -v mlton &> /dev/null; then
        echo "Compiling with MLton..." >&2
        mlton -output cp cp.sml
        if [ $? -ne 0 ]; then
            report_env_error "MLton compilation failed"
        fi
    elif command -v polyc &> /dev/null; then
        echo "Compiling with Poly/ML..." >&2
        polyc -o cp cp.sml
        if [ $? -ne 0 ]; then
            report_env_error "Poly/ML compilation failed"
        fi
    elif command -v poly &> /dev/null; then
        echo "Using Poly/ML interpreter..." >&2
        # Create a wrapper script for interpreter mode
        cat > cp <<'EOF'
#!/bin/bash
# Use Poly/ML to run the SML code
# We suppress the start-up message and exit after execution
poly -q --use cp.sml --eval "OS.Process.exit(OS.Process.success)" "$@" 2>&1 | grep -v "^val " | grep -v "^>"
EOF
        chmod +x cp
    elif command -v sml &> /dev/null; then
        echo "Compiling with SML/NJ..." >&2
        # Create a wrapper script for SML/NJ
        cat > cp <<'EOF'
#!/bin/bash
echo 'use "cp.sml";' | sml "$@" 2>&1 | tail -n +3
EOF
        chmod +x cp
    else
        report_env_error "No SML compiler found (tried mlton, polyc, poly, sml)"
    fi
}

main "$@"
