#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="SML"
SOLVER_BINARY="./dlx"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    # Check for SML compiler (prefer mlton, fall back to polyml, then smlnj)
    if command -v mlton &> /dev/null; then
        echo "Compiling with MLton..." >&2
        mlton -output dlx dlx.sml
        if [ $? -ne 0 ]; then
            report_env_error "MLton compilation failed"
        fi
    elif command -v polyc &> /dev/null; then
        echo "Compiling with Poly/ML..." >&2
        polyc -o dlx dlx.sml
        if [ $? -ne 0 ]; then
            report_env_error "Poly/ML compilation failed"
        fi
    elif command -v poly &> /dev/null; then
        echo "Using Poly/ML interpreter..." >&2
        # Create a wrapper script for interpreter mode
        cat > dlx <<'EOF'
#!/bin/bash
# Use Poly/ML to run the SML code
# We suppress the start-up message and exit after execution
poly -q --use dlx.sml --eval "OS.Process.exit(OS.Process.success)" "$@" 2>&1 | grep -v "^val " | grep -v "^>"
EOF
        chmod +x dlx
    elif command -v sml &> /dev/null || [ -x /usr/local/smlnj/bin/sml ]; then
        # Determine path to sml
        SML_PATH=$(command -v sml 2>/dev/null || echo "/usr/local/smlnj/bin/sml")

        echo "Using SML/NJ interpreter at $SML_PATH..." >&2

        # Create a wrapper that generates a modified source file with hardcoded arguments
        cat > dlx <<'WRAPPER_EOF'
#!/bin/bash
DIR="$(cd "$(dirname "$0")" && pwd)"
SML_PATH="SMLPATH_PLACEHOLDER"

# Create temp file with modified source
TMP_FILE="$DIR/.dlx_$$.sml"
trap "rm -f '$TMP_FILE'" EXIT

# Build argument list
ARGS=""
for arg in "$@"; do
    [ -n "$ARGS" ] && ARGS="$ARGS, "
    # Escape backslashes and quotes in the argument
    ESCAPED=$(echo "$arg" | sed 's/\\/\\\\/g; s/"/\\"/g')
    ARGS="$ARGS\"$ESCAPED\""
done

# Copy source and replace CommandLine.arguments() with our hardcoded args
# Use | as delimiter to avoid conflicts with / in paths
sed "s|CommandLine\.arguments ()|let val _ = () in [$ARGS] end|g" "$DIR/dlx.sml" > "$TMP_FILE"

# Run SML
cd "$DIR" && $SML_PATH < "$TMP_FILE" 2>&1 | tail -n +3
WRAPPER_EOF

        # Substitute SML path
        sed -i.bak "s|SMLPATH_PLACEHOLDER|$SML_PATH|" dlx
        rm -f dlx.bak
        chmod +x dlx
    else
        report_env_error "No SML compiler found (tried mlton, polyc, poly, sml)"
    fi
}

main "$@"
