#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="SML"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    # Check for SML compiler (prefer mlton, fall back to polyml, then smlnj)
    if command -v mlton &> /dev/null; then
        echo "Compiling with MLton..." >&2
        mlton -output Sudoku Sudoku.sml
        if [ $? -ne 0 ]; then
            report_env_error "MLton compilation failed"
        fi
    elif command -v polyc &> /dev/null; then
        echo "Compiling with Poly/ML..." >&2
        polyc -o Sudoku Sudoku.sml
        if [ $? -ne 0 ]; then
            report_env_error "Poly/ML compilation failed"
        fi
    elif command -v poly &> /dev/null; then
        echo "Using Poly/ML interpreter..." >&2
        # Create a wrapper script for interpreter mode
        cat > Sudoku <<'EOF'
#!/bin/bash
# Use Poly/ML to run the SML code
# We suppress the start-up message and exit after execution
poly -q --use Sudoku.sml --eval "OS.Process.exit(OS.Process.success)" "$@" 2>&1 | grep -v "^val " | grep -v "^>"
EOF
        chmod +x Sudoku
    elif command -v sml &> /dev/null || [ -x /usr/local/smlnj/bin/sml ]; then
        # Determine path to sml
        SML_PATH=$(command -v sml 2>/dev/null || echo "/usr/local/smlnj/bin/sml")

        echo "Using SML/NJ interpreter at $SML_PATH..." >&2

        # Create a simple wrapper that generates a launcher file
        cat > Sudoku <<'WRAPPER_EOF'
#!/bin/bash
DIR="$(cd "$(dirname "$0")" && pwd)"
SML_PATH="SMLPATH_PLACEHOLDER"

# Create launcher file
LAUNCHER="$DIR/.launcher_$$.sml"
trap "rm -f '$LAUNCHER'" EXIT

# Generate SML code that includes our arguments
cat > "$LAUNCHER" <<'SMLCODE'
use "Sudoku.sml";

(* Run with hardcoded arguments *)
let
    val myArgs = [ARGS_PLACEHOLDER]
    val startTime = Time.now ()
    fun processFile filename =
        if String.isSuffix ".matrix" filename then
            let
                val puzzle = readMatrixFile filename
                val _ = printPuzzle puzzle
                val _ = iterations := 0
                val _ = solve puzzle
            in
                ()
            end
        else
            ()
    val _ = List.app processFile myArgs
    val endTime = Time.now ()
    val duration = Time.toReal (Time.-(endTime, startTime))
in
    print ("Seconds to process " ^ Real.fmt (StringCvt.FIX (SOME 3)) duration ^ "\n")
end;
SMLCODE

# Build argument list
ARGS=""
for arg in "$@"; do
    [ -n "$ARGS" ] && ARGS="$ARGS, "
    ARGS="$ARGS\"$arg\""
done

# Substitute arguments into launcher
if [ -n "$ARGS" ]; then
    sed -i.bak "s|ARGS_PLACEHOLDER|$ARGS|" "$LAUNCHER"
else
    sed -i.bak "s|ARGS_PLACEHOLDER||" "$LAUNCHER"
fi
rm -f "$LAUNCHER.bak"

# Run SML
cd "$DIR" && $SML_PATH < "$LAUNCHER" 2>&1 | tail -n +3
WRAPPER_EOF

        # Substitute SML path
        sed -i.bak "s|SMLPATH_PLACEHOLDER|$SML_PATH|" Sudoku
        rm -f Sudoku.bak
        chmod +x Sudoku
    else
        report_env_error "No SML compiler found (tried mlton, polyc, poly, sml)"
    fi
}

main "$@"
