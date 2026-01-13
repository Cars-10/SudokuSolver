#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="SML"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

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
    elif command -v sml &> /dev/null; then
        echo "Compiling with SML/NJ..." >&2
        # SML/NJ uses heap images, create a build script
        cat > build.sml <<'EOF'
CM.make "$/basis.cm";
use "Sudoku.sml";
EOF
        # For SML/NJ, we'll use interpreter mode
        # Create a wrapper script
        cat > Sudoku <<'EOF'
#!/bin/bash
sml @SMLload=Sudoku "$@"
EOF
        chmod +x Sudoku

        # Try to export heap image
        echo 'use "Sudoku.sml"; SMLofNJ.exportFn("Sudoku", main);' | sml 2>&1 | grep -q "Error" && {
            # If heap export fails, fall back to direct execution
            cat > Sudoku <<'EOF'
#!/bin/bash
echo 'use "Sudoku.sml";' | sml "$@" 2>&1 | tail -n +3
EOF
            chmod +x Sudoku
        }
    else
        report_env_error "No SML compiler found (tried mlton, polyc, poly, sml)"
    fi
}

main "$@"
