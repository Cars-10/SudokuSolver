#!/bin/bash
# Languages/Sed/runMe.sh

LANGUAGE="Sed"

# Source shared functions
source ../common.sh

# Wrapper script to handle input cleaning and output formatting
cat << 'EOF' > sed_wrapper.sh
#!/bin/bash
SED_SCRIPT="Sudoku.sed"
MATRIX="$1"

# 1. Print Path
echo "$MATRIX"
echo ""

# 2. Print Initial Puzzle
echo "Puzzle:"
cat "$MATRIX" | grep -v '^#' | sed 's/\([0-9]\)/\1 /g'
echo ""

# 3. Solve
# Clean input to 81 digits on one line
INPUT=$(cat "$MATRIX" | tr -cd '0-9')
RESULT=$(echo "$INPUT" | sed -nf "$SED_SCRIPT")

if [[ "$RESULT" == "No solution found." ]]; then
    echo "$RESULT"
    exit 0
fi

# 4. Format Result
BOARD=$(echo "$RESULT" | cut -d'|' -f1)
ITERS=$(echo "$RESULT" | cut -d'|' -f2)

echo "Puzzle:"
echo "$BOARD" | sed 's/./& /g; s/.\{18\}/&\n/g'
echo ""
echo "Solved in Iterations=$ITERS"
EOF
chmod +x sed_wrapper.sh

SOLVER_BINARY="./sed_wrapper.sh"

compile() {
    # Check for sed (standard)
    if ! command -v sed &> /dev/null;
    then
        report_env_error "sed not found"
    fi
}

# Execute benchmarks
main "$@"
