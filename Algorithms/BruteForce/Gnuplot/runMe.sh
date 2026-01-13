#!/bin/bash
# Languages/Gnuplot/runMe.sh

LANGUAGE="Gnuplot"

# Source shared functions
source ../../common.sh

# gnuplot wrapper
cat << 'EOF' > gp_wrapper.sh
#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Generate gnuplot script
SCRIPT_FILE=$(mktemp)
echo "iterations = 0; array board[81];" >> "$SCRIPT_FILE"
for (( i=0; i<${#DIGITS}; i++ )); do
    echo "board[$((i+1))] = ${DIGITS:$i:1};" >> "$SCRIPT_FILE"
done

# 4. Initial Puzzle
echo "Puzzle:"
cat "$MATRIX" | grep -v '^#' | sed 's/\([0-9]\)/\1 /g'
echo ""

# 5. Run gnuplot
cat Sudoku.gp >> "$SCRIPT_FILE"
gnuplot "$SCRIPT_FILE"
rm "$SCRIPT_FILE"
EOF
chmod +x gp_wrapper.sh

SOLVER_BINARY="./gp_wrapper.sh"

compile() {
    if ! command -v gnuplot &> /dev/null; then
        report_env_error "gnuplot not found"
    fi
}

# Execute benchmarks
main "$@"