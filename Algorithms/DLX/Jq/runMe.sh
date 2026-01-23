#!/bin/bash
# Algorithms/BruteForce/Jq/runMe.sh

LANGUAGE="Jq"

# Source shared functions
source ../../common.sh

cat << 'EOF' > jq_wrapper.sh
#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Initial Puzzle
echo "Puzzle:"
cat "$MATRIX" | grep -v '^#' | sed 's/\([0-9]\)/\1 /g'
echo ""

# 3. Solve
INPUT=$(cat "$MATRIX" | tr -cd '0-9')
# -R for raw input, -r for raw output
jq -Rr -f Sudoku.jq <<< "$INPUT"
EOF
chmod +x jq_wrapper.sh

SOLVER_BINARY="./jq_wrapper.sh"

compile() {
    if ! command -v jq &> /dev/null; then
        report_env_error "jq not found"
    fi
}

# Execute benchmarks
main "$@"
