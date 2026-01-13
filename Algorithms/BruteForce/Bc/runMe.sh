#!/bin/bash
# Algorithms/BruteForce/Bc/runMe.sh

LANGUAGE="Bc"

# Source shared functions
source ../../common.sh

# bc wrapper to handle matrix input
cat << 'EOF' > bc_wrapper.sh
#!/bin/bash
MATRIX="$1"
# 1. Print path
echo "$MATRIX"

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Generate bc assignments
BC_INPUT="iterations = 0; "
for (( i=0; i<${#DIGITS}; i++ )); do
    BC_INPUT+="board[$i] = ${DIGITS:$i:1}; "
done

# 4. Run bc
echo "$BC_INPUT" | cat - Sudoku.bc | bc -q
EOF
chmod +x bc_wrapper.sh

SOLVER_BINARY="./bc_wrapper.sh"

compile() {
    # Check for bc (already verified in Docker)
    if ! command -v bc &> /dev/null; then
        report_env_error "bc not found"
    fi
}

# Execute benchmarks
main "$@"
