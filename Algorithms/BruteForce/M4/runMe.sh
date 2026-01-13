#!/bin/bash
# Algorithms/BruteForce/M4/runMe.sh

LANGUAGE="M4"

# Source shared functions
source ../../common.sh

# m4 wrapper
cat << 'EOF' > m4_wrapper.sh
#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Generate m4 board initialization
M4_INIT="changequote([, ])dnl
"
for (( i=0; i<${#DIGITS}; i++ )); do
    R=$(( i / 9 ))
    C=$(( i % 9 ))
    VAL=${DIGITS:$i:1}
    M4_INIT+="define([cell_${R}_${C}], [$VAL])dnl
"
done

# 4. Run m4 (redirect stderr to stdout)
(echo "$M4_INIT"; cat Sudoku.m4) | m4 -L 10000 2>&1
EOF
chmod +x m4_wrapper.sh

SOLVER_BINARY="./m4_wrapper.sh"

compile() {
    if ! command -v m4 &> /dev/null; then
        report_env_error "m4 not found"
    fi
}

# Execute benchmarks
main "$@"