#!/bin/bash
# Algorithms/BruteForce/Jupyter/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Jupyter"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../../common.sh

# jupyter wrapper
cat << 'EOF' > jupyter_wrapper.sh
#!/bin/bash
MATRIX="$1"
# nbconvert execute and output to markdown, which prints stdout of cells
# Use environment variable to pass the matrix file
export MATRIX_FILE="$MATRIX"
jupyter nbconvert --execute --to markdown --stdout Sudoku.ipynb 2>/dev/null
EOF
chmod +x jupyter_wrapper.sh

SOLVER_BINARY="./jupyter_wrapper.sh"

compile() {
    check_toolchain jupyter
}

# Execute benchmarks
main "$@"
