#!/bin/bash
# Languages/Jupyter/runMe.sh

LANGUAGE="Jupyter"

# Source shared functions
source ../common.sh

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
    # Check if jupyter is available
    if ! command -v jupyter &> /dev/null; then
        report_env_error "jupyter not found"
    fi
}

# Execute benchmarks
main "$@"
