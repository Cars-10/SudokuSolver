#!/bin/bash
# Languages/PostScript/runMe.sh

LANGUAGE="PostScript"

# Source shared functions
source ../common.sh

# Create a wrapper script to handle Ghostscript's argument order
cat << 'EOF' > ps_wrapper.sh
#!/bin/bash
MATRIX="$1"
# Use -dNOSAFER to allow reading matrix files
gs -dNODISPLAY -q -dNOSAFER -sInputFile="$MATRIX" Sudoku.ps
EOF
chmod +x ps_wrapper.sh

SOLVER_BINARY="./ps_wrapper.sh"

compile() {
    if ! command -v gs &> /dev/null; then
        report_env_error "Ghostscript (gs) not found"
    fi
}

# Execute benchmarks
main "$@"
