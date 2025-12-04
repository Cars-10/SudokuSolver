#!/bin/bash
# runMeGlobal.sh
# Usage: ./runMeGlobal.sh [Language] [MatrixSpec]
# Example: ./runMeGlobal.sh C "1,1-3"
# Example: ./runMeGlobal.sh Python "all"

cd "$(dirname "$0")"

LANGUAGE=$1
MATRIX_SPEC=$2

if [ -z "$LANGUAGE" ] || [ -z "$MATRIX_SPEC" ]; then
    echo "Usage: $0 [Language] [MatrixSpec]"
    exit 1
fi

# Resolve Matrix Spec to File Paths
MATRIX_DIR="../../Matrices"
MATRIX_FILES=""

if [ "$MATRIX_SPEC" == "all" ]; then
    MATRIX_FILES="$MATRIX_DIR/*.matrix"
else
    # Split by comma
    IFS=',' read -ra ADDR <<< "$MATRIX_SPEC"
    for i in "${ADDR[@]}"; do
        # Handle ranges (e.g., 1-3)
        if [[ "$i" == *-* ]]; then
            start=$(echo $i | cut -d'-' -f1)
            end=$(echo $i | cut -d'-' -f2)
            for ((j=start; j<=end; j++)); do
                MATRIX_FILES="$MATRIX_FILES $MATRIX_DIR/$j.matrix"
            done
        else
            # Single number
            MATRIX_FILES="$MATRIX_FILES $MATRIX_DIR/$i.matrix"
        fi
    done
fi

# Check if language directory exists
LANG_DIR="$LANGUAGE"
if [ ! -d "$LANG_DIR" ]; then
    echo "Error: Language directory '$LANG_DIR' not found."
    exit 1
fi

# Check if setupAndRunMe.sh exists
SCRIPT="$LANG_DIR/setupAndRunMe.sh"
if [ ! -f "$SCRIPT" ]; then
    echo "Error: $SCRIPT not found."
    exit 1
fi

# Execute the language script
# We pass the resolved matrix files as arguments
"$SCRIPT" $MATRIX_FILES
