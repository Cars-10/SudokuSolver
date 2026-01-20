#!/bin/bash
MATRIX_FILE="$1"

# Convert matrix file to matrix_data.mk format
{
    echo "# Auto-generated from $MATRIX_FILE"
    row=0
    while IFS= read -r line || [ -n "$line" ]; do
        # Skip comments and empty lines
        [[ "$line" =~ ^# ]] && continue
        [[ -z "$line" ]] && continue

        echo "DEBUG: Parsing Row $row: $line" >&2

        col=0
        for val in $line; do
            echo "cell_${row}_${col} := $val"
            col=$((col + 1))
        done
        row=$((row + 1))
        [ $row -ge 9 ] && break
    done < "$MATRIX_FILE"
} > matrix_data.mk

# Run the solver
make -s solve INPUT="$MATRIX_FILE"
