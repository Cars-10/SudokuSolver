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
