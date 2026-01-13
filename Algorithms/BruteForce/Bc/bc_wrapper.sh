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
