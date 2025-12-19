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
