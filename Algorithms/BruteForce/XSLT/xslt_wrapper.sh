#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Print initial board
echo "Puzzle:"
echo "$DIGITS" | sed 's/./& /g' | sed 's/.
{18}/&\n/g'
echo ""

# 4. Run XSLT
xsltproc --maxdepth 10000 --stringparam board "$DIGITS" Sudoku.xslt dummy.xml
