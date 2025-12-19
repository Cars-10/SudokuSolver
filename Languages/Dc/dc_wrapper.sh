#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Generate dc board initialization
DC_INIT=""
for (( i=0; i<${#DIGITS}; i++ )); do
    VAL=${DIGITS:$i:1}
    DC_INIT+="$VAL $i :b "
done

# 4. Main logic execution
(
echo "$DC_INIT"
# STRIP ALL COMMENTS AND EMPTY LINES FROM Sudoku.dc
grep -v '^#' Sudoku.dc | grep -v '^$' | sed 's/ #.*//'
cat << 'EOM'
[Puzzle:]P 10P
lBx
10P

lsx 1 = [
  [Puzzle:]P 10P
  lBx
  10P
  [Solved in Iterations=]P li n 10P
] [
  [No solution found.]P 10P
] ifelse
q
EOM
) | dc
