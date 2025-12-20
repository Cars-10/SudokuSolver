#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Build dc input
(
for (( i=0; i<${#DIGITS}; i++ )); do
    echo "${DIGITS:$i:1} $i :b"
done

cat << 'EOM'
[Puzzle:]P 10P
0sk [ lk ;b n [ ]P lk 9 % 8 = [10P]sN lNx lk 1 + sk lk 81 >P ] dsPx
10P
q
EOM
) | dc
