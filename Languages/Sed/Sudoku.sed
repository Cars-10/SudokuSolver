#!/bin/sed -nf

# Sudoku Solver in GNU Sed
# Final attempt at correct backtracking

# 1. Setup
s/0/./g
s/$/|0/
s/^/>/

:main
# Solved?
/>|/ b solved

# Skip fixed
s/>\([1-9]\)/\1>/
t main

# Try next digit at cursor
s/>\./>1/; t check
s/>1/>2/; t check
s/>2/>3/; t check
s/>3/>4/; t check
s/>4/>5/; t check
s/>5/>6/; t check
s/>6/>7/; t check
s/>7/>8/; t check
s/>8/>9/; t check

# 9 failed, backtrack
s/>9/./
:backtrack
s/\(.\)>/>\1/
/^>/ b nosol
s/>\([1-9]\)/>\1/; t backtrack
# Found a guess cell
b main

:check
# 1. Increment iterations
s/|\([0-9]*\)$/|\1:/
:inc
s/9:/:0/; t inc
s/8:/9/; t id
s/7:/8/; t id
s/6:/7/; t id
s/5:/6/; t id
s/4:/5/; t id
s/3:/4/; t id
s/2:/3/; t id
s/1:/2/; t id
s/0:/1/; t id
s/:/1/
:id
s/://g

# 2. Check Validity
h
s/|.*//; s/>//

# Row check
s/.\{9\}/& /g
/\([1-9]\)[^ ]*\1/ b invalid

# Col check
s/ //g
/\([1-9]\).\{8\}\1/ b invalid
/\([1-9]\).\{17\}\1/ b invalid
/\([1-9]\).\{26\}\1/ b invalid
/\([1-9]\).\{35\}\1/ b invalid
/\([1-9]\).\{44\}\1/ b invalid
/\([1-9]\).\{53\}\1/ b invalid
/\([1-9]\).\{62\}\1/ b invalid
/\([1-9]\).\{71\}\1/ b invalid

# Box check
# Band 1
s/^\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\).*/\1\4\7 \2\5\8 \3\6\9/
/\([1-9]\)[^ ]*\1/ b invalid
# Band 2
g; s/|.*//; s/>//
s/^.\{27\}\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\).*/\1\4\7 \2\5\8 \3\6\9/
/\([1-9]\)[^ ]*\1/ b invalid
# Band 3
g; s/|.*//; s/>//
s/^.\{54\}\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\)\(...\).*/\1\4\7 \2\5\8 \3\6\9/
/\([1-9]\)[^ ]*\1/ b invalid

# Valid!
g
s/>\(.\)/\1>/
b main

:invalid
g
# Move cursor back before the digit we just tried
s/\(.\)>/>\1/
b main

:solved
s/>//
s/|.*/ /
p
g
s/.*|//
i\
Solved in Iterations=
p
q

:nosol
i\
No solution found.
q
