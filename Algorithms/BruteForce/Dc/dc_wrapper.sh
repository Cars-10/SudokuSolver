#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Build dc input
(
# Fill board 'b'
for (( i=0; i<${#DIGITS}; i++ )); do
    echo "${DIGITS:$i:1} $i :b"
done

cat << 'EOM'
# Registers:
# b: Board (0-80)
# E: Empty Indices (0..N-1)
# N: Count of empty
# C: Iterations
# k: Depth (index in E)
# I: Current Board Index
# V: Current Value

0 sC # Count = 0

# --- Populate Empty List (E) ---
0 sN
0 si
[
  li ;b 0 =A
  li 1 + d si 81 >L
] dsL x
[
  li lN :E
  lN 1 + sN
] sA

# --- IsValid Macro (M) ---
# Stack: val idx -> 0/1
# Preserves I/V by using temp registers
[
  St Si # Save args to t, i
  lt si
  lt sv # v = val, i = idx
  
  1 sO # Result
  
  # Row Check
  li 9 / 9 * sr # r = start of row
  0 sc          # counter
  [
    lr lc + ;b lv =F
    lc 1 + d sc 9 >1
  ] ds1 x
  
  # Col Check
  li 9 % sc     # c = start of col
  0 sr
  [
    lr 9 * sc + ;b lv =F
    lr 1 + d sr 9 >2
  ] ds2 x
  
  # Box Check
  li 27 / 27 * sr      # row offset
  li 9 % 3 / 3 * sc    # col offset
  sr sc + sB           # B = box start
  0 sr
  [
    0 sc
    [
      lB lr 9 * + lc + ;b lv =F
      lc 1 + d sc 3 >4
    ] ds4 x
    lr 1 + d sr 3 >3
  ] ds3 x
  
  lO # Return result
  Lt Li # Restore stack
  
  # Fail sub-macro
  [0 sO] sF
] sM

# --- Go (G) ---
# Recursive step
# Needs to preserve I and V across recursion
[
  SI SV        # Save I, V on stack
  lV lI :b     # Place V
  lk 1 + sk    # k++
  lS x         # Recurse
  lk 1 - sk    # k-- (restore k)
  0 lI :b      # Reset board (backtrack)
  LV LI        # Restore V, I
] sG

# --- Backtrack (B) ---
# Called when loop exhausted
# Quits the current X loop
[
  lV Q
] sB

# --- Solver (S) ---
[
  # Check if Solved
  lk lN =Z
  
  # Get Index I
  lk ;E sI
  
  # Loop 1..9
  # We use a recursive loop X to iterate V
  1 sV
  [
    lV 9 >B # If V > 9, quit loop (backtrack)
    
    # Count++
    lC 1 + sC
    
    # Check Valid
    lV lI lM x 1 =G
    
    lV 1 + sV
    lX x
  ] dsX x
  
] dsS x

# --- Solved (Z) ---
[
  [Puzzle:] p
  # Print Board
  0 si
  [
    li ;b n [ ] P
    li 1 + 9 % 0 =Y
    li 1 + d si 81 >P
  ] dsP x
  [
] P
  [Solved in Iterations=] n lC p
  [
] P
  9999 Q # Quit everything
] sZ

[10 P] sY

# Start
lS x
[No solution found.] p
EOM
) | dc
