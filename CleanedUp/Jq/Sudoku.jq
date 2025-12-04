# Sudoku Solver in Jq

# Input: A string of 81 digits (0 for empty)
# Output: Solved string

def to_board: split("") | map(tonumber);

def get_row($r): .[$r*9 : $r*9+9];
def get_col($c): [range(9) | . * 9 + $c] as $idxs | [.[$idxs[]]];
def get_box($r; $c): 
  ($r - ($r % 3)) as $sr | ($c - ($c % 3)) as $sc |
  [range(3) | . as $i | range(3) | . as $j | ($sr+$i)*9 + ($sc+$j)] as $idxs |
  [.[$idxs[]]];

def is_valid($board; $idx; $num):
  ($idx / 9 | floor) as $r | ($idx % 9) as $c |
  # Check row
  (any($board | get_row($r)[]; . == $num) | not) and
  # Check col
  (any($board | get_col($c)[]; . == $num) | not) and
  # Check box
  (any($board | get_box($r; $c)[]; . == $num) | not);

def solve($idx; $iterations):
  if $idx == 81 then
    {solved: true, board: ., iterations: $iterations}
  elif .[$idx] != 0 then
    solve($idx + 1; $iterations)
  else
    # Try 1-9
    reduce range(1; 10) as $num (
      {solved: false, board: ., iterations: $iterations};
      if .solved then .
      else
        if is_valid(.board; $idx; $num) then
          (.board | .[$idx] = $num) as $new_board |
          ($new_board | solve($idx + 1; .iterations + 1))
        else
          .
        end
      end
    )
  end;

# Main
split("") | map(tonumber) | solve(0; 0) | 
if .solved then
  "Solved in Iterations= \(.iterations)",
  (.board | _nwise(9) | join(" "))
else
  "No solution found."
end
