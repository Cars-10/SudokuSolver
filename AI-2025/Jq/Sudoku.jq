# Jq Sudoku Solver

# We represent the puzzle as an array of 81 integers.
# Jq is functional, so we pass state.

def print_puzzle:
  . as $p |
  "Puzzle:",
  (range(0;9) | . as $r |
    (range(0;9) | . as $c |
      $p[$r*9 + $c] | tostring
    ) | join(" ")
  );

def is_possible($p; $r; $c; $val):
  # Row
  all(range(0;9); $p[.*9 + $c] != $val) and
  # Col
  all(range(0;9); $p[$r*9 + .] != $val) and
  # Box
  (
    ($r / 3 | floor * 3) as $r0 |
    ($c / 3 | floor * 3) as $c0 |
    all(range(0;3); . as $i |
      all(range(0;3); . as $j |
        $p[($r0+$i)*9 + ($c0+$j)] != $val
      )
    )
  );

def solve($p; $idx; $count):
  if $idx == 81 then
    # Solved
    {$p, $solved: true, $count}
  elif $p[$idx] != 0 then
    solve($p; $idx+1; $count)
  else
    ($idx / 9 | floor) as $r |
    ($idx % 9) as $c |
    
    # Try values 1..9
    reduce range(1;10) as $val (
      {$p, $solved: false, $count};
      if .solved then .
      else
        .count += 1 |
        if is_possible($p; $r; $c; $val) then
          solve($p | .[$idx] = $val; $idx+1; .count)
        else
          .
        end
      end
    )
  end;

# Input is a flat array of numbers
. as $input |
$input |
{p: ., solved: false, count: 0} |
# Find first empty? No, just start at 0
solve(.p; 0; 0) |
if .solved then
  (.p | print_puzzle),
  "\nSolved in Iterations=\(.count)\n"
else
  "No solution found"
end
