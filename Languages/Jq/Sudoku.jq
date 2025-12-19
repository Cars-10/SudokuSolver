# Sudoku Solver in Jq
# Exact match of C brute-force algorithm

def isValid($grid; $r; $c; $val):
  # Row check
  all(range(9); $grid[$r * 9 + .] != $val) and
  # Col check
  all(range(9); $grid[. * 9 + $c] != $val) and
  # Box check
  ((($r / 3) | floor) * 3) as $br |
  ((($c / 3) | floor) * 3) as $bc |
  all(range(3); . as $i | all(range(3); . as $j | $grid[($br + $i) * 9 + ($bc + $j)] != $val));

def findEmpty($grid):
  label $out | (range(81) | if $grid[.] == 0 then ., break $out else empty end) // null;

def solve:
  findEmpty(.grid) as $idx |
  if ($idx == null) then
    . + {solved: true}
  else
    (($idx / 9) | floor) as $r | ($idx % 9) as $c |
    reduce range(1; 10) as $v (.;
      if .solved then .
      else
        .count += 1 |
        if isValid(.grid; $r; $c; $v) then
          .grid[$idx] = $v | solve |
          if .solved then . else .grid[$idx] = 0 end
        else .
        end
      end
    )
  end;

def formatBoard($grid):
  [range(9) | . as $r |
    [range(9) | . as $c | $grid[$r * 9 + $c] | tostring + " "] | join("")
  ] | join("\n");

# Main
select(length >= 81) | 
split("") | map(tonumber) as $initial |
{grid: $initial, count: 0, solved: false} | solve |
if .solved then
  "Puzzle:",
  formatBoard(.grid),
  "",
  "Solved in Iterations=\(.count)"
else
  "No solution found."
end