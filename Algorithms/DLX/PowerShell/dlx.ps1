# DLX (Dancing Links) Algorithm X Sudoku Solver in PowerShell
# Implements Knuth's Algorithm X with Dancing Links data structure

$script:dlx_iterations = 0

# Node class for Dancing Links
class DlxNode {
    [DlxNode]$up
    [DlxNode]$down
    [DlxNode]$left
    [DlxNode]$right
    [DlxColumn]$column
    [int]$row_id

    DlxNode() {
        $this.up = $this
        $this.down = $this
        $this.left = $this
        $this.right = $this
        $this.column = $null
        $this.row_id = -1
    }
}

# Column header class (inherits from DlxNode)
class DlxColumn : DlxNode {
    [int]$size
    [string]$name

    DlxColumn([string]$colName) : base() {
        $this.size = 0
        $this.name = $colName
        $this.column = $this
    }
}

# Cover a column (remove from matrix)
function Cover-Column {
    param([DlxColumn]$col)

    # Remove column header from header list
    $col.right.left = $col.left
    $col.left.right = $col.right

    # For each row in this column
    $row = $col.down
    while ($row -ne $col) {
        # For each node in this row
        $node = $row.right
        while ($node -ne $row) {
            # Remove node from its column
            $node.down.up = $node.up
            $node.up.down = $node.down
            $node.column.size--
            $node = $node.right
        }
        $row = $row.down
    }
}

# Uncover a column (restore to matrix)
function Uncover-Column {
    param([DlxColumn]$col)

    # For each row in this column (reverse order)
    $row = $col.up
    while ($row -ne $col) {
        # For each node in this row (reverse order)
        $node = $row.left
        while ($node -ne $row) {
            # Restore node to its column
            $node.column.size++
            $node.down.up = $node
            $node.up.down = $node
            $node = $node.left
        }
        $row = $row.up
    }

    # Restore column header to header list
    $col.right.left = $col
    $col.left.right = $col
}

# Choose column with minimum size (Knuth's S heuristic)
function Choose-Column {
    param([DlxColumn]$root)

    $best = $null
    $min_size = [int]::MaxValue

    $col = $root.right
    while ($col -ne $root) {
        if ($null -eq $col.size) {
            Write-Host "ERROR: Column has no size property: $($col.GetType().Name)"
            break
        }
        if ($col.size -lt $min_size) {
            $min_size = $col.size
            $best = $col
        }
        $col = $col.right
    }

    if ($null -ne $best -and $script:dlx_iterations -le 3) {
        Write-Host "DEBUG: Chose column $($best.name) with size $($best.size)"
    }

    return $best
}

# DLX Search - Algorithm X with Dancing Links
function Search-DLX {
    param(
        [DlxColumn]$root,
        [System.Collections.ArrayList]$solution
    )

    $script:dlx_iterations++

    # If matrix is empty, we found a solution
    if ($root.right -eq $root) {
        Write-Host "DEBUG: Found solution at iteration $($script:dlx_iterations)"
        return $true
    }

    # Choose column with minimum size
    $col = Choose-Column $root

    if ($null -eq $col) {
        Write-Host "DEBUG: Choose-Column returned null"
        return $false
    }

    # If column has no rows, no solution possible
    if ($col.size -eq 0) {
        return $false
    }

    # Cover this column
    Cover-Column $col

    # Try each row in this column
    $row = $col.down
    $row_count = 0
    while ($row -ne $col) {
        $row_count++
        # Add row to partial solution
        $null = $solution.Add($row.row_id)

        # Cover all other columns in this row
        $node = $row.right
        while ($node -ne $row) {
            Cover-Column $node.column
            $node = $node.right
        }

        # Recurse
        if (Search-DLX $root $solution) {
            return $true
        }

        # Backtrack: uncover all columns in this row
        $node = $row.left
        while ($node -ne $row) {
            Uncover-Column $node.column
            $node = $node.left
        }

        # Remove row from partial solution
        $solution.RemoveAt($solution.Count - 1)

        $row = $row.down
    }

    if ($script:dlx_iterations -le 5) {
        Write-Host "DEBUG: Tried $row_count rows in column $($col.name), all failed"
    }

    # Uncover column
    Uncover-Column $col

    return $false
}

# Build DLX matrix for Sudoku
function Build-DLXMatrix {
    param([int[,]]$puzzle)

    # Create root column
    $root = [DlxColumn]::new("root")

    # Create 324 column headers (4 constraint types)
    # Constraint 0-80: Cell[r,c] has a value
    # Constraint 81-161: Row[r] has digit[d]
    # Constraint 162-242: Column[c] has digit[d]
    # Constraint 243-323: Box[b] has digit[d]
    $columns = @()
    for ($i = 0; $i -lt 324; $i++) {
        $col = [DlxColumn]::new("C$i")
        $columns += $col

        # Link into header list
        $col.left = $root.left
        $col.right = $root
        $root.left.right = $col
        $root.left = $col
    }

    # Store row info and row starts for solution decoding
    $row_info = @()
    $row_starts = @{}

    # Create row nodes for each possible placement
    # Build ALL rows (including clues), then cover clues separately
    $row_id = 0
    for ($r = 0; $r -lt 9; $r++) {
        for ($c = 0; $c -lt 9; $c++) {
            $box = [int]($r / 3) * 3 + [int]($c / 3)

            # If cell has a clue, create only that row
            # Otherwise create rows for all digits 1-9
            $start_digit = 1
            $end_digit = 9
            if ($puzzle[$r, $c] -ne 0) {
                $start_digit = $puzzle[$r, $c]
                $end_digit = $puzzle[$r, $c]
            }

            for ($d = $start_digit; $d -le $end_digit; $d++) {
                # Store row info for solution decoding
                $row_info += @{
                    row = $r
                    col = $c
                    num = $d
                }

                # Calculate constraint indices
                $c1 = $r * 9 + $c                    # Cell constraint
                $c2 = 81 + $r * 9 + ($d - 1)         # Row constraint
                $c3 = 162 + $c * 9 + ($d - 1)        # Column constraint
                $c4 = 243 + $box * 9 + ($d - 1)      # Box constraint

                # Create 4 nodes for this row
                $nodes = @()
                foreach ($cidx in @($c1, $c2, $c3, $c4)) {
                    $node = [DlxNode]::new()
                    $node.row_id = $row_id
                    $node.column = $columns[$cidx]
                    $nodes += $node

                    # Link into column
                    $node.up = $columns[$cidx].up
                    $node.down = $columns[$cidx]
                    $columns[$cidx].up.down = $node
                    $columns[$cidx].up = $node
                    $columns[$cidx].size++
                }

                # Link nodes horizontally
                for ($i = 0; $i -lt 4; $i++) {
                    $nodes[$i].left = $nodes[($i + 3) % 4]
                    $nodes[$i].right = $nodes[($i + 1) % 4]
                }

                # Store first node of this row
                $row_starts[$row_id] = $nodes[0]

                $row_id++
            }
        }
    }

    return @{
        root = $root
        columns = $columns
        row_info = $row_info
        row_starts = $row_starts
    }
}

# Cover clues (pre-filled cells)
function Cover-Clues {
    param(
        [int[,]]$puzzle,
        [hashtable]$dlx
    )

    $clues_covered = 0
    for ($r = 0; $r -lt 9; $r++) {
        for ($c = 0; $c -lt 9; $c++) {
            if ($puzzle[$r, $c] -ne 0) {
                $n = $puzzle[$r, $c]

                # Find the row for this clue
                for ($row_id = 0; $row_id -lt $dlx.row_info.Count; $row_id++) {
                    $info = $dlx.row_info[$row_id]
                    if ($info.row -eq $r -and $info.col -eq $c -and $info.num -eq $n) {
                        # Cover all columns in this row
                        $node = $dlx.row_starts[$row_id]
                        if ($null -eq $node) {
                            Write-Host "ERROR: null node for row_id $row_id (r=$r,c=$c,n=$n)"
                            continue
                        }
                        $curr = $node
                        do {
                            Cover-Column $curr.column
                            $curr = $curr.right
                        } while ($curr -ne $node)
                        $clues_covered++
                        break
                    }
                }
            }
        }
    }
    Write-Host "DEBUG: Covered $clues_covered clues"
}

# Decode solution from row IDs
function Decode-Solution {
    param(
        [System.Collections.ArrayList]$solution,
        [array]$row_info,
        [int[,]]$puzzle
    )

    # Start with the original puzzle (includes clues)
    $grid = New-Object 'int[,]' 9, 9
    for ($r = 0; $r -lt 9; $r++) {
        for ($c = 0; $c -lt 9; $c++) {
            $grid[$r, $c] = $puzzle[$r, $c]
        }
    }

    # Add solution values
    foreach ($row_id in $solution) {
        if ($row_id -ge 0 -and $row_id -lt $row_info.Count) {
            $info = $row_info[$row_id]
            $grid[$info.row, $info.col] = $info.num
        }
    }

    return $grid
}

# Print puzzle
function Print-Puzzle {
    param([int[,]]$grid)

    Write-Host ""
    Write-Host "Puzzle:"
    for ($r = 0; $r -lt 9; $r++) {
        $line = ""
        for ($c = 0; $c -lt 9; $c++) {
            $line += "$($grid[$r,$c]) "
        }
        Write-Host $line.TrimEnd()
    }
}

# Read matrix file
function Read-MatrixFile {
    param([string]$filename)

    Write-Host $filename

    $lines = Get-Content $filename
    $grid = New-Object 'int[,]' 9, 9
    $row = 0

    foreach ($line in $lines) {
        if ($row -ge 9) { break }
        if ($line -match '^\s*#' -or $line -match '^\s*$') { continue }

        $nums = @()
        foreach ($char in $line.ToCharArray()) {
            if ($char -match '\d') {
                [void]($nums += [int]::Parse($char))
            }
        }

        if ($nums.Count -ge 9) {
            $rowOutput = ""
            for ($c = 0; $c -lt 9; $c++) {
                $grid[$row, $c] = $nums[$c]
                $rowOutput += "$($nums[$c]) "
            }
            Write-Host $rowOutput.TrimEnd()
            $row++
        }
    }

    return , $grid
}

# Main
if ($args.Count -eq 0) {
    Write-Output "Usage: pwsh dlx.ps1 <matrix_file>"
    exit 1
}

$filename = $args[0]
$startTime = Get-Date

# Read puzzle
$puzzle = Read-MatrixFile $filename

# Print initial puzzle
Print-Puzzle $puzzle

# Build DLX matrix
$dlx = Build-DLXMatrix $puzzle

# Cover pre-filled clues
Cover-Clues $puzzle $dlx

# Debug: count remaining columns
$col_count = 0
$col = $dlx.root.right
while ($col -ne $dlx.root) {
    $col_count++
    $col = $col.right
}
Write-Host "DEBUG: Remaining columns after covering clues: $col_count"

# Solve
$script:dlx_iterations = 0
$solution = New-Object System.Collections.ArrayList
$solved = Search-DLX $dlx.root $solution

if ($solved) {
    # Decode and print solution
    $solved_grid = Decode-Solution $solution $dlx.row_info $puzzle
    Print-Puzzle $solved_grid

    Write-Output ""
    Write-Output "Solved in Iterations=$($script:dlx_iterations)"
    Write-Output ""
} else {
    Write-Output ""
    Write-Output "No solution found"
    Write-Output ""
}

$endTime = Get-Date
$elapsed = ($endTime - $startTime).TotalSeconds
Write-Output "Seconds to process $($elapsed.ToString('F3'))"
