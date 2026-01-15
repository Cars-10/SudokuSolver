# Constraint Propagation Sudoku Solver in PowerShell
# Implements CP algorithm with bitsets and constraint propagation

$script:iterations = 0

# Helper functions for bitset operations
function Has-Bit {
    param([uint16]$set, [int]$digit)
    return ($set -band (1 -shl $digit)) -ne 0
}

function Remove-Bit {
    param([uint16]$set, [int]$digit)
    return $set -band (-bnot (1 -shl $digit))
}

function Count-Bits {
    param([uint16]$set)
    $count = 0
    for ($i = 1; $i -le 9; $i++) {
        if (Has-Bit $set $i) { $count++ }
    }
    return $count
}

function Get-FirstBit {
    param([uint16]$set)
    for ($i = 1; $i -le 9; $i++) {
        if (Has-Bit $set $i) { return $i }
    }
    return 0
}

# Get peers for a cell (20 cells in same row, column, or box)
function Get-Peers {
    param([int]$row, [int]$col)

    $peers = @()

    # Same row
    for ($c = 0; $c -lt 9; $c++) {
        if ($c -ne $col) {
            $peers += @{r=$row; c=$c}
        }
    }

    # Same column
    for ($r = 0; $r -lt 9; $r++) {
        if ($r -ne $row) {
            $peers += @{r=$r; c=$col}
        }
    }

    # Same 3x3 box (excluding cells already added in same row/col)
    $boxRow = [int]($row / 3) * 3
    $boxCol = [int]($col / 3) * 3
    for ($r = $boxRow; $r -lt ($boxRow + 3); $r++) {
        for ($c = $boxCol; $c -lt ($boxCol + 3); $c++) {
            # Exclude the cell itself, and cells already counted in row/col
            if (-not (($r -eq $row) -or ($c -eq $col))) {
                $peers += @{r=$r; c=$c}
            }
        }
    }

    return $peers
}

# Eliminate a digit from a cell's candidates
function Eliminate {
    param(
        [ref]$grid_values,
        [ref]$grid_candidates,
        [int]$row,
        [int]$col,
        [int]$digit
    )

    # If cell is already assigned, don't eliminate
    if ($grid_values.Value[$row,$col] -ne 0) {
        return $true
    }

    # Check if already eliminated
    if (-not (Has-Bit $grid_candidates.Value[$row,$col] $digit)) {
        return $true
    }

    # Remove digit from candidates
    $grid_candidates.Value[$row,$col] = Remove-Bit $grid_candidates.Value[$row,$col] $digit

    # Check for contradiction
    $remaining = Count-Bits $grid_candidates.Value[$row,$col]
    if ($remaining -eq 0) {
        return $false
    }

    # If only one candidate left, assign it
    if ($remaining -eq 1) {
        $last_digit = Get-FirstBit $grid_candidates.Value[$row,$col]
        return Assign -grid_values $grid_values -grid_candidates $grid_candidates -row $row -col $col -digit $last_digit
    }

    return $true
}

# Assign a digit to a cell
function Assign {
    param(
        [ref]$grid_values,
        [ref]$grid_candidates,
        [int]$row,
        [int]$col,
        [int]$digit
    )

    # Increment iteration counter (this is our benchmark metric)
    $script:iterations++

    # Set value
    $grid_values.Value[$row,$col] = $digit
    $grid_candidates.Value[$row,$col] = [uint16](1 -shl $digit)

    # Eliminate digit from all peers
    $peers = Get-Peers $row $col
    foreach ($peer in $peers) {
        if (-not (Eliminate -grid_values $grid_values -grid_candidates $grid_candidates -row $peer.r -col $peer.c -digit $digit)) {
            return $false
        }
    }

    return $true
}

# Propagate constraints (singleton elimination and hidden singles)
function Propagate {
    param(
        [ref]$grid_values,
        [ref]$grid_candidates
    )

    # First pass: eliminate clue values from their peers
    # This is necessary because init sets clues directly without propagation
    for ($r = 0; $r -lt 9; $r++) {
        for ($c = 0; $c -lt 9; $c++) {
            if ($grid_values.Value[$r,$c] -ne 0) {
                $digit = $grid_values.Value[$r,$c]
                $peers = Get-Peers $r $c
                foreach ($peer in $peers) {
                    $pr = $peer.r
                    $pc = $peer.c
                    # Only eliminate from unassigned cells
                    if ($grid_values.Value[$pr,$pc] -eq 0) {
                        $grid_candidates.Value[$pr,$pc] = Remove-Bit $grid_candidates.Value[$pr,$pc] $digit
                        # Check for contradiction
                        if ((Count-Bits $grid_candidates.Value[$pr,$pc]) -eq 0) {
                            return $false
                        }
                    }
                }
            }
        }
    }

    $changed = $true
    while ($changed) {
        $changed = $false

        # Singleton elimination
        for ($r = 0; $r -lt 9; $r++) {
            for ($c = 0; $c -lt 9; $c++) {
                if ($grid_values.Value[$r,$c] -eq 0) {
                    $num_candidates = Count-Bits $grid_candidates.Value[$r,$c]
                    if ($num_candidates -eq 0) {
                        return $false
                    }
                    if ($num_candidates -eq 1) {
                        $digit = Get-FirstBit $grid_candidates.Value[$r,$c]
                        if (-not (Assign -grid_values $grid_values -grid_candidates $grid_candidates -row $r -col $c -digit $digit)) {
                            return $false
                        }
                        $changed = $true
                    }
                }
            }
        }

        # Hidden singles - rows
        for ($r = 0; $r -lt 9; $r++) {
            for ($d = 1; $d -le 9; $d++) {
                $count = 0
                $last_c = -1
                $already_assigned = $false

                for ($c = 0; $c -lt 9; $c++) {
                    if ($grid_values.Value[$r,$c] -eq $d) {
                        $already_assigned = $true
                        break
                    }
                    if (Has-Bit $grid_candidates.Value[$r,$c] $d) {
                        $count++
                        $last_c = $c
                    }
                }

                if (-not $already_assigned) {
                    if ($count -eq 0) {
                        return $false
                    }
                    if ($count -eq 1) {
                        if (-not (Assign -grid_values $grid_values -grid_candidates $grid_candidates -row $r -col $last_c -digit $d)) {
                            return $false
                        }
                        $changed = $true
                    }
                }
            }
        }

        # Hidden singles - columns
        for ($c = 0; $c -lt 9; $c++) {
            for ($d = 1; $d -le 9; $d++) {
                $count = 0
                $last_r = -1
                $already_assigned = $false

                for ($r = 0; $r -lt 9; $r++) {
                    if ($grid_values.Value[$r,$c] -eq $d) {
                        $already_assigned = $true
                        break
                    }
                    if (Has-Bit $grid_candidates.Value[$r,$c] $d) {
                        $count++
                        $last_r = $r
                    }
                }

                if (-not $already_assigned) {
                    if ($count -eq 0) {
                        return $false
                    }
                    if ($count -eq 1) {
                        if (-not (Assign -grid_values $grid_values -grid_candidates $grid_candidates -row $last_r -col $c -digit $d)) {
                            return $false
                        }
                        $changed = $true
                    }
                }
            }
        }

        # Hidden singles - boxes
        for ($box = 0; $box -lt 9; $box++) {
            $boxRow = [int]($box / 3) * 3
            $boxCol = ($box % 3) * 3

            for ($d = 1; $d -le 9; $d++) {
                $count = 0
                $last_r = -1
                $last_c = -1
                $already_assigned = $false

                for ($r = $boxRow; $r -lt ($boxRow + 3); $r++) {
                    for ($c = $boxCol; $c -lt ($boxCol + 3); $c++) {
                        if ($grid_values.Value[$r,$c] -eq $d) {
                            $already_assigned = $true
                            break
                        }
                        if (Has-Bit $grid_candidates.Value[$r,$c] $d) {
                            $count++
                            $last_r = $r
                            $last_c = $c
                        }
                    }
                    if ($already_assigned) { break }
                }

                if (-not $already_assigned) {
                    if ($count -eq 0) {
                        return $false
                    }
                    if ($count -eq 1) {
                        if (-not (Assign -grid_values $grid_values -grid_candidates $grid_candidates -row $last_r -col $last_c -digit $d)) {
                            return $false
                        }
                        $changed = $true
                    }
                }
            }
        }
    }

    return $true
}

# Find cell with minimum remaining values (MRV heuristic)
function Find-MRVCell {
    param(
        [ref]$grid_values,
        [ref]$grid_candidates
    )

    $min_candidates = 10
    $best_r = -1
    $best_c = -1

    for ($r = 0; $r -lt 9; $r++) {
        for ($c = 0; $c -lt 9; $c++) {
            if ($grid_values.Value[$r,$c] -eq 0) {
                $num_candidates = Count-Bits $grid_candidates.Value[$r,$c]
                if ($num_candidates -lt $min_candidates) {
                    $min_candidates = $num_candidates
                    $best_r = $r
                    $best_c = $c
                }
            }
        }
    }

    if ($best_r -eq -1) {
        return $null  # No empty cells
    }

    return @{r=$best_r; c=$best_c}
}

# Search with backtracking
function Search-CP {
    param(
        [ref]$grid_values,
        [ref]$grid_candidates
    )

    # Find cell with minimum remaining values
    $mrv = Find-MRVCell -grid_values $grid_values -grid_candidates $grid_candidates

    if ($null -eq $mrv) {
        # No empty cells - solved!
        return $true
    }

    $r = $mrv.r
    $c = $mrv.c
    $candidates = $grid_candidates.Value[$r,$c]

    # Try each candidate
    for ($d = 1; $d -le 9; $d++) {
        if (Has-Bit $candidates $d) {
            # Save state for backtracking
            $saved_values = $grid_values.Value.Clone()
            $saved_candidates = $grid_candidates.Value.Clone()

            # Try assigning this digit
            if (Assign -grid_values $grid_values -grid_candidates $grid_candidates -row $r -col $c -digit $d) {
                # Propagate constraints
                if (Propagate -grid_values $grid_values -grid_candidates $grid_candidates) {
                    # Recurse
                    if (Search-CP -grid_values $grid_values -grid_candidates $grid_candidates) {
                        return $true
                    }
                }
            }

            # Backtrack
            $grid_values.Value = $saved_values
            $grid_candidates.Value = $saved_candidates
        }
    }

    return $false
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
    Write-Output "Usage: pwsh cp.ps1 <matrix_file>"
    exit 1
}

$filename = $args[0]
$startTime = Get-Date

# Read puzzle
$puzzle = Read-MatrixFile $filename

# Print initial puzzle
Print-Puzzle $puzzle

# Initialize grid with all candidates
$grid_values = New-Object 'int[,]' 9, 9
$grid_candidates = New-Object 'uint16[,]' 9, 9

for ($r = 0; $r -lt 9; $r++) {
    for ($c = 0; $c -lt 9; $c++) {
        $grid_values[$r,$c] = 0
        $grid_candidates[$r,$c] = [uint16]0x3FE  # Bits 1-9 set
    }
}

# Initialize clues directly WITHOUT calling Assign (following C reference pattern)
# This sets values and candidates without triggering constraint propagation
for ($r = 0; $r -lt 9; $r++) {
    for ($c = 0; $c -lt 9; $c++) {
        if ($puzzle[$r,$c] -ne 0) {
            $digit = $puzzle[$r,$c]
            $grid_values[$r,$c] = $digit
            $grid_candidates[$r,$c] = [uint16](1 -shl $digit)
        }
    }
}

# Reset iteration counter before propagation (C reference starts at 0)
$script:iterations = 0

# Initial propagation - this handles constraint elimination properly
if (-not (Propagate -grid_values ([ref]$grid_values) -grid_candidates ([ref]$grid_candidates))) {
    Write-Host ""
    Write-Host "No solution found (contradiction during initial propagation)"
    exit 0
}

# Solve
$solved = Search-CP -grid_values ([ref]$grid_values) -grid_candidates ([ref]$grid_candidates)

if ($solved) {
    # Print solution
    Print-Puzzle $grid_values

    Write-Host ""
    Write-Host "Solved in Iterations=$($script:iterations)"
    Write-Host ""
} else {
    Write-Host ""
    Write-Host "No solution found"
    Write-Host ""
}

$endTime = Get-Date
$elapsed = ($endTime - $startTime).TotalSeconds
Write-Host "Seconds to process $($elapsed.ToString('F3'))"

