# Sudoku Solver in PowerShell
# Brute-force backtracking algorithm matching C reference

$global:puzzle = New-Object 'int[,]' 9,9
$global:iterations = 0

function Print-Puzzle {
    Write-Host ""
    Write-Host "Puzzle:"
    for ($r = 0; $r -lt 9; $r++) {
        $row = ""
        for ($c = 0; $c -lt 9; $c++) {
            $row += "$($global:puzzle[$r,$c]) "
        }
        Write-Host $row
    }
}

function Is-Valid([int]$row, [int]$col, [int]$val) {
    # Check row
    for ($i = 0; $i -lt 9; $i++) {
        if ($global:puzzle[$row,$i] -eq $val) { return $false }
    }

    # Check column
    for ($i = 0; $i -lt 9; $i++) {
        if ($global:puzzle[$i,$col] -eq $val) { return $false }
    }

    # Check 3x3 box
    $boxRow = [int][math]::Floor($row / 3) * 3
    $boxCol = [int][math]::Floor($col / 3) * 3
    for ($i = 0; $i -lt 3; $i++) {
        for ($j = 0; $j -lt 3; $j++) {
            if ($global:puzzle[($boxRow + $i),($boxCol + $j)] -eq $val) { return $false }
        }
    }

    return $true
}

function Solve {
    # Find first empty cell (row-major order)
    $row = -1
    $col = -1

    for ($r = 0; $r -lt 9; $r++) {
        for ($c = 0; $c -lt 9; $c++) {
            if ($global:puzzle[$r,$c] -eq 0) {
                $row = $r
                $col = $c
                break
            }
        }
        if ($row -ne -1) { break }
    }

    # If no empty cell, puzzle is solved
    if ($row -eq -1) {
        Print-Puzzle
        Write-Host ""
        Write-Host "Solved in Iterations=$($global:iterations)"
        Write-Host ""
        return $true
    }

    # Try values 1-9 in order
    for ($val = 1; $val -le 9; $val++) {
        $global:iterations++

        if (Is-Valid $row $col $val) {
            $global:puzzle[$row,$col] = $val

            if (Solve) { return $true }

            $global:puzzle[$row,$col] = 0
        }
    }

    return $false
}

function Read-Matrix([string]$filename) {
    Write-Host $filename

    $lines = Get-Content $filename
    $row = 0

    foreach ($line in $lines) {
        if ($row -ge 9) { break }
        if ($line -match "^\s*#" -or $line -match "^\s*$") { continue }

        $nums = @()
        foreach ($char in $line.ToCharArray()) {
            if ($char -match '\d') {
                $nums += [int]::Parse($char)
            }
        }

        if ($nums.Count -ge 9) {
            $rowOutput = ""
            for ($c = 0; $c -lt 9; $c++) {
                $global:puzzle[$row,$c] = $nums[$c]
                $rowOutput += "$($nums[$c]) "
            }
            Write-Host $rowOutput
            $row++
        }
    }
}

# Main
if ($args.Count -eq 0) {
    Write-Host "Usage: pwsh Sudoku.ps1 <matrix_file>"
    exit 1
}

$filename = $args[0]

Read-Matrix $filename
Print-Puzzle

$global:iterations = 0
$null = Solve

Write-Host "Seconds to process 0.000"
