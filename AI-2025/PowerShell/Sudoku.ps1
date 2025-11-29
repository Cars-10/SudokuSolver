# PowerShell Sudoku Solver

$puzzle = New-Object 'int[,]' 9,9
$count = 0

function Print-Puzzle {
    Write-Host "`nPuzzle:"
    for ($r = 0; $r -lt 9; $r++) {
        $line = ""
        for ($c = 0; $c -lt 9; $c++) {
            $line += "$($puzzle[$r,$c]) "
        }
        Write-Host $line
    }
}

function Read-MatrixFile {
    param([string]$filename)
    Write-Host $filename
    
    $lines = Get-Content $filename
    $row = 0
    foreach ($line in $lines) {
        if ($line.StartsWith("#") -or [string]::IsNullOrWhiteSpace($line)) { continue }
        
        $parts = $line -split "\s+" | Where-Object { $_ -ne "" }
        if ($parts.Count -eq 9) {
            for ($col = 0; $col -lt 9; $col++) {
                $puzzle[$row, $col] = [int]$parts[$col]
            }
            $row++
            if ($row -eq 9) { break }
        }
    }
}

function Test-Possible {
    param([int]$r, [int]$c, [int]$val)
    
    for ($i = 0; $i -lt 9; $i++) {
        if ($puzzle[$i, $c] -eq $val) { return $false }
        if ($puzzle[$r, $i] -eq $val) { return $false }
    }
    
    $r0 = [math]::Floor($r / 3) * 3
    $c0 = [math]::Floor($c / 3) * 3
    
    for ($i = 0; $i -lt 3; $i++) {
        for ($j = 0; $j -lt 3; $j++) {
            if ($puzzle[$r0 + $i, $c0 + $j] -eq $val) { return $false }
        }
    }
    return $true
}

function Solve-Puzzle {
    for ($r = 0; $r -lt 9; $r++) {
        for ($c = 0; $c -lt 9; $c++) {
            if ($puzzle[$r, $c] -eq 0) {
                for ($val = 1; $val -le 9; $val++) {
                    $script:count++
                    if (Test-Possible $r $c $val) {
                        $puzzle[$r, $c] = $val
                        if (Solve-Puzzle) { return $true }
                        $puzzle[$r, $c] = 0
                    }
                }
                return $false
            }
        }
    }
    Print-Puzzle
    Write-Host "`nSolved in Iterations=$script:count`n"
    return $true
}

$start = Get-Date

foreach ($arg in $args) {
    if ($arg.EndsWith(".matrix")) {
        Read-MatrixFile $arg
        Print-Puzzle
        $script:count = 0
        $null = Solve-Puzzle
    }
}

$end = Get-Date
$diff = ($end - $start).TotalSeconds
Write-Host ("Seconds to process {0:N3}" -f $diff)
