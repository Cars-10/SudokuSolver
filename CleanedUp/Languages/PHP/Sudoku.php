<?php
$puzzle = [];
$iterations = 0;

function print_board() {
    global $puzzle;
    echo "Puzzle:\n";
    for ($i = 0; $i < 9; $i++) {
        for ($j = 0; $j < 9; $j++) {
            echo $puzzle[$i][$j] . " ";
        }
        echo "\n";
    }
}

function is_possible($row, $col, $num) {
    global $puzzle;
    for ($i = 0; $i < 9; $i++) {
        if ($puzzle[$row][$i] == $num || $puzzle[$i][$col] == $num) {
            return false;
        }
    }
    $startRow = floor($row / 3) * 3;
    $startCol = floor($col / 3) * 3;
    for ($i = 0; $i < 3; $i++) {
        for ($j = 0; $j < 3; $j++) {
            if ($puzzle[$startRow + $i][$startCol + $j] == $num) {
                return false;
            }
        }
    }
    return true;
}

function solve($row, $col) {
    global $puzzle, $iterations;
    if ($row == 9) {
        return true;
    }
    $nextRow = $row;
    $nextCol = $col + 1;
    if ($nextCol == 9) {
        $nextRow = $row + 1;
        $nextCol = 0;
    }

    if ($puzzle[$row][$col] != 0) {
        return solve($nextRow, $nextCol);
    }

    for ($num = 1; $num <= 9; $num++) {
        $iterations++;
        if (is_possible($row, $col, $num)) {
            $puzzle[$row][$col] = $num;
            if (solve($nextRow, $nextCol)) {
                return true;
            }
            $puzzle[$row][$col] = 0;
        }
    }
    return false;
}

function read_board($filename) {
    global $puzzle;
    $puzzle = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    $row = 0;
    foreach ($lines as $line) {
        $line = trim($line);
        if (empty($line) || strpos($line, '#') === 0) {
            continue;
        }
        $parts = preg_split('/\s+/', $line);
        $col = 0;
        foreach ($parts as $part) {
            if ($col < 9) {
                $puzzle[$row][$col] = (int)$part;
                $col++;
            }
        }
        $row++;
        if ($row == 9) break;
    }
}

if ($argc < 2) {
    echo "Usage: php Sudoku.php <file1> <file2> ...\n";
    exit(1);
}

for ($i = 1; $i < $argc; $i++) {
    $filename = $argv[$i];
    echo "\nProcessing $filename\n";
    read_board($filename);
    print_board();
    $iterations = 0;
    if (solve(0, 0)) {
        print_board();
        echo "\nSolved in Iterations=$iterations\n";
    } else {
        echo "No solution found\n";
    }
}
?>
