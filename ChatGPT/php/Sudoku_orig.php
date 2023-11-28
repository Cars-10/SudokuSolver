<?php

const SIZE = 9;
const SUBGRID_SIZE = 3;

$solverCount = 0;
$start_time = microtime(true); // Start the timer

function solveSudoku(&$board) {
    global $solverCount;

    $solverCount++;

    for ($row = 0; $row < SIZE; $row++) {
        for ($col = 0; $col < SIZE; $col++) {
            if ($board[$row][$col] == 0) {
                for ($num = 1; $num <= SIZE; $num++) {
                    if (isValidMove($board, $row, $col, $num)) {
                        $board[$row][$col] = $num;
                        if (solveSudoku($board)) {
                            return true;
                        }
                        $board[$row][$col] = 0;  // Backtrack
                    }
                }
                return false;
            }
        }
    }
    return true;
}

function isValidMove(&$board, $row, $col, $num) {
    global $solverCount;

    // Check row, column, and subgrid for conflicts.
    for ($i = 0; $i < SIZE; $i++) {
        if ($board[$row][$i] == $num ||
            $board[$i][$col] == $num ||
            $board[$row - $row % SUBGRID_SIZE + (int)($i / SUBGRID_SIZE)]
                  [$col - $col % SUBGRID_SIZE + $i % SUBGRID_SIZE] == $num) {
            return false;
        }
    }
    return true;
}

function printBoard(&$board) {
    echo "\nPuzzle:\n";
    for ($i = 0; $i < SIZE; $i++) {
        for ($j = 0; $j < SIZE; $j++) {
            echo $board[$i][$j] . " ";
        }
        echo "\n";
    }
}

function readSudokuFromFile($filePath) {
    global $solverCount;

    $board = array();

    $lines = file($filePath, FILE_IGNORE_NEW_LINES);
    if ($lines === false) {
        echo "Error opening file: $filePath\n";
        exit(1);
    }

    foreach ($lines as $line) {
        // Ignore lines starting with #
        if (strlen($line) > 0 && $line[0] == '#') {
            continue;
        }
        $val = 0;
        $row = array();
        for ($i = 0; $i < SIZE; $i++) {
            if (strlen($line) > $i * 2) {
                $val = intval($line[$i * 2]);
                $row[] = $val;
            } else {
                $row[] = 0;
            }
        }
        $board[] = $row;
    }

    return $board;
}

if ($argc < 2) {
    echo "Usage: php sudoku.php input_file1.txt input_file2.txt ...\n";
    exit(1);
}

for ($i = 1; $i < $argc; $i++) {
    echo "$argv[$i]:\n";
    $board = readSudokuFromFile($argv[$i]);
    printBoard($board);

    if (solveSudoku($board)) {
        printBoard($board);
        echo "\nSolver was called $solverCount times\n\n";
    } else {
        echo "No solution found for $argv[$i]\n\n";
    }
}
$end_time = microtime(true); // Stop the timer
$execution_time = ($end_time - $start_time) * 1000; // Calculate execution time in milliseconds

echo "Seconds to process: " . number_format($execution_time / 1000000, 3) . " ms\n\n";
