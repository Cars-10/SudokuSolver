#!/usr/bin/env php
<?php
declare(strict_types=1);

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
$cp_iterations = 0;

// ============================================================================
// BITSET HELPER FUNCTIONS
// ============================================================================

function has_candidate($set, $digit) {
    return ($set & (1 << $digit)) != 0;
}

function add_candidate($set, $digit) {
    return $set | (1 << $digit);
}

function remove_candidate($set, $digit) {
    return $set & ~(1 << $digit);
}

function count_candidates($set) {
    // Custom popcount implementation (PHP doesn't have built-in)
    $count = 0;
    while ($set > 0) {
        $count += $set & 1;
        $set >>= 1;
    }
    return $count;
}

function get_first_candidate($set) {
    for ($digit = 1; $digit <= 9; $digit++) {
        if (has_candidate($set, $digit)) {
            return $digit;
        }
    }
    return 0;
}

// ============================================================================
// GRID FUNCTIONS
// ============================================================================

function init_grid($puzzle) {
    $grid = [
        'values' => [],
        'candidates' => []
    ];

    for ($row = 0; $row < 9; $row++) {
        for ($col = 0; $col < 9; $col++) {
            if ($puzzle[$row][$col] == 0) {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                $grid['values'][$row][$col] = 0;
                $grid['candidates'][$row][$col] = 0x3FE;  // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                $digit = $puzzle[$row][$col];
                $grid['values'][$row][$col] = $digit;
                $grid['candidates'][$row][$col] = (1 << $digit);
            }
        }
    }

    return $grid;
}

function get_peers($row, $col) {
    $peers = [];

    // Same row (9 cells minus self = 8)
    for ($c = 0; $c < 9; $c++) {
        if ($c != $col) {
            $peers[] = [$row, $c];
        }
    }

    // Same column (9 cells minus self = 8)
    for ($r = 0; $r < 9; $r++) {
        if ($r != $row) {
            $peers[] = [$r, $col];
        }
    }

    // Same 3x3 box (9 cells minus self minus already counted = 4)
    $box_row = intdiv($row, 3) * 3;
    $box_col = intdiv($col, 3) * 3;
    for ($r = $box_row; $r < $box_row + 3; $r++) {
        for ($c = $box_col; $c < $box_col + 3; $c++) {
            if ($r != $row && $c != $col) {
                $peers[] = [$r, $c];
            }
        }
    }

    return $peers;
}

// ============================================================================
// CONSTRAINT PROPAGATION
// ============================================================================

function eliminate(&$grid, $row, $col, $digit) {
    // Check if digit is already eliminated
    if (!has_candidate($grid['candidates'][$row][$col], $digit)) {
        return true;  // Already eliminated, no change
    }

    // Remove digit from candidates
    $grid['candidates'][$row][$col] = remove_candidate($grid['candidates'][$row][$col], $digit);

    // Check for contradiction (no candidates left)
    $remaining = count_candidates($grid['candidates'][$row][$col]);
    if ($remaining == 0) {
        return false;  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if ($remaining == 1 && $grid['values'][$row][$col] == 0) {
        $last_digit = get_first_candidate($grid['candidates'][$row][$col]);
        if (!assign($grid, $row, $col, $last_digit)) {
            return false;  // Assignment caused contradiction
        }
    }

    return true;
}

function assign(&$grid, $row, $col, $digit) {
    global $cp_iterations;

    // Increment iteration counter (this is our benchmark metric)
    $cp_iterations++;

    // Set value
    $grid['values'][$row][$col] = $digit;
    $grid['candidates'][$row][$col] = (1 << $digit);

    // Eliminate digit from all peers
    $peers = get_peers($row, $col);

    foreach ($peers as $peer) {
        list($peer_row, $peer_col) = $peer;

        if (!eliminate($grid, $peer_row, $peer_col, $digit)) {
            return false;  // Contradiction in peer elimination
        }
    }

    return true;
}

function propagate(&$grid) {
    $changed = true;

    while ($changed) {
        $changed = false;

        // Strategy 1: Singleton elimination
        // If a cell has only one candidate, assign it
        for ($row = 0; $row < 9; $row++) {
            for ($col = 0; $col < 9; $col++) {
                if ($grid['values'][$row][$col] == 0) {
                    $num_candidates = count_candidates($grid['candidates'][$row][$col]);
                    if ($num_candidates == 0) {
                        return false;  // Contradiction
                    }
                    if ($num_candidates == 1) {
                        $digit = get_first_candidate($grid['candidates'][$row][$col]);
                        if (!assign($grid, $row, $col, $digit)) {
                            return false;  // Assignment caused contradiction
                        }
                        $changed = true;
                    }
                }
            }
        }

        // Strategy 2: Hidden singles
        // For each unit (row, col, box), if a digit appears in only one cell, assign it

        // Check rows
        for ($row = 0; $row < 9; $row++) {
            for ($digit = 1; $digit <= 9; $digit++) {
                $count = 0;
                $last_col = -1;
                for ($col = 0; $col < 9; $col++) {
                    if ($grid['values'][$row][$col] == $digit) {
                        $count = 0;  // Already assigned
                        break;
                    }
                    if (has_candidate($grid['candidates'][$row][$col], $digit)) {
                        $count++;
                        $last_col = $col;
                    }
                }
                if ($count == 1) {
                    if (!assign($grid, $row, $last_col, $digit)) {
                        return false;
                    }
                    $changed = true;
                } else if ($count == 0) {
                    // Check if digit is already assigned in this row
                    $found = false;
                    for ($col = 0; $col < 9; $col++) {
                        if ($grid['values'][$row][$col] == $digit) {
                            $found = true;
                            break;
                        }
                    }
                    if (!$found) {
                        return false;  // Digit cannot be placed anywhere in row
                    }
                }
            }
        }

        // Check columns
        for ($col = 0; $col < 9; $col++) {
            for ($digit = 1; $digit <= 9; $digit++) {
                $count = 0;
                $last_row = -1;
                for ($row = 0; $row < 9; $row++) {
                    if ($grid['values'][$row][$col] == $digit) {
                        $count = 0;  // Already assigned
                        break;
                    }
                    if (has_candidate($grid['candidates'][$row][$col], $digit)) {
                        $count++;
                        $last_row = $row;
                    }
                }
                if ($count == 1) {
                    if (!assign($grid, $last_row, $col, $digit)) {
                        return false;
                    }
                    $changed = true;
                } else if ($count == 0) {
                    // Check if digit is already assigned in this column
                    $found = false;
                    for ($row = 0; $row < 9; $row++) {
                        if ($grid['values'][$row][$col] == $digit) {
                            $found = true;
                            break;
                        }
                    }
                    if (!$found) {
                        return false;  // Digit cannot be placed anywhere in column
                    }
                }
            }
        }

        // Check boxes
        for ($box = 0; $box < 9; $box++) {
            $box_row = intdiv($box, 3) * 3;
            $box_col = ($box % 3) * 3;

            for ($digit = 1; $digit <= 9; $digit++) {
                $count = 0;
                $last_r = -1;
                $last_c = -1;

                for ($r = $box_row; $r < $box_row + 3; $r++) {
                    for ($c = $box_col; $c < $box_col + 3; $c++) {
                        if ($grid['values'][$r][$c] == $digit) {
                            $count = 0;  // Already assigned
                            goto next_box_digit;
                        }
                        if (has_candidate($grid['candidates'][$r][$c], $digit)) {
                            $count++;
                            $last_r = $r;
                            $last_c = $c;
                        }
                    }
                }

                if ($count == 1) {
                    if (!assign($grid, $last_r, $last_c, $digit)) {
                        return false;
                    }
                    $changed = true;
                } else if ($count == 0) {
                    // Check if digit is already assigned in this box
                    $found = false;
                    for ($r = $box_row; $r < $box_row + 3; $r++) {
                        for ($c = $box_col; $c < $box_col + 3; $c++) {
                            if ($grid['values'][$r][$c] == $digit) {
                                $found = true;
                                goto found_box_digit;
                            }
                        }
                    }
                    found_box_digit:
                    if (!$found) {
                        return false;  // Digit cannot be placed anywhere in box
                    }
                }

                next_box_digit:
                continue;
            }
        }
    }

    return true;  // Success - reached fixpoint
}

// ============================================================================
// SEARCH
// ============================================================================

function find_mrv_cell($grid) {
    $min_candidates = 10;  // More than 9, so any cell will be smaller
    $found_row = -1;
    $found_col = -1;

    for ($r = 0; $r < 9; $r++) {
        for ($c = 0; $c < 9; $c++) {
            if ($grid['values'][$r][$c] == 0) {
                $num_candidates = count_candidates($grid['candidates'][$r][$c]);
                if ($num_candidates < $min_candidates) {
                    $min_candidates = $num_candidates;
                    $found_row = $r;
                    $found_col = $c;
                }
            }
        }
    }

    if ($found_row == -1) {
        return null;  // No empty cells
    }

    return [$found_row, $found_col];
}

function cp_search($grid) {
    // Base case: check if grid is complete
    $mrv = find_mrv_cell($grid);
    if ($mrv === null) {
        // No empty cells - grid is complete, extract solution
        $solution = [];
        for ($r = 0; $r < 9; $r++) {
            for ($c = 0; $c < 9; $c++) {
                $solution[] = $grid['values'][$r][$c];
            }
        }
        return $solution;
    }

    list($mrv_row, $mrv_col) = $mrv;

    // Recursive case: try each candidate for the MRV cell
    $candidates = $grid['candidates'][$mrv_row][$mrv_col];

    for ($digit = 1; $digit <= 9; $digit++) {
        if (has_candidate($candidates, $digit)) {
            // Save grid state for backtracking (deep copy)
            $grid_copy = [
                'values' => array_map(function($row) { return $row; }, $grid['values']),
                'candidates' => array_map(function($row) { return $row; }, $grid['candidates'])
            ];

            // Try assigning this digit
            if (assign($grid, $mrv_row, $mrv_col, $digit)) {
                // Assignment succeeded, propagate constraints
                if (propagate($grid)) {
                    // Propagation succeeded, recurse
                    $result = cp_search($grid);
                    if ($result !== false) {
                        return $result;  // Found solution
                    }
                }
            }

            // Failed - restore grid state and try next candidate
            $grid = $grid_copy;
        }
    }

    // All candidates exhausted - dead end
    return false;
}

// ============================================================================
// I/O FUNCTIONS
// ============================================================================

function print_puzzle($puzzle) {
    echo "\nPuzzle:\n";
    for ($r = 0; $r < 9; $r++) {
        for ($c = 0; $c < 9; $c++) {
            echo $puzzle[$r][$c] . " ";
        }
        echo "\n";
    }
}

function parse_matrix($filename) {
    if (!file_exists($filename)) {
        fwrite(STDERR, "Error opening file '$filename'\n");
        return null;
    }

    // Normalize path for output
    $display_path = $filename;
    if (strpos($filename, '/app/Matrices/') === 0) {
        $display_path = '../' . substr($filename, 5);
    }
    echo "$display_path\n";

    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    $puzzle = [];
    $line_count = 0;

    foreach ($lines as $line) {
        $line = trim($line);
        if (empty($line) || $line[0] === '#') {
            continue;
        }

        $parts = preg_split('/\s+/', $line);
        if (count($parts) >= 9) {
            $row = [];
            for ($i = 0; $i < 9; $i++) {
                $row[] = (int)$parts[$i];
                echo $parts[$i] . " ";
            }
            echo "\n";
            $puzzle[] = $row;
            $line_count++;

            if ($line_count == 9) {
                break;
            }
        }
    }

    if ($line_count != 9) {
        fwrite(STDERR, "Error: file does not contain 9 lines\n");
        return null;
    }

    return $puzzle;
}

// ============================================================================
// MAIN
// ============================================================================

if ($argc < 2) {
    fwrite(STDERR, "Usage: php cp.php <matrix_file>\n");
    exit(1);
}

$start_time = microtime(true);

$filename = $argv[1];

$puzzle = parse_matrix($filename);
if ($puzzle === null) {
    exit(1);
}

print_puzzle($puzzle);

// Initialize CP grid
$grid = init_grid($puzzle);

// Apply initial propagation
if (!propagate($grid)) {
    echo "\nNo solution found (contradiction during initial propagation)\n";
    $end_time = microtime(true);
    $elapsed = $end_time - $start_time;
    printf("Seconds to process %.3f\n", $elapsed);
    exit(0);
}

// Run search
$cp_iterations = 0;
$solution = cp_search($grid);

if ($solution !== false) {
    // Convert solution array back to 2D for printing
    $solution_grid = [];
    for ($r = 0; $r < 9; $r++) {
        $row = [];
        for ($c = 0; $c < 9; $c++) {
            $row[] = $solution[$r * 9 + $c];
        }
        $solution_grid[] = $row;
    }

    print_puzzle($solution_grid);
    echo "\nSolved in Iterations=$cp_iterations\n\n";
} else {
    echo "\nNo solution found\n\n";
}

$end_time = microtime(true);
$elapsed = $end_time - $start_time;
printf("Seconds to process %.3f\n", $elapsed);
