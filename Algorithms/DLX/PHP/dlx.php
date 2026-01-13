#!/usr/bin/env php
<?php
declare(strict_types=1);

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
$dlx_iterations = 0;

// ============================================================================
// DLX CORE FUNCTIONS
// ============================================================================

function dlx_cover_column($col) {
    // Remove column header from the header list
    $col->right->left = $col->left;
    $col->left->right = $col->right;

    // For each row in this column
    $row = $col->down;
    while ($row !== $col) {
        // For each node in this row (excluding the column itself)
        $right = $row->right;
        while ($right !== $row) {
            // Remove this node from its column
            $right->down->up = $right->up;
            $right->up->down = $right->down;
            $right->column->size--;
            $right = $right->right;
        }
        $row = $row->down;
    }
}

function dlx_uncover_column($col) {
    // For each row in this column (in reverse order)
    $row = $col->up;
    while ($row !== $col) {
        // For each node in this row (in reverse order)
        $left = $row->left;
        while ($left !== $row) {
            // Restore this node to its column
            $left->column->size++;
            $left->down->up = $left;
            $left->up->down = $left;
            $left = $left->left;
        }
        $row = $row->up;
    }

    // Restore column header to the header list
    $col->right->left = $col;
    $col->left->right = $col;
}

function choose_column($root) {
    $best = null;
    $min_size = PHP_INT_MAX;

    $col = $root->right;
    while ($col !== $root) {
        if ($col->size < $min_size) {
            $min_size = $col->size;
            $best = $col;
        }
        $col = $col->right;
    }

    return $best;
}

function dlx_search($root, $k, &$solution) {
    global $dlx_iterations;
    $dlx_iterations++;

    // If matrix is empty, we found a solution
    if ($root->right === $root) {
        return true;
    }

    // Choose column with minimum size
    $col = choose_column($root);

    // If column has no rows, no solution possible
    if ($col === null || $col->size == 0) {
        return false;
    }

    // Cover this column
    dlx_cover_column($col);

    // Try each row in this column
    $row = $col->down;
    while ($row !== $col) {
        // Add row to partial solution
        $solution[$k] = $row->row_id;

        // Cover all other columns in this row
        $right = $row->right;
        while ($right !== $row) {
            dlx_cover_column($right->column);
            $right = $right->right;
        }

        // Recurse
        if (dlx_search($root, $k + 1, $solution)) {
            return true;
        }

        // Backtrack: uncover all columns in this row
        $left = $row->left;
        while ($left !== $row) {
            dlx_uncover_column($left->column);
            $left = $left->left;
        }

        $row = $row->down;
    }

    // Uncover column
    dlx_uncover_column($col);

    return false;
}

// ============================================================================
// SUDOKU-SPECIFIC FUNCTIONS
// ============================================================================

function get_position_col($r, $c) {
    return $r * 9 + $c;
}

function get_row_col($r, $n) {
    return 81 + $r * 9 + ($n - 1);
}

function get_col_col($c, $n) {
    return 162 + $c * 9 + ($n - 1);
}

function get_box_col($r, $c, $n) {
    $box = intdiv($r, 3) * 3 + intdiv($c, 3);
    return 243 + $box * 9 + ($n - 1);
}

function init_dlx_matrix() {
    // Create root column
    $root = new stdClass();
    $root->left = $root;
    $root->right = $root;
    $root->up = $root;
    $root->down = $root;
    $root->size = 0;
    $root->name = 'root';

    // Create 324 column headers
    $columns = [];
    for ($i = 0; $i < 324; $i++) {
        $col = new stdClass();
        $col->up = $col;
        $col->down = $col;
        $col->size = 0;
        $col->name = "C$i";

        // Link into header list
        $col->left = $root->left;
        $col->right = $root;
        $root->left->right = $col;
        $root->left = $col;

        $columns[$i] = $col;
    }

    return ['root' => $root, 'columns' => $columns];
}

function add_node($dlx, $col_idx, $row_id) {
    $col = $dlx['columns'][$col_idx];

    $node = new stdClass();
    $node->column = $col;
    $node->row_id = $row_id;

    // Insert at end of column's circular list
    $node->down = $col;
    $node->up = $col->up;
    $col->up->down = $node;
    $col->up = $node;
    $col->size++;

    return $node;
}

function build_dlx_row($dlx, $r, $c, $n, $row_id, &$row_info) {
    // Store row metadata
    $row_info[$row_id] = ['row' => $r, 'col' => $c, 'num' => $n];

    // Create nodes for the 4 constraints
    $n1 = add_node($dlx, get_position_col($r, $c), $row_id);
    $n2 = add_node($dlx, get_row_col($r, $n), $row_id);
    $n3 = add_node($dlx, get_col_col($c, $n), $row_id);
    $n4 = add_node($dlx, get_box_col($r, $c, $n), $row_id);

    // Link nodes horizontally in circular list
    $n1->right = $n2;
    $n1->left = $n4;

    $n2->right = $n3;
    $n2->left = $n1;

    $n3->right = $n4;
    $n3->left = $n2;

    $n4->right = $n1;
    $n4->left = $n3;

    return $n1;
}

function build_exact_cover_matrix($puzzle) {
    $dlx = init_dlx_matrix();
    $row_info = [];
    $row_starts = [];
    $row_id = 0;

    for ($r = 0; $r < 9; $r++) {
        for ($c = 0; $c < 9; $c++) {
            if ($puzzle[$r][$c] != 0) {
                // Cell has a clue - create only one row for that value
                $row_starts[$row_id] = build_dlx_row($dlx, $r, $c, $puzzle[$r][$c], $row_id, $row_info);
                $row_id++;
            } else {
                // Cell is empty - create rows for all possible values
                for ($n = 1; $n <= 9; $n++) {
                    $row_starts[$row_id] = build_dlx_row($dlx, $r, $c, $n, $row_id, $row_info);
                    $row_id++;
                }
            }
        }
    }

    return ['dlx' => $dlx, 'row_info' => $row_info, 'row_starts' => $row_starts, 'puzzle' => $puzzle];
}

function cover_clues($data) {
    $puzzle = $data['puzzle'];
    $row_info = $data['row_info'];
    $row_starts = $data['row_starts'];

    for ($r = 0; $r < 9; $r++) {
        for ($c = 0; $c < 9; $c++) {
            if ($puzzle[$r][$c] != 0) {
                $n = $puzzle[$r][$c];

                // Find the row for this clue
                foreach ($row_starts as $row_id => $node) {
                    if (isset($row_info[$row_id]) &&
                        $row_info[$row_id]['row'] == $r &&
                        $row_info[$row_id]['col'] == $c &&
                        $row_info[$row_id]['num'] == $n) {

                        // Cover all columns in this row
                        $curr = $node;
                        do {
                            dlx_cover_column($curr->column);
                            $curr = $curr->right;
                        } while ($curr !== $node);
                        break;
                    }
                }
            }
        }
    }
}

function extract_solution($data, $solution) {
    $puzzle = $data['puzzle'];
    $row_info = $data['row_info'];
    $solution_grid = $puzzle;

    foreach ($solution as $row_id) {
        if (isset($row_info[$row_id])) {
            $info = $row_info[$row_id];
            $solution_grid[$info['row']][$info['col']] = $info['num'];
        }
    }

    return $solution_grid;
}

function print_puzzle($grid) {
    echo "\nPuzzle:\n";
    for ($r = 0; $r < 9; $r++) {
        for ($c = 0; $c < 9; $c++) {
            echo $grid[$r][$c] . " ";
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
    fwrite(STDERR, "Usage: php dlx.php <file1> <file2> ...\n");
    exit(1);
}

$start_time = microtime(true);

for ($i = 1; $i < $argc; $i++) {
    $filename = $argv[$i];

    if (substr($filename, -7) !== '.matrix') {
        continue;
    }

    $puzzle = parse_matrix($filename);
    if ($puzzle === null) {
        continue;
    }

    print_puzzle($puzzle);

    // Build DLX matrix
    $data = build_exact_cover_matrix($puzzle);

    // Cover pre-filled clues
    cover_clues($data);

    // Solve using DLX
    $dlx_iterations = 0;
    $solution = [];
    $result = dlx_search($data['dlx']['root'], 0, $solution);

    if ($result) {
        $solution_grid = extract_solution($data, $solution);
        print_puzzle($solution_grid);
        echo "\nSolved in Iterations=$dlx_iterations\n\n";
    } else {
        echo "\nNo solution found\n\n";
    }
}

$end_time = microtime(true);
$elapsed = $end_time - $start_time;
printf("Seconds to process %.3f\n", $elapsed);
