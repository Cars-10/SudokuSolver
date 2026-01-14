module main

import os
import time

// DLX Node structure - uses heap allocation for long-lived circular linked structure
struct DlxNode {
mut:
	left   &DlxNode = unsafe { nil }
	right  &DlxNode = unsafe { nil }
	up     &DlxNode = unsafe { nil }
	down   &DlxNode = unsafe { nil }
	column &DlxNode = unsafe { nil } // Points to column header
	size   int                        // Only used by column headers
	row_id int = -1                   // Row ID for solution tracking
	name   string
}

// Global iteration counter
__global (
	dlx_iterations int
)

// Cover a column in the DLX matrix
fn cover_column(col &DlxNode) {
	// Remove column header from the header list
	unsafe {
		col.right.left = col.left
		col.left.right = col.right
	}

	// For each row in this column
	mut row_node := col.down
	for voidptr(row_node) != voidptr(col) {
		// For each node in this row (excluding the column itself)
		mut right_node := row_node.right
		for voidptr(right_node) != voidptr(row_node) {
			// Remove this node from its column
			unsafe {
				right_node.down.up = right_node.up
				right_node.up.down = right_node.down
				right_node.column.size--
			}
			right_node = right_node.right
		}
		row_node = row_node.down
	}
}

// Uncover a column (exact reverse of cover)
fn uncover_column(col &DlxNode) {
	// For each row in this column (in reverse order)
	mut row_node := col.up
	for voidptr(row_node) != voidptr(col) {
		// For each node in this row (in reverse order)
		mut left_node := row_node.left
		for voidptr(left_node) != voidptr(row_node) {
			// Restore this node to its column
			unsafe {
				left_node.column.size++
				left_node.down.up = left_node
				left_node.up.down = left_node
			}
			left_node = left_node.left
		}
		row_node = row_node.up
	}

	// Restore column header to the header list
	unsafe {
		col.right.left = col
		col.left.right = col
	}
}

// Choose column with minimum size (Knuth's S heuristic)
fn choose_column(root &DlxNode) &DlxNode {
	mut best := unsafe { nil }
	mut min_size := 2147483647 // INT_MAX

	mut col_node := root.right
	for voidptr(col_node) != voidptr(root) {
		if col_node.size < min_size {
			min_size = col_node.size
			best = col_node
		}
		col_node = col_node.right
	}

	return best
}

// DLX Search - Algorithm X with Dancing Links
fn dlx_search(root &DlxNode, k int, mut solution []int) bool {
	unsafe {
		dlx_iterations++ // Count every search call
	}

	// If matrix is empty, we found a solution
	if voidptr(root.right) == voidptr(root) {
		return true
	}

	// Choose column with minimum size
	col := choose_column(root)
	if voidptr(col) == unsafe { nil } {
		return false
	}

	// If column has no rows, no solution possible
	if col.size == 0 {
		return false
	}

	// Cover this column
	cover_column(col)

	// Try each row in this column
	mut row_node := col.down
	for voidptr(row_node) != voidptr(col) {
		// Add row to partial solution
		solution[k] = row_node.row_id

		// Cover all other columns in this row
		mut right_node := row_node.right
		for voidptr(right_node) != voidptr(row_node) {
			cover_column(right_node.column)
			right_node = right_node.right
		}

		// Recurse
		if dlx_search(root, k + 1, mut solution) {
			return true // Solution found
		}

		// Backtrack: uncover all columns in this row
		mut left_node := row_node.left
		for voidptr(left_node) != voidptr(row_node) {
			uncover_column(left_node.column)
			left_node = left_node.left
		}

		row_node = row_node.down
	}

	// Uncover column
	uncover_column(col)

	return false // No solution found
}

// Sudoku-specific structures
struct RowInfo {
	row   int
	col   int
	digit int
}

// Cover given clues before search
fn cover_clues(puzzle [][]int, row_starts []&DlxNode, row_infos []RowInfo) {
	for r in 0 .. 9 {
		for c in 0 .. 9 {
			if puzzle[r][c] != 0 {
				digit := puzzle[r][c]
				// Find the row for this clue
				for i, info in row_infos {
					if info.row == r && info.col == c && info.digit == digit {
						// Cover all columns in this row (selects this row)
						node_start := row_starts[i]
						mut curr := node_start
						// Traverse the row and cover each column
						for {
							cover_column(curr.column)
							curr = curr.right
							if voidptr(curr) == voidptr(node_start) {
								break
							}
						}
						break
					}
				}
			}
		}
	}
}

// Build the exact cover matrix for Sudoku
fn build_sudoku_matrix(puzzle [][]int) (&DlxNode, []RowInfo, []&DlxNode) {
	// Create root node
	mut root := &DlxNode{
		name: 'root'
	}
	unsafe {
		root.left = root
		root.right = root
		root.up = root
		root.down = root
	}

	// Create 324 column headers (81 cell + 81 row + 81 col + 81 box constraints)
	num_cols := 324
	mut columns := []&DlxNode{cap: num_cols}

	mut prev := root
	for i in 0 .. num_cols {
		mut col := &DlxNode{
			name: '${i}'
			size: 0
		}
		unsafe {
			col.left = prev
			col.right = root
			col.up = col
			col.down = col
			col.column = col
			prev.right = col
			root.left = col
		}
		columns << col
		prev = col
	}

	// Build rows for Sudoku exact cover
	mut row_infos := []RowInfo{}
	mut row_starts := []&DlxNode{cap: 729}
	mut row_id := 0

	for row in 0 .. 9 {
		for col in 0 .. 9 {
			// Determine which digits to try
			mut digits := []int{}
			if puzzle[row][col] == 0 {
				// Empty cell: try all digits 1-9
				for d in 1 .. 10 {
					digits << d
				}
			} else {
				// Given clue: only use that digit
				digits << puzzle[row][col]
			}

			for digit in digits {
				// Calculate column indices for this (row, col, digit) combination
				col_cell := row * 9 + col               // Cell constraint
				col_row := 81 + row * 9 + (digit - 1)   // Row-digit constraint
				col_col := 162 + col * 9 + (digit - 1)  // Col-digit constraint
				box := (row / 3) * 3 + (col / 3)
				col_box := 243 + box * 9 + (digit - 1)  // Box-digit constraint

				col_indices := [col_cell, col_row, col_col, col_box]

				// Create nodes for this row
				mut nodes := []&DlxNode{cap: 4}
				for col_idx in col_indices {
					node := &DlxNode{
						row_id: row_id
					}
					unsafe {
						node.column = columns[col_idx]
						node.left = node
						node.right = node
					}
					nodes << node
				}

				// Link nodes horizontally (circular)
				for i in 0 .. nodes.len {
					unsafe {
						nodes[i].left = nodes[(i + nodes.len - 1) % nodes.len]
						nodes[i].right = nodes[(i + 1) % nodes.len]
					}
				}

				// Link nodes vertically to columns
				for i, node in nodes {
					col_idx := col_indices[i]
					col_header := columns[col_idx]

					unsafe {
						node.up = col_header.up
						node.down = col_header
						col_header.up.down = node
						col_header.up = node
						col_header.size++
					}
				}

				// Store row info for solution reconstruction
				row_infos << RowInfo{
					row: row
					col: col
					digit: digit
				}
				// Store first node of row
				row_starts << nodes[0]
				row_id++
			}
		}
	}

	return root, row_infos, row_starts
}

// Read matrix file
fn read_matrix_file(filename string) ([][]int, bool) {
	// Normalize path for output (convert absolute to relative)
	if filename.starts_with('/app/Matrices/') {
		display_path := filename[5..] // Skip "/app/" to get "Matrices/..."
		println('../${display_path}')
	} else {
		println(filename)
	}

	content := os.read_file(filename) or {
		eprintln('Error reading file: ${filename}')
		return [][]int{}, false
	}

	lines := content.split('\n')
	mut puzzle := [][]int{len: 9, init: []int{len: 9, init: 0}}
	mut line_count := 0

	for line in lines {
		trimmed := line.trim_space()
		if trimmed.len == 0 || trimmed.starts_with('#') {
			continue
		}

		parts := trimmed.split_any(' \t')
		mut values := []string{}
		for part in parts {
			if part.len > 0 {
				values << part
			}
		}

		if values.len >= 9 && line_count < 9 {
			for i in 0 .. 9 {
				puzzle[line_count][i] = values[i].int()
				print('${values[i]} ')
			}
			println('')
			line_count++
		}

		if line_count >= 9 {
			break
		}
	}

	return puzzle, line_count == 9
}

// Print puzzle
fn print_puzzle(puzzle [][]int) {
	println('\nPuzzle:')
	for row in 0 .. 9 {
		for col in 0 .. 9 {
			print('${puzzle[row][col]} ')
		}
		println('')
	}
}

fn main() {
	sw := time.new_stopwatch()

	args := os.args[1..]
	for filename in args {
		if !filename.ends_with('.matrix') {
			continue
		}

		puzzle, ok := read_matrix_file(filename)
		if !ok {
			eprintln('Error reading ${filename}')
			continue
		}

		print_puzzle(puzzle)

		// Build DLX matrix
		root, row_infos, row_starts := build_sudoku_matrix(puzzle)

		// Cover given clues
		cover_clues(puzzle, row_starts, row_infos)

		// Solve
		unsafe {
			dlx_iterations = 0
		}
		mut solution := []int{len: 81, init: -1}
		if dlx_search(root, 0, mut solution) {
			// Reconstruct solved puzzle
			mut solved := [][]int{len: 9, init: []int{len: 9, init: 0}}
			// Copy given clues
			for r in 0 .. 9 {
				for c in 0 .. 9 {
					solved[r][c] = puzzle[r][c]
				}
			}
			// Fill in solution
			for i in 0 .. solution.len {
				if solution[i] >= 0 {
					info := row_infos[solution[i]]
					solved[info.row][info.col] = info.digit
				}
			}

			print_puzzle(solved)
			unsafe {
				println('\nSolved in Iterations=${dlx_iterations}\n')
			}
		} else {
			println('No solution found')
		}
	}

	elapsed := f64(sw.elapsed().microseconds()) / 1000000.0
	println('Seconds to process ${elapsed:.3}')
}
