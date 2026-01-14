module main

import os
import time

// Grid structures
struct Grid {
mut:
	values     [9][9]int
	candidates [9][9]int // Bitsets (bits 1-9 represent digits)
}

// Global iteration counter
__global (
	cp_iterations i64
)

// Bitwise operations
fn has_candidate(set int, digit int) bool {
	return (set & (1 << digit)) != 0
}

fn remove_candidate(set int, digit int) int {
	return set & ~(1 << digit)
}

fn add_candidate(set int, digit int) int {
	return set | (1 << digit)
}

fn count_candidates(set int) int {
	mut count := 0
	for d in 1 .. 10 {
		if has_candidate(set, d) {
			count++
		}
	}
	return count
}

fn get_first_candidate(set int) int {
	for d in 1 .. 10 {
		if has_candidate(set, d) {
			return d
		}
	}
	return 0
}

// Get all 20 peers for a cell (row, col, box)
fn get_peers(row int, col int) [][2]int {
	mut peers := [][2]int{cap: 20}

	// Same row (9 cells minus self = 8)
	for c in 0 .. 9 {
		if c != col {
			peers << [row, c]!
		}
	}

	// Same column (9 cells minus self = 8)
	for r in 0 .. 9 {
		if r != row {
			peers << [r, col]!
		}
	}

	// Same 3x3 box (9 cells minus self minus already counted = 4)
	box_row := (row / 3) * 3
	box_col := (col / 3) * 3
	for r in box_row .. box_row + 3 {
		for c in box_col .. box_col + 3 {
			if r != row && c != col {
				peers << [r, c]!
			}
		}
	}

	return peers
}

// Initialize grid from puzzle
fn init_grid(puzzle [][]int) Grid {
	mut grid := Grid{}

	for row in 0 .. 9 {
		for col in 0 .. 9 {
			if puzzle[row][col] == 0 {
				// Empty cell: set all candidates 1-9 (bits 1-9 set)
				grid.values[row][col] = 0
				grid.candidates[row][col] = 0x3FE // Binary: 0011 1111 1110 (bits 1-9)
			} else {
				// Given clue: set single value
				digit := puzzle[row][col]
				grid.values[row][col] = digit
				grid.candidates[row][col] = (1 << digit)
			}
		}
	}

	return grid
}

// Eliminate a digit from a cell's candidates
fn eliminate(mut grid Grid, row int, col int, digit int) bool {
	// Check if digit is already eliminated
	if !has_candidate(grid.candidates[row][col], digit) {
		return true // Already eliminated, no change
	}

	// Remove digit from candidates
	grid.candidates[row][col] = remove_candidate(grid.candidates[row][col], digit)

	// Check for contradiction (no candidates left)
	remaining := count_candidates(grid.candidates[row][col])
	if remaining == 0 {
		return false // Contradiction
	}

	// If only one candidate left, assign it (singleton elimination)
	if remaining == 1 && grid.values[row][col] == 0 {
		last_digit := get_first_candidate(grid.candidates[row][col])
		if !assign(mut grid, row, col, last_digit) {
			return false // Assignment caused contradiction
		}
	}

	return true
}

// Assign a digit to a cell
fn assign(mut grid Grid, row int, col int, digit int) bool {
	// Increment iteration counter (this is our benchmark metric)
	unsafe {
		cp_iterations++
	}

	// Set value
	grid.values[row][col] = digit
	grid.candidates[row][col] = (1 << digit)

	// Eliminate digit from all peers
	peers := get_peers(row, col)

	for peer in peers {
		peer_row := peer[0]
		peer_col := peer[1]

		if !eliminate(mut grid, peer_row, peer_col, digit) {
			return false // Contradiction in peer elimination
		}
	}

	return true
}

// Propagate constraints
fn propagate(mut grid Grid) bool {
	mut changed := true

	for changed {
		changed = false

		// Strategy 1: Singleton elimination
		// If a cell has only one candidate, assign it
		for row in 0 .. 9 {
			for col in 0 .. 9 {
				if grid.values[row][col] == 0 {
					num_candidates := count_candidates(grid.candidates[row][col])
					if num_candidates == 0 {
						return false // Contradiction
					}
					if num_candidates == 1 {
						digit := get_first_candidate(grid.candidates[row][col])
						if !assign(mut grid, row, col, digit) {
							return false // Assignment caused contradiction
						}
						changed = true
					}
				}
			}
		}

		// Strategy 2: Hidden singles - Rows
		for row in 0 .. 9 {
			for digit in 1 .. 10 {
				mut count := 0
				mut last_col := -1
				mut already_assigned := false

				for col in 0 .. 9 {
					if grid.values[row][col] == digit {
						already_assigned = true
						break
					}
					if has_candidate(grid.candidates[row][col], digit) {
						count++
						last_col = col
					}
				}

				if !already_assigned {
					if count == 1 {
						if !assign(mut grid, row, last_col, digit) {
							return false
						}
						changed = true
					} else if count == 0 {
						return false // Digit cannot be placed anywhere in row
					}
				}
			}
		}

		// Strategy 2: Hidden singles - Columns
		for col in 0 .. 9 {
			for digit in 1 .. 10 {
				mut count := 0
				mut last_row := -1
				mut already_assigned := false

				for row in 0 .. 9 {
					if grid.values[row][col] == digit {
						already_assigned = true
						break
					}
					if has_candidate(grid.candidates[row][col], digit) {
						count++
						last_row = row
					}
				}

				if !already_assigned {
					if count == 1 {
						if !assign(mut grid, last_row, col, digit) {
							return false
						}
						changed = true
					} else if count == 0 {
						return false // Digit cannot be placed anywhere in column
					}
				}
			}
		}

		// Strategy 2: Hidden singles - Boxes
		for box in 0 .. 9 {
			box_row := (box / 3) * 3
			box_col := (box % 3) * 3

			for digit in 1 .. 10 {
				mut count := 0
				mut last_r := -1
				mut last_c := -1
				mut already_assigned := false

				for r in box_row .. box_row + 3 {
					for c in box_col .. box_col + 3 {
						if grid.values[r][c] == digit {
							already_assigned = true
							break
						}
						if has_candidate(grid.candidates[r][c], digit) {
							count++
							last_r = r
							last_c = c
						}
					}
					if already_assigned {
						break
					}
				}

				if !already_assigned {
					if count == 1 {
						if !assign(mut grid, last_r, last_c, digit) {
							return false
						}
						changed = true
					} else if count == 0 {
						return false // Digit cannot be placed anywhere in box
					}
				}
			}
		}
	}

	return true // Success - reached fixpoint
}

// Find cell with minimum remaining values (MRV heuristic)
fn find_mrv_cell(grid &Grid) (int, int, bool) {
	mut min_candidates := 10 // More than 9, so any cell will be smaller
	mut best_row := -1
	mut best_col := -1

	for r in 0 .. 9 {
		for c in 0 .. 9 {
			if grid.values[r][c] == 0 {
				num_candidates := count_candidates(grid.candidates[r][c])
				if num_candidates < min_candidates {
					min_candidates = num_candidates
					best_row = r
					best_col = c
				}
			}
		}
	}

	if best_row == -1 {
		return -1, -1, false // No empty cells (grid complete)
	}
	return best_row, best_col, true
}

// Search with backtracking
fn cp_search(mut grid Grid) bool {
	// Base case: check if grid is complete
	mrv_row, mrv_col, found := find_mrv_cell(&grid)
	if !found {
		return true // Grid is complete
	}

	// Recursive case: try each candidate for the MRV cell
	candidates := grid.candidates[mrv_row][mrv_col]

	for digit in 1 .. 10 {
		if has_candidate(candidates, digit) {
			// Save grid state for backtracking
			mut grid_copy := Grid{
				values: grid.values
				candidates: grid.candidates
			}

			// Try assigning this digit
			if assign(mut grid, mrv_row, mrv_col, digit) {
				// Assignment succeeded, propagate constraints
				if propagate(mut grid) {
					// Propagation succeeded, recurse
					if cp_search(mut grid) {
						return true // Found solution
					}
				}
			}

			// Failed - restore grid state and try next candidate
			grid = grid_copy
		}
	}

	// All candidates exhausted - dead end
	return false
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

// Print puzzle (dynamic array version)
fn print_puzzle_dyn(puzzle [][]int) {
	println('\nPuzzle:')
	for row in 0 .. 9 {
		for col in 0 .. 9 {
			print('${puzzle[row][col]} ')
		}
		println('')
	}
}

// Print puzzle (fixed array version)
fn print_puzzle_fixed(puzzle [9][9]int) {
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

		print_puzzle_dyn(puzzle)

		// Initialize grid
		mut grid := init_grid(puzzle)

		// Solve
		unsafe {
			cp_iterations = 0
		}
		if cp_search(mut grid) {
			print_puzzle_fixed(grid.values)
			unsafe {
				println('\nSolved in Iterations=${cp_iterations}\n')
			}
		} else {
			println('No solution found')
		}
	}

	elapsed := f64(sw.elapsed().microseconds()) / 1000000.0
	println('Seconds to process ${elapsed:.3}')
}
