module main

import os
import time

struct Solver {
mut:
	puzzle [9][9]int
	count  int
}

fn (s &Solver) is_valid(row int, col int, val int) bool {
	// Check row
	for i in 0 .. 9 {
		if s.puzzle[row][i] == val {
			return false
		}
	}

	// Check column
	for i in 0 .. 9 {
		if s.puzzle[i][col] == val {
			return false
		}
	}

	// Check 3x3 box
	br := (row / 3) * 3
	bc := (col / 3) * 3
	for i in 0 .. 3 {
		for j in 0 .. 3 {
			if s.puzzle[br + i][bc + j] == val {
				return false
			}
		}
	}
	return true
}

fn (s &Solver) print_puzzle() {
	println('\nPuzzle:')
	for row in 0 .. 9 {
		for col in 0 .. 9 {
			print('${s.puzzle[row][col]} ')
		}
		println('')
	}
}

fn (mut s Solver) solve() bool {
	// Find first empty cell (row-major order)
	mut row := -1
	mut col := -1

	for r in 0 .. 9 {
		for c in 0 .. 9 {
			if s.puzzle[r][c] == 0 {
				row = r
				col = c
				break
			}
		}
		if row >= 0 {
			break
		}
	}

	// If no empty cell found, puzzle is solved
	if row == -1 {
		return true
	}

	// Try values 1-9 in order
	for val in 1 .. 10 {
		s.count++ // COUNT BEFORE validity check - algorithm fingerprint
		if s.is_valid(row, col, val) {
			s.puzzle[row][col] = val

			if s.solve() {
				return true
			}

			s.puzzle[row][col] = 0 // Backtrack
		}
	}

	return false
}

fn (mut s Solver) read_matrix_file(filename string) bool {
	// Normalize path for output (convert absolute to relative)
	if filename.starts_with('/app/Matrices/') {
		display_path := filename[5..] // Skip "/app/" to get "Matrices/..."
		println('../${display_path}')
	} else {
		println(filename)
	}

	content := os.read_file(filename) or {
		eprintln('Error reading file: ${filename}')
		return false
	}

	lines := content.split('\n')
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
				s.puzzle[line_count][i] = values[i].int()
				print('${values[i]} ')
			}
			println('')
			line_count++
		}

		if line_count >= 9 {
			break
		}
	}

	return line_count == 9
}

fn main() {
	sw := time.new_stopwatch()

	args := os.args[1..]
	for filename in args {
		if !filename.ends_with('.matrix') {
			continue
		}

		mut solver := Solver{}

		if !solver.read_matrix_file(filename) {
			eprintln('Error reading ${filename}')
			continue
		}

		solver.print_puzzle()

		solver.count = 0
		if solver.solve() {
			solver.print_puzzle()
			println('\nSolved in Iterations=${solver.count}\n')
		} else {
			println('No solution found')
		}
	}

	elapsed := f64(sw.elapsed().microseconds()) / 1000000.0
	println('Seconds to process ${elapsed:.3}')
}
