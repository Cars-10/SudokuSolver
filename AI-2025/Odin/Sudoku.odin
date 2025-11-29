package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:time"

Puzzle :: [9][9]int
count: int

print_puzzle :: proc(puzzle: ^Puzzle) {
	fmt.println("\nPuzzle:")
	for r in 0..<9 {
		for c in 0..<9 {
			fmt.printf("%d ", puzzle[r][c])
		}
		fmt.println()
	}
}

read_matrix_file :: proc(filename: string) -> (Puzzle, bool) {
	fmt.println(filename)
	puzzle: Puzzle
	data, ok := os.read_entire_file(filename)
	if !ok {
		return puzzle, false
	}
	defer delete(data)

	content := string(data)
	lines := strings.split(content, "\n")
	defer delete(lines)

	row := 0
	for line in lines {
		trimmed := strings.trim_space(line)
		if len(trimmed) == 0 || strings.has_prefix(trimmed, "#") {
			continue
		}
		
		parts := strings.fields(trimmed)
		defer delete(parts)
		
		if len(parts) == 9 {
			for col in 0..<9 {
				val, _ := strconv.parse_int(parts[col])
				puzzle[row][col] = val
			}
			row += 1
			if row == 9 {
				break
			}
		}
	}
	return puzzle, true
}

is_possible :: proc(puzzle: ^Puzzle, r: int, c: int, val: int) -> bool {
	for i in 0..<9 {
		if puzzle[i][c] == val { return false }
		if puzzle[r][i] == val { return false }
	}

	r0 := (r / 3) * 3
	c0 := (c / 3) * 3

	for i in 0..<3 {
		for j in 0..<3 {
			if puzzle[r0 + i][c0 + j] == val { return false }
		}
	}
	return true
}

solve :: proc(puzzle: ^Puzzle) -> bool {
	for r in 0..<9 {
		for c in 0..<9 {
			if puzzle[r][c] == 0 {
				for val in 1..=9 {
					count += 1
					if is_possible(puzzle, r, c, val) {
						puzzle[r][c] = val
						if solve(puzzle) { return true }
						puzzle[r][c] = 0
					}
				}
				return false
			}
		}
	}
	print_puzzle(puzzle)
	fmt.printf("\nSolved in Iterations=%d\n\n", count)
	return true
}

main :: proc() {
	start := time.now()
	args := os.args
	for i in 1..<len(args) {
		arg := args[i]
		if strings.has_suffix(arg, ".matrix") {
			puzzle, ok := read_matrix_file(arg)
			if ok {
				print_puzzle(&puzzle)
				count = 0
				solve(&puzzle)
			}
		}
	}
	end := time.now()
	duration := time.diff(start, end)
	seconds := time.duration_seconds(duration)
	fmt.printf("Seconds to process %.3f\n", seconds)
}
