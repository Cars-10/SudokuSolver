package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const Size = 9

// Step 1: Read a 9x9 matrix from a file and return as a 1D array, ignoring comments.
func readSudokuFromFile(filePath string) ([]int, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var numbers []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "#") || line == "" { // Ignore comments and empty lines.
			continue
		}
		rowNumbers := strings.Fields(line)
		for _, num := range rowNumbers {
			if len(numbers) >= Size*Size { // Stop after 81 numbers.
				break
			}
			if n, err := strconv.Atoi(num); err == nil {
				numbers = append(numbers, n)
			}
		}
	}

	return numbers, scanner.Err()
}

// Step 2: Calculate the complexity of a 9x9 matrix.
func calculateComplexity(matrix []int) int {
	complexity := 0
	for _, num := range matrix {
		if num == 0 {
			complexity++
		}
	}
	return complexity
}

// Step 3: Print the board in a 9x9 grid, with optional complexity display.
func printBoard(matrix []int, showComplexity bool) {
	if showComplexity {
		fmt.Printf("Complexity: %d\n", calculateComplexity(matrix))
	}
	for i := 0; i < Size; i++ {
		for j := 0; j < Size; j++ {
			fmt.Printf("%d ", matrix[i*Size+j])
		}
		fmt.Println()
	}
}

// Step 4: Solve the Sudoku board using a backtracking algorithm.
func solveSudoku(matrix []int) (bool, int) {
	var iterations int
	var solve func(int) bool
	solve = func(index int) bool {
		iterations++
		if index == Size*Size {
			return true // Solved!
		}
		if matrix[index] != 0 { // Skip filled cells.
			return solve(index + 1)
		}
		for num := 1; num <= Size; num++ {
			if isValid(matrix, index, num) {
				matrix[index] = num
				if solve(index + 1) {
					return true
				}
				matrix[index] = 0 // Backtrack
			}
		}
		return false
	}
	if solve(0) {
		return true, iterations
	}
	return false, iterations
}

// Helper function to check if a number placement is valid.
func isValid(matrix []int, index, num int) bool {
	row := index / Size
	col := index % Size
	// Check row and column
	for i := 0; i < Size; i++ {
		if matrix[row*Size+i] == num || matrix[i*Size+col] == num {
			return false
		}
	}
	// Check 3x3 subgrid
	startRow := row - row%3
	startCol := col - col%3
	for i := startRow; i < startRow+3; i++ {
		for j := startCol; j < startCol+3; j++ {
			if matrix[i*Size+j] == num {
				return false
			}
		}
	}
	return true
}

// Step 5 & 6: Main function to tie everything together.
func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: sudoku_solver <file_path>")
		return
	}
	matrix, err := readSudokuFromFile(os.Args[1])
	if err != nil {
		fmt.Printf("Error reading file: %s\n", err)
		return
	}
	printBoard(matrix, false)
	fmt.Println("Solving Sudoku...")
	if solved, iterations := solveSudoku(matrix); solved {
		printBoard(matrix, false)
		fmt.Printf("Solved in %s iterations.\n", formatNumber(iterations))
	} else {
		fmt.Println("Could not solve sudoku.")
	}
}

// Helper function to format the number with commas.
func formatNumber(n int) string {
	in := strconv.Itoa(n)
	out := make([]byte, len(in)+(len(in)-1)/3)
	for e, i, j := len(in)-1, len(out)-1, 0; e >= 0; e-- {
		out[i] = in[e]
		i--
		j++
		if j%3 == 0 && e != 0 {
			out[i] = ','
			i--
		}
	}
	return string(out)
}
