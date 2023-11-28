package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Step 1: Read a 9x9 matrix from a file and return it as a 1D array
func readSudokuBoard(filePath string) ([]int, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	board := make([]int, 0, 81)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		// Ignore comments and empty lines
		if len(line) == 0 || strings.HasPrefix(line, "#") {
			continue
		}
		for _, char := range line {
			if char >= '0' && char <= '9' {
				num := int(char - '0')
				board = append(board, num)
			}
		}
	}
	if len(board) != 81 {
		return nil, fmt.Errorf("Invalid board size")
	}
	err = scanner.Err()
	return board, err
}

// Step 2: Calculate the complexity of a 9x9 matrix
func calculateComplexity(board []int) int {
	complexity := 0
	for _, num := range board {
		if num == 0 {
			complexity++
		}
	}
	return complexity
}

// Step 3: Print the board in a 9x9 grid and display complexity if desired
func printBoard(board []int, displayComplexity bool) {
	if displayComplexity {
		complexity := calculateComplexity(board)
		fmt.Printf("Complexity: %d\n", complexity)
	}
	for i := 0; i < 81; i++ {
		fmt.Printf("%d ", board[i])
		if (i+1)%9 == 0 {
			fmt.Println()
		}
	}
}

// Step 4: Solve the Sudoku board using a backtracking algorithm
func solveSudoku(board []int, iterations *int) bool {
	(*iterations)++
	for i := 0; i < 81; i++ {
		if board[i] == 0 {
			for num := 1; num <= 9; num++ {
				if isValid(board, i, num) {
					board[i] = num
					if solveSudoku(board, iterations) {
						return true
					}
					board[i] = 0
				}
			}
			return false
		}
	}
	return true
}

// Helper function to check if a number can be placed at a given position
func isValid(board []int, pos, num int) bool {
	row := pos / 9
	col := pos % 9

	// Check row
	for i := 0; i < 9; i++ {
		if board[row*9+i] == num {
			return false
		}
	}

	// Check column
	for i := 0; i < 9; i++ {
		if board[i*9+col] == num {
			return false
		}
	}

	// Check 3x3 square
	startRow := row / 3 * 3
	startCol := col / 3 * 3
	for i := startRow; i < startRow+3; i++ {
		for j := startCol; j < startCol+3; j++ {
			if board[i*9+j] == num {
				return false
			}
		}
	}

	return true
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: sudoku_solver <file_path>")
		return
	}
	board, err := readSudokuBoard(os.Args[1])
	if err != nil {
		fmt.Println("Error reading board:", err)
		return
	}

	printBoard(board, true)

	iterations := 0
	if solveSudoku(board, &iterations) {
		fmt.Printf("Solved board after %s iterations:\n", strconv.FormatInt(int64(iterations), 10))
		printBoard(board, false)
	} else {
		fmt.Println("Could not solve the Sudoku board.")
	}
}
