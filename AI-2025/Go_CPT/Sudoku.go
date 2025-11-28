package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"time"
)

const (
	Size        = 9
	SubgridSize = 3
)

var solverCount int

func main() {
	start := time.Now()
	if len(os.Args) < 2 {
		fmt.Printf("Usage: %s input_file1.txt input_file2.txt ...\n", os.Args[0])
		os.Exit(1)
	}

	for i := 1; i < len(os.Args); i++ {
		fmt.Printf("\n%s:\n\n", os.Args[i])
		board := readSudokuFromFile(os.Args[i])
		printBoard(board)

		if solveSudoku(board) {
			printBoard(board)
			fmt.Printf("\nSolved in Iterations=%d\n\n", solverCount)
		} else {
			fmt.Printf("No solution found for %s\n\n", os.Args[i])
		}
	}

	fmt.Printf("Seconds to process %.3f\n", float64((time.Now().UnixNano()-start.UnixNano())/1000000000))

}

func solveSudoku(board [Size][Size]int) bool {
	solverCount++

	for row := 0; row < Size; row++ {
		for col := 0; col < Size; col++ {
			if board[row][col] == 0 {
				for num := 1; num <= Size; num++ {
					if isValidMove(board, row, col, num) {
						board[row][col] = num
						if solveSudoku(board) {
							return true
						}
						board[row][col] = 0 // Backtrack
					}
				}
				return false
			}
		}
	}
	return true
}

func isValidMove(board [Size][Size]int, row, col, num int) bool {
	// Check row, column, and subgrid for conflicts.
	for i := 0; i < Size; i++ {
		if board[row][i] == num || board[i][col] == num || board[row-row%SubgridSize+i/SubgridSize][col-col%SubgridSize+i%SubgridSize] == num {
			return false
		}
	}
	return true
}

func printBoard(board [Size][Size]int) {
	fmt.Println("\nPuzzle:")

	for i := 0; i < Size; i++ {
		for j := 0; j < Size; j++ {
			fmt.Printf("%d ", board[i][j])
		}
		fmt.Println()
	}
}

func readSudokuFromFile(filePath string) [Size][Size]int {
	var board [Size][Size]int
	file, err := os.Open(filePath)
	if err != nil {
		fmt.Printf("Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for i := 0; i < Size; i++ {
		if !scanner.Scan() {
			fmt.Printf("Error reading from file: %v\n", scanner.Err())
			os.Exit(1)
		}
		line := scanner.Text()

		// Ignore lines starting with #
		if len(line) > 0 && line[0] == '#' {
			i-- // Decrement the counter to read another line for this row.
			continue
		}

		for j := 0; j < Size; j++ {
			if line[j*2] != ' ' {
				val, err := strconv.Atoi(string(line[j*2]))
				if err != nil {
					fmt.Printf("Error parsing line: %v\n", err)
					os.Exit(1)
				}
				board[i][j] = val
			} else {
				board[i][j] = 0
			}
		}
	}

	if scanner.Err() != nil {
		fmt.Printf("Error reading from file: %v\n", scanner.Err())
		os.Exit(1)
	}

	return board
}
