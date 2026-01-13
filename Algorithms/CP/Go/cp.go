package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

// CandidateSet is a bitset for digits 1-9 (bit 0 unused)
type CandidateSet uint16

// CPGrid represents the constraint propagation grid
type CPGrid struct {
	values     [9][9]int
	candidates [9][9]CandidateSet
}

var cpIterations int64

// hasCand checks if a digit is in the candidate set
func hasCand(cs CandidateSet, digit int) bool {
	return (cs & (1 << digit)) != 0
}

// countCand counts the number of set bits in a candidate set
func countCand(cs CandidateSet) int {
	count := 0
	for i := 1; i <= 9; i++ {
		if hasCand(cs, i) {
			count++
		}
	}
	return count
}

// getFirstCandidate returns the first candidate digit (1-9)
func getFirstCandidate(cs CandidateSet) int {
	for digit := 1; digit <= 9; digit++ {
		if hasCand(cs, digit) {
			return digit
		}
	}
	return 0
}

// getPeers returns the 20 peer coordinates for a cell
func getPeers(row, col int) [][2]int {
	peers := make([][2]int, 0, 20)

	// Same row (8 cells)
	for c := 0; c < 9; c++ {
		if c != col {
			peers = append(peers, [2]int{row, c})
		}
	}

	// Same column (8 cells)
	for r := 0; r < 9; r++ {
		if r != row {
			peers = append(peers, [2]int{r, col})
		}
	}

	// Same 3x3 box (4 cells, excluding already counted)
	boxRow := (row / 3) * 3
	boxCol := (col / 3) * 3
	for r := boxRow; r < boxRow+3; r++ {
		for c := boxCol; c < boxCol+3; c++ {
			if r != row && c != col {
				peers = append(peers, [2]int{r, c})
			}
		}
	}

	return peers
}

// initGrid initializes the CP grid from a puzzle
func initGrid(puzzle [9][9]int) *CPGrid {
	grid := &CPGrid{}

	for row := 0; row < 9; row++ {
		for col := 0; col < 9; col++ {
			if puzzle[row][col] == 0 {
				// Empty cell: set all candidates 1-9 (bits 1-9 set)
				grid.values[row][col] = 0
				grid.candidates[row][col] = 0x3FE // Binary: 0011 1111 1110
			} else {
				// Given clue: set single value
				digit := puzzle[row][col]
				grid.values[row][col] = digit
				grid.candidates[row][col] = 1 << digit
			}
		}
	}

	return grid
}

// eliminate removes a digit from a cell's candidates
func eliminate(grid *CPGrid, row, col, digit int) bool {
	// Check if digit is already eliminated
	if !hasCand(grid.candidates[row][col], digit) {
		return true // Already eliminated, no change
	}

	// Remove digit from candidates
	grid.candidates[row][col] &= ^(1 << digit)

	// Check for contradiction (no candidates left)
	remaining := countCand(grid.candidates[row][col])
	if remaining == 0 {
		return false // Contradiction
	}

	// If only one candidate left, assign it (singleton elimination)
	if remaining == 1 && grid.values[row][col] == 0 {
		lastDigit := getFirstCandidate(grid.candidates[row][col])
		if !assign(grid, row, col, lastDigit) {
			return false // Assignment caused contradiction
		}
	}

	return true
}

// assign assigns a digit to a cell and propagates constraints
func assign(grid *CPGrid, row, col, digit int) bool {
	// Increment iteration counter (this is our benchmark metric)
	cpIterations++

	// Set value
	grid.values[row][col] = digit
	grid.candidates[row][col] = 1 << digit

	// Eliminate digit from all peers
	peers := getPeers(row, col)
	for _, peer := range peers {
		peerRow := peer[0]
		peerCol := peer[1]

		if !eliminate(grid, peerRow, peerCol, digit) {
			return false // Contradiction in peer elimination
		}
	}

	return true
}

// propagate applies constraint propagation until fixpoint
func propagate(grid *CPGrid) bool {
	changed := true

	for changed {
		changed = false

		// Strategy 1: Singleton elimination
		// If a cell has only one candidate, assign it
		for row := 0; row < 9; row++ {
			for col := 0; col < 9; col++ {
				if grid.values[row][col] == 0 {
					numCandidates := countCand(grid.candidates[row][col])
					if numCandidates == 0 {
						return false // Contradiction
					}
					if numCandidates == 1 {
						digit := getFirstCandidate(grid.candidates[row][col])
						if !assign(grid, row, col, digit) {
							return false // Assignment caused contradiction
						}
						changed = true
					}
				}
			}
		}

		// Strategy 2: Hidden singles - Check rows
		for row := 0; row < 9; row++ {
			for digit := 1; digit <= 9; digit++ {
				count := 0
				lastCol := -1
				alreadyAssigned := false

				for col := 0; col < 9; col++ {
					if grid.values[row][col] == digit {
						alreadyAssigned = true
						break
					}
					if hasCand(grid.candidates[row][col], digit) {
						count++
						lastCol = col
					}
				}

				if !alreadyAssigned {
					if count == 1 {
						if !assign(grid, row, lastCol, digit) {
							return false
						}
						changed = true
					} else if count == 0 {
						return false // Digit cannot be placed anywhere in row
					}
				}
			}
		}

		// Strategy 2: Hidden singles - Check columns
		for col := 0; col < 9; col++ {
			for digit := 1; digit <= 9; digit++ {
				count := 0
				lastRow := -1
				alreadyAssigned := false

				for row := 0; row < 9; row++ {
					if grid.values[row][col] == digit {
						alreadyAssigned = true
						break
					}
					if hasCand(grid.candidates[row][col], digit) {
						count++
						lastRow = row
					}
				}

				if !alreadyAssigned {
					if count == 1 {
						if !assign(grid, lastRow, col, digit) {
							return false
						}
						changed = true
					} else if count == 0 {
						return false // Digit cannot be placed anywhere in column
					}
				}
			}
		}

		// Strategy 2: Hidden singles - Check boxes
		for box := 0; box < 9; box++ {
			boxRow := (box / 3) * 3
			boxCol := (box % 3) * 3

			for digit := 1; digit <= 9; digit++ {
				count := 0
				lastR := -1
				lastC := -1
				alreadyAssigned := false

				for r := boxRow; r < boxRow+3; r++ {
					for c := boxCol; c < boxCol+3; c++ {
						if grid.values[r][c] == digit {
							alreadyAssigned = true
							goto nextBoxDigit
						}
						if hasCand(grid.candidates[r][c], digit) {
							count++
							lastR = r
							lastC = c
						}
					}
				}

			nextBoxDigit:
				if !alreadyAssigned {
					if count == 1 {
						if !assign(grid, lastR, lastC, digit) {
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

// findMrvCell finds the cell with minimum remaining values
func findMrvCell(grid *CPGrid) (int, int, bool) {
	minCandidates := 10 // More than 9, so any cell will be smaller
	mrvRow := -1
	mrvCol := -1

	for r := 0; r < 9; r++ {
		for c := 0; c < 9; c++ {
			if grid.values[r][c] == 0 {
				numCandidates := countCand(grid.candidates[r][c])
				if numCandidates < minCandidates {
					minCandidates = numCandidates
					mrvRow = r
					mrvCol = c
				}
			}
		}
	}

	return mrvRow, mrvCol, mrvRow >= 0
}

// copyGrid creates a deep copy of the grid
func copyGrid(grid *CPGrid) *CPGrid {
	newGrid := &CPGrid{}
	newGrid.values = grid.values
	newGrid.candidates = grid.candidates
	return newGrid
}

// cpSearch performs backtracking search with constraint propagation
func cpSearch(grid *CPGrid) (*CPGrid, bool) {
	// Base case: check if grid is complete
	mrvRow, mrvCol, found := findMrvCell(grid)
	if !found {
		// No empty cells - grid is complete
		return grid, true
	}

	// Recursive case: try each candidate for the MRV cell
	candidates := grid.candidates[mrvRow][mrvCol]

	for digit := 1; digit <= 9; digit++ {
		if hasCand(candidates, digit) {
			// Save grid state for backtracking
			gridCopy := copyGrid(grid)

			// Try assigning this digit
			if assign(gridCopy, mrvRow, mrvCol, digit) {
				// Assignment succeeded, propagate constraints
				if propagate(gridCopy) {
					// Propagation succeeded, recurse
					if result, ok := cpSearch(gridCopy); ok {
						return result, true // Found solution
					}
				}
			}
			// Failed - try next candidate (grid is already restored via copy)
		}
	}

	// All candidates exhausted - dead end
	return nil, false
}

// printPuzzle prints a 9x9 grid
func printPuzzle(grid [9][9]int) {
	fmt.Printf("\nPuzzle:\n")
	for r := 0; r < 9; r++ {
		for c := 0; c < 9; c++ {
			fmt.Printf("%d ", grid[r][c])
		}
		fmt.Println()
	}
}

// readMatrixFile reads a puzzle from a .matrix file
func readMatrixFile(filename string) ([9][9]int, error) {
	var puzzle [9][9]int

	file, err := os.Open(filename)
	if err != nil {
		return puzzle, err
	}
	defer file.Close()

	// Normalize path for output
	displayPath := filename
	if strings.HasPrefix(filename, "/app/Matrices/") {
		displayPath = "../" + filename[5:]
	}
	fmt.Println(displayPath)

	scanner := bufio.NewScanner(file)
	lineCount := 0

	for scanner.Scan() {
		line := scanner.Text()
		line = strings.TrimSpace(line)

		// Skip comments and empty lines
		if len(line) == 0 || strings.HasPrefix(line, "#") {
			continue
		}

		// Parse 9 integers
		fields := strings.Fields(line)
		if len(fields) != 9 {
			continue
		}

		for i := 0; i < 9; i++ {
			val, err := strconv.Atoi(fields[i])
			if err != nil {
				return puzzle, fmt.Errorf("invalid number: %s", fields[i])
			}
			puzzle[lineCount][i] = val
			fmt.Printf("%d ", val)
		}
		fmt.Println()

		lineCount++
		if lineCount >= 9 {
			break
		}
	}

	if lineCount != 9 {
		return puzzle, fmt.Errorf("expected 9 rows, got %d", lineCount)
	}

	return puzzle, scanner.Err()
}

func main() {
	start := time.Now()

	// Process each .matrix file from command line
	for _, arg := range os.Args[1:] {
		if !strings.HasSuffix(arg, ".matrix") {
			continue
		}

		// Read puzzle
		puzzle, err := readMatrixFile(arg)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", arg, err)
			continue
		}

		printPuzzle(puzzle)

		// Initialize CP grid
		grid := initGrid(puzzle)

		// Apply initial propagation
		if !propagate(grid) {
			fmt.Println("\nNo solution found (contradiction during initial propagation)")
			continue
		}

		// Run search
		cpIterations = 0
		resultGrid, solved := cpSearch(grid)

		if solved {
			printPuzzle(resultGrid.values)
			fmt.Printf("\nSolved in Iterations=%d\n\n", cpIterations)
		} else {
			fmt.Println("\nNo solution found")
		}
	}

	elapsed := time.Since(start)
	fmt.Printf("Seconds to process %.3f\n", elapsed.Seconds())
}
