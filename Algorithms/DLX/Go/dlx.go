package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

// DlxNode represents a node in the Dancing Links matrix
type DlxNode struct {
	left   *DlxNode
	right  *DlxNode
	up     *DlxNode
	down   *DlxNode
	column *DlxNode
	size   int
	rowId  int
	colId  int
}

// RowInfo stores metadata to map DLX rows back to Sudoku
type RowInfo struct {
	row int
	col int
	num int
}

var (
	dlxIterations int
	puzzle        [9][9]int
	solutionGrid  [9][9]int
	rowInfo       [729]RowInfo
	rowStarts     [729]*DlxNode
)

// coverColumn removes a column and its rows from the matrix
func coverColumn(c *DlxNode) {
	// Remove column header from the header list
	c.right.left = c.left
	c.left.right = c.right

	// For each row in this column
	rowNode := c.down
	for rowNode != c {
		// For each node in this row (excluding the column itself)
		rightNode := rowNode.right
		for rightNode != rowNode {
			// Remove this node from its column
			rightNode.down.up = rightNode.up
			rightNode.up.down = rightNode.down
			rightNode.column.size--
			rightNode = rightNode.right
		}
		rowNode = rowNode.down
	}
}

// uncoverColumn restores a column (exact reverse of cover)
func uncoverColumn(c *DlxNode) {
	// For each row in this column (in reverse order)
	rowNode := c.up
	for rowNode != c {
		// For each node in this row (in reverse order)
		leftNode := rowNode.left
		for leftNode != rowNode {
			// Restore this node to its column
			leftNode.column.size++
			leftNode.down.up = leftNode
			leftNode.up.down = leftNode
			leftNode = leftNode.left
		}
		rowNode = rowNode.up
	}

	// Restore column header to the header list
	c.right.left = c
	c.left.right = c
}

// chooseColumn selects the column with minimum size (Knuth's S heuristic)
func chooseColumn(root *DlxNode) *DlxNode {
	var best *DlxNode
	minSize := int(^uint(0) >> 1) // Max int

	colNode := root.right
	for colNode != root {
		if colNode.size < minSize {
			minSize = colNode.size
			best = colNode
		}
		colNode = colNode.right
	}

	return best
}

// dlxSearch implements Algorithm X with Dancing Links
func dlxSearch(root *DlxNode, k int, solution []int) bool {
	dlxIterations++ // Count every search call

	// If matrix is empty, we found a solution
	if root.right == root {
		return true
	}

	// Choose column with minimum size
	col := chooseColumn(root)

	// If column has no rows, no solution possible
	if col.size == 0 {
		return false
	}

	// Cover this column
	coverColumn(col)

	// Try each row in this column
	rowNode := col.down
	for rowNode != col {
		// Add row to partial solution
		solution[k] = rowNode.rowId

		// Cover all other columns in this row
		rightNode := rowNode.right
		for rightNode != rowNode {
			coverColumn(rightNode.column)
			rightNode = rightNode.right
		}

		// Recurse
		if dlxSearch(root, k+1, solution) {
			return true // Solution found
		}

		// Backtrack: uncover all columns in this row
		leftNode := rowNode.left
		for leftNode != rowNode {
			uncoverColumn(leftNode.column)
			leftNode = leftNode.left
		}

		rowNode = rowNode.down
	}

	// Uncover column
	uncoverColumn(col)

	return false // No solution found
}

// getPositionCol returns the column index for position constraint
func getPositionCol(r, c int) int {
	return r*9 + c
}

// getRowCol returns the column index for row constraint
func getRowCol(r, n int) int {
	return 81 + r*9 + (n - 1)
}

// getColCol returns the column index for column constraint
func getColCol(c, n int) int {
	return 162 + c*9 + (n - 1)
}

// getBoxCol returns the column index for box constraint
func getBoxCol(r, c, n int) int {
	box := (r/3)*3 + (c / 3)
	return 243 + box*9 + (n - 1)
}

// addNode adds a node to the DLX matrix
func addNode(col *DlxNode, rowId int) *DlxNode {
	node := &DlxNode{
		column: col,
		rowId:  rowId,
	}

	// Insert at end of column's circular list
	node.down = col
	node.up = col.up
	col.up.down = node
	col.up = node
	col.size++

	return node
}

// buildDlxRow creates a DLX row for Sudoku cell (r,c) with value n
func buildDlxRow(r, c, n, rowId int, columns []*DlxNode) {
	// Store row metadata
	rowInfo[rowId].row = r
	rowInfo[rowId].col = c
	rowInfo[rowId].num = n

	// Create nodes for the 4 constraints
	n1 := addNode(columns[getPositionCol(r, c)], rowId)
	n2 := addNode(columns[getRowCol(r, n)], rowId)
	n3 := addNode(columns[getColCol(c, n)], rowId)
	n4 := addNode(columns[getBoxCol(r, c, n)], rowId)

	// Link nodes horizontally in circular list
	n1.right = n2
	n2.right = n3
	n3.right = n4
	n4.right = n1

	n1.left = n4
	n2.left = n1
	n3.left = n2
	n4.left = n3

	// Store first node for this row
	rowStarts[rowId] = n1
}

// initDlxMatrix initializes the DLX matrix structure
func initDlxMatrix() (*DlxNode, []*DlxNode) {
	// Create root column
	root := &DlxNode{
		colId: -1,
		rowId: -1,
	}
	root.left = root
	root.right = root
	root.up = root
	root.down = root
	root.column = root

	// Create 324 column headers
	columns := make([]*DlxNode, 324)
	for i := 0; i < 324; i++ {
		columns[i] = &DlxNode{
			colId: i,
			rowId: -1,
			size:  0,
		}
		columns[i].up = columns[i]
		columns[i].down = columns[i]
		columns[i].column = columns[i]

		// Link into header list
		columns[i].left = root.left
		columns[i].right = root
		root.left.right = columns[i]
		root.left = columns[i]
	}

	return root, columns
}

// buildDlxMatrixFromPuzzle builds the complete DLX matrix from the puzzle
func buildDlxMatrixFromPuzzle(columns []*DlxNode) {
	rowId := 0

	for r := 0; r < 9; r++ {
		for c := 0; c < 9; c++ {
			if puzzle[r][c] != 0 {
				// Cell has a clue - create only one row for that value
				buildDlxRow(r, c, puzzle[r][c], rowId, columns)
				rowId++
			} else {
				// Cell is empty - create rows for all possible values
				for n := 1; n <= 9; n++ {
					buildDlxRow(r, c, n, rowId, columns)
					rowId++
				}
			}
		}
	}
}

// coverClues covers the columns for pre-filled clues
func coverClues(root *DlxNode) {
	for r := 0; r < 9; r++ {
		for c := 0; c < 9; c++ {
			if puzzle[r][c] != 0 {
				n := puzzle[r][c]

				// Find the row for this clue
				for rowId := 0; rowId < 729; rowId++ {
					if rowStarts[rowId] != nil &&
						rowInfo[rowId].row == r &&
						rowInfo[rowId].col == c &&
						rowInfo[rowId].num == n {

						// Cover all columns in this row
						node := rowStarts[rowId]
						curr := node
						for {
							coverColumn(curr.column)
							curr = curr.right
							if curr == node {
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

// extractSolution populates solution_grid from the DLX solution
func extractSolution(solution []int) {
	// Initialize with original puzzle (includes clues)
	solutionGrid = puzzle

	// Each solution entry is a row_id
	for _, rowId := range solution {
		if rowId >= 0 && rowId < 729 && rowInfo[rowId].num > 0 {
			solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num
		}
	}
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
func readMatrixFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
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
				return fmt.Errorf("invalid number: %s", fields[i])
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
		return fmt.Errorf("expected 9 rows, got %d", lineCount)
	}

	return scanner.Err()
}

func main() {
	start := time.Now()

	// Process each .matrix file from command line
	for _, arg := range os.Args[1:] {
		if !strings.HasSuffix(arg, ".matrix") {
			continue
		}

		// Read puzzle
		if err := readMatrixFile(arg); err != nil {
			fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", arg, err)
			continue
		}

		printPuzzle(puzzle)

		// Initialize DLX matrix
		root, columns := initDlxMatrix()

		// Build matrix from puzzle
		buildDlxMatrixFromPuzzle(columns)

		// Cover pre-filled clues
		coverClues(root)

		// Solve using DLX
		dlxIterations = 0
		solution := make([]int, 81)
		result := dlxSearch(root, 0, solution)

		if result {
			extractSolution(solution)
			printPuzzle(solutionGrid)
			fmt.Printf("\nSolved in Iterations=%d\n\n", dlxIterations)
		} else {
			fmt.Println("\nNo solution found")
		}
	}

	elapsed := time.Since(start)
	fmt.Printf("Seconds to process %.3f\n", elapsed.Seconds())
}
