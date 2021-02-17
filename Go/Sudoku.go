package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strings"
	"time"
)

func readMatrixFile(filename string) {
	file, err := os.Open(filename)
	if err != nil {
		os.Exit(2)
		//return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var j = 0
	for scanner.Scan() {
		line := scanner.Text()
		if !strings.HasPrefix(line, "#") {
			_, err := fmt.Sscan(line, &puzzle[j][0], &puzzle[j][1],
				&puzzle[j][2], &puzzle[j][3], &puzzle[j][4], &puzzle[j][5],
				&puzzle[j][6], &puzzle[j][7], &puzzle[j][8])

			if err != nil {
				fmt.Printf("Error %s\n", err)
				os.Exit(2)
			}
			j++
		}
	}
}

func printMatrix() {
	fmt.Printf("\nPuzzle:\n")
	for j := 0; j < 9; j++ { // start of the execution block
		for i := 0; i < 9; i++ { // start of the execution block
			fmt.Print(puzzle[j][i], " ")
		}
		fmt.Println("")
	}
}

func isPossible(y int, x int, val int) bool {
	if DEBUG > 0 {
		fmt.Printf("Is possible %o, %o, %o\n", y, x, val)
	}
	// Find if a matching number (val) already exists
	// in the same row (y) or column (x) or within its rectangle
	for i := 0; i < 9; i++ {
		if puzzle[i][x] == val {
			return false
		}
	}
	for i := 0; i < 9; i++ {
		if puzzle[y][i] == val {
			return false
		}
	}

	// Search the Rectangle containing x & y
	// Find which 3x3 square we are in using the floor quotient
	x0 := int(math.Floor(float64(x/3)) * 3)
	y0 := int(math.Floor(float64(y/3)) * 3)

	for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			if DEBUG > 2 {
				fmt.Printf("Is Possible: y0+i=%o i=%o, x0+j=%o j=%o Puzzle[y0+i][x0+j]=%o, val=%o\n", y0+i, i, x0+j, j, puzzle[y0+i][x0+j], val)
			}
			if puzzle[y0+i][x0+j] == val {
				return false
			}
		}
	}
	if DEBUG > 0 {
		fmt.Printf("YES possible\n")
	}
	return true
}

func solve() int {
	for j := 0; j < 9; j++ {
		for i := 0; i < 9; i++ {
			if puzzle[j][i] == 0 {
				if DEBUG > 0 {
					fmt.Printf("Solve: j=%o,i=%o: %o\n", j, i, puzzle[j][i])
				}
				for val := 1; val < 10; val++ {
					count++
					if DEBUG > 0 {
						fmt.Printf("Count= %v\n", count)
					}
					if isPossible(j, i, val) {
						puzzle[j][i] = val
						if solve() == 2 {
							return 2 //Makes sure to do a quick exit when solution was found
						}
						puzzle[j][i] = 0
					}
				}
				return 0
			}
		}
	}
	printMatrix()
	fmt.Printf("\nSolved in Iterations=%v\n\n", count)
	return 2
}

var count uint64 = 0
var puzzle [9][9]int
var DEBUG int = 0

func main() {
	start := time.Now()
	for _, arg := range os.Args[1:] {
		fmt.Printf("Arg: %v\n", arg)
		if strings.HasSuffix(arg, ".matrix") {
			readMatrixFile(arg)
			printMatrix()
			count = 0
			solve()
		}
	}
	fmt.Printf("Seconds to process %.3f\n", float64((time.Now().UnixNano()-start.UnixNano())/1000000000))
}
