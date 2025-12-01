module main

import os
import time

__global (
    puzzle [9][9]int
    iterations int
)

fn print_board() {
    println('Puzzle:')
    for i in 0..9 {
        for j in 0..9 {
            print('${puzzle[i][j]} ')
        }
        println('')
    }
}

fn is_possible(row int, col int, num int) bool {
    for i in 0..9 {
        if puzzle[row][i] == num || puzzle[i][col] == num {
            return false
        }
    }

    start_row := (row / 3) * 3
    start_col := (col / 3) * 3
    for i in 0..3 {
        for j in 0..3 {
            if puzzle[start_row + i][start_col + j] == num {
                return false
            }
        }
    }
    return true
}

fn solve(row int, col int) bool {
    if row == 9 {
        return true
    }

    mut next_row := row
    mut next_col := col + 1
    if next_col == 9 {
        next_row = row + 1
        next_col = 0
    }

    if puzzle[row][col] != 0 {
        return solve(next_row, next_col)
    }

    for num in 1..10 {
        iterations++
        if is_possible(row, col, num) {
            puzzle[row][col] = num
            if solve(next_row, next_col) {
                return true
            }
            puzzle[row][col] = 0
        }
    }
    return false
}

fn read_board(filename string) bool {
    content := os.read_file(filename) or {
        println('Error reading file $filename')
        return false
    }
    
    lines := content.split_into_lines()
    mut row := 0
    for line in lines {
        trimmed := line.trim_space()
        if trimmed.len > 0 && !trimmed.starts_with('#') {
            parts := trimmed.split(' ')
            mut col := 0
            for part in parts {
                if part.len > 0 {
                    if col < 9 {
                        puzzle[row][col] = part.int()
                        col++
                    }
                }
            }
            row++
            if row == 9 {
                return true
            }
        }
    }
    return true
}

fn main() {
    if os.args.len < 2 {
        println('Usage: ./Sudoku <file1> <file2> ...')
        return
    }

    for i in 1..os.args.len {
        filename := os.args[i]
        println('\nProcessing $filename')
        if read_board(filename) {
            print_board()
            iterations = 0
            if solve(0, 0) {
                print_board()
                println('\nSolved in Iterations=$iterations')
            } else {
                println('No solution found')
            }
        }
    }
}
