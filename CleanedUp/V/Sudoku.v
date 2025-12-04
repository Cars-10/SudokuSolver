module main

import os
import strconv

struct Solver {
mut:
    grid [][]int
    iterations u64
}

fn (s Solver) print_grid() {
    for row in s.grid {
        for i, cell in row {
            print('$cell')
            if i < 8 {
                print(' ')
            }
        }
        println('')
    }
}

fn (s Solver) is_valid(row int, col int, num int) bool {
    for i in 0..9 {
        if s.grid[row][i] == num { return false }
        if s.grid[i][col] == num { return false }
    }

    start_row := row - (row % 3)
    start_col := col - (col % 3)
    for i in 0..3 {
        for j in 0..3 {
            if s.grid[start_row + i][start_col + j] == num { return false }
        }
    }
    return true
}

fn (mut s Solver) solve() bool {
    s.iterations++

    mut row := -1
    mut col := -1
    mut is_empty := false

    for r in 0..9 {
        for c in 0..9 {
            if s.grid[r][c] == 0 {
                row = r
                col = c
                is_empty = true
                break
            }
        }
        if is_empty { break }
    }

    if !is_empty { return true }

    for num in 1..10 {
        if s.is_valid(row, col, num) {
            s.grid[row][col] = num
            if s.solve() { return true }
            s.grid[row][col] = 0
        }
    }

    return false
}

fn main() {
    if os.args.len < 2 {
        println('Usage: $os.args[0] <matrix_file>')
        return
    }

    content := os.read_file(os.args[1]) or {
        println('Failed to read file')
        return
    }

    mut grid := [][]int{len: 9, init: []int{len: 9}}
    mut row := 0

    lines := content.split('\n')
    for line in lines {
        if line.len == 0 { continue }
        if line[0] < 48 || line[0] > 57 { continue }

        parts := line.split_any(' \t\r')
        mut col := 0
        for part in parts {
            if part.len == 0 { continue }
            if col < 9 {
                grid[row][col] = strconv.atoi(part) or { 0 }
                col++
            }
        }
        if col > 0 { row++ }
        if row >= 9 { break }
    }

    mut solver := Solver{
        grid: grid
        iterations: 0
    }

    if solver.solve() {
        solver.print_grid()
        println('Solved in Iterations=$solver.iterations')
    } else {
        println('No solution found.')
    }
}
