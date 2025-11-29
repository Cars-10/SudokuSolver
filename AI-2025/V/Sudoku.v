import os
import time

fn print_puzzle(puzzle [][]int) {
    println('\nPuzzle:')
    for row in puzzle {
        for val in row {
            print('$val ')
        }
        println('')
    }
}

fn read_matrix_file(filename string) [][]int {
    println(filename)
    mut puzzle := [][]int{len: 9, init: []int{len: 9}}
    lines := os.read_lines(filename) or { return puzzle }
    mut row := 0
    for line in lines {
        if line.starts_with('#') || line.trim_space() == '' { continue }
        parts := line.split_any(' \t')
        mut col := 0
        for part in parts {
            if part.trim_space() == '' { continue }
            puzzle[row][col] = part.int()
            col++
        }
        if col == 9 {
            row++
            if row == 9 { break }
        }
    }
    return puzzle
}

fn is_possible(puzzle [][]int, y int, x int, val int) bool {
    for i in 0 .. 9 {
        if puzzle[i][x] == val { return false }
        if puzzle[y][i] == val { return false }
    }
    
    x0 := (x / 3) * 3
    y0 := (y / 3) * 3
    
    for i in 0 .. 3 {
        for j in 0 .. 3 {
            if puzzle[y0 + i][x0 + j] == val { return false }
        }
    }
    return true
}

// Use a struct to hold count to pass by reference easily
struct SolverState {
mut:
    count i64
}

fn solve(mut puzzle [][]int, mut state SolverState) bool {
    for j in 0 .. 9 {
        for i in 0 .. 9 {
            if puzzle[j][i] == 0 {
                for val in 1 .. 10 {
                    state.count++
                    if is_possible(puzzle, j, i, val) {
                        puzzle[j][i] = val
                        if solve(mut puzzle, mut state) { return true }
                        puzzle[j][i] = 0
                    }
                }
                return false
            }
        }
    }
    print_puzzle(puzzle)
    println('\nSolved in Iterations=${state.count}\n')
    return true
}

fn main() {
    start := time.ticks()
    args := os.args
    for arg in args {
        if arg.ends_with('.matrix') {
            mut puzzle := read_matrix_file(arg)
            print_puzzle(puzzle)
            mut state := SolverState{count: 0}
            solve(mut puzzle, mut state)
        }
    }
    end := time.ticks()
    println('Seconds to process ${(f64(end - start) / 1000.0):.3f}')
}
