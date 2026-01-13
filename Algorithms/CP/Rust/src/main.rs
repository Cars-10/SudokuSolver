use std::env;
use std::fs;
use std::time::Instant;

type CandidateSet = u16;

// Macros for bitset operations
macro_rules! has_candidate {
    ($cs:expr, $d:expr) => {
        ($cs & (1 << $d)) != 0
    };
}

macro_rules! remove_candidate {
    ($cs:expr, $d:expr) => {
        $cs &= !(1 << $d)
    };
}

#[derive(Clone)]
struct CPGrid {
    values: [[i32; 9]; 9],
    candidates: [[CandidateSet; 9]; 9],
}

impl CPGrid {
    fn new() -> Self {
        CPGrid {
            values: [[0; 9]; 9],
            candidates: [[0; 9]; 9],
        }
    }
}

struct CPSolver {
    iterations: i64,
}

impl CPSolver {
    fn new() -> Self {
        CPSolver { iterations: 0 }
    }

    fn count_candidates(cs: CandidateSet) -> usize {
        let mut count = 0;
        for digit in 1..=9 {
            if has_candidate!(cs, digit) {
                count += 1;
            }
        }
        count
    }

    fn get_first_candidate(cs: CandidateSet) -> Option<i32> {
        for digit in 1..=9 {
            if has_candidate!(cs, digit) {
                return Some(digit);
            }
        }
        None
    }

    fn get_peers(row: usize, col: usize) -> Vec<(usize, usize)> {
        let mut peers = Vec::with_capacity(20);

        // Same row (8 cells)
        for c in 0..9 {
            if c != col {
                peers.push((row, c));
            }
        }

        // Same column (8 cells)
        for r in 0..9 {
            if r != row {
                peers.push((r, col));
            }
        }

        // Same 3x3 box (4 cells)
        let box_row = (row / 3) * 3;
        let box_col = (col / 3) * 3;
        for r in box_row..box_row + 3 {
            for c in box_col..box_col + 3 {
                if r != row && c != col {
                    peers.push((r, c));
                }
            }
        }

        peers
    }

    fn init_grid(puzzle: &[[i32; 9]; 9]) -> CPGrid {
        let mut grid = CPGrid::new();

        for row in 0..9 {
            for col in 0..9 {
                if puzzle[row][col] == 0 {
                    // Empty cell: set all candidates 1-9 (bits 1-9 set)
                    grid.values[row][col] = 0;
                    grid.candidates[row][col] = 0x3FE; // Binary: 0011 1111 1110 (bits 1-9)
                } else {
                    // Given clue: set single value
                    let digit = puzzle[row][col];
                    grid.values[row][col] = digit;
                    grid.candidates[row][col] = 1 << digit;
                }
            }
        }

        grid
    }

    fn eliminate(&mut self, grid: &mut CPGrid, row: usize, col: usize, digit: i32) -> bool {
        // Check if digit is already eliminated
        if !has_candidate!(grid.candidates[row][col], digit) {
            return true; // Already eliminated
        }

        // Remove digit from candidates
        remove_candidate!(grid.candidates[row][col], digit);

        // Check for contradiction (no candidates left)
        let remaining = Self::count_candidates(grid.candidates[row][col]);
        if remaining == 0 {
            return false; // Contradiction
        }

        // If only one candidate left, assign it (singleton elimination)
        if remaining == 1 && grid.values[row][col] == 0 {
            if let Some(last_digit) = Self::get_first_candidate(grid.candidates[row][col]) {
                if !self.assign(grid, row, col, last_digit) {
                    return false; // Assignment caused contradiction
                }
            }
        }

        true
    }

    fn assign(&mut self, grid: &mut CPGrid, row: usize, col: usize, digit: i32) -> bool {
        // Increment iteration counter (this is our benchmark metric)
        self.iterations += 1;

        // Set value
        grid.values[row][col] = digit;
        grid.candidates[row][col] = 1 << digit;

        // Eliminate digit from all peers
        let peers = Self::get_peers(row, col);

        for (peer_row, peer_col) in peers {
            if !self.eliminate(grid, peer_row, peer_col, digit) {
                return false; // Contradiction in peer elimination
            }
        }

        true
    }

    fn propagate(&mut self, grid: &mut CPGrid) -> bool {
        let mut changed = true;

        while changed {
            changed = false;

            // Strategy 1: Singleton elimination
            // If a cell has only one candidate, assign it
            for row in 0..9 {
                for col in 0..9 {
                    if grid.values[row][col] == 0 {
                        let num_candidates = Self::count_candidates(grid.candidates[row][col]);
                        if num_candidates == 0 {
                            return false; // Contradiction
                        }
                        if num_candidates == 1 {
                            if let Some(digit) = Self::get_first_candidate(grid.candidates[row][col]) {
                                if !self.assign(grid, row, col, digit) {
                                    return false; // Assignment caused contradiction
                                }
                                changed = true;
                            }
                        }
                    }
                }
            }

            // Strategy 2: Hidden singles
            // For each unit (row, col, box), if a digit appears in only one cell, assign it

            // Check rows
            for row in 0..9 {
                for digit in 1..=9 {
                    let mut count = 0;
                    let mut last_col = 0;
                    let mut already_assigned = false;

                    for col in 0..9 {
                        if grid.values[row][col] == digit {
                            already_assigned = true;
                            break;
                        }
                        if has_candidate!(grid.candidates[row][col], digit) {
                            count += 1;
                            last_col = col;
                        }
                    }

                    if !already_assigned {
                        if count == 1 {
                            if !self.assign(grid, row, last_col, digit) {
                                return false;
                            }
                            changed = true;
                        } else if count == 0 {
                            return false; // Digit cannot be placed anywhere in row
                        }
                    }
                }
            }

            // Check columns
            for col in 0..9 {
                for digit in 1..=9 {
                    let mut count = 0;
                    let mut last_row = 0;
                    let mut already_assigned = false;

                    for row in 0..9 {
                        if grid.values[row][col] == digit {
                            already_assigned = true;
                            break;
                        }
                        if has_candidate!(grid.candidates[row][col], digit) {
                            count += 1;
                            last_row = row;
                        }
                    }

                    if !already_assigned {
                        if count == 1 {
                            if !self.assign(grid, last_row, col, digit) {
                                return false;
                            }
                            changed = true;
                        } else if count == 0 {
                            return false; // Digit cannot be placed anywhere in column
                        }
                    }
                }
            }

            // Check boxes
            for box_num in 0..9 {
                let box_row = (box_num / 3) * 3;
                let box_col = (box_num % 3) * 3;

                for digit in 1..=9 {
                    let mut count = 0;
                    let mut last_r = 0;
                    let mut last_c = 0;
                    let mut already_assigned = false;

                    for r in box_row..box_row + 3 {
                        for c in box_col..box_col + 3 {
                            if grid.values[r][c] == digit {
                                already_assigned = true;
                                break;
                            }
                            if has_candidate!(grid.candidates[r][c], digit) {
                                count += 1;
                                last_r = r;
                                last_c = c;
                            }
                        }
                        if already_assigned {
                            break;
                        }
                    }

                    if !already_assigned {
                        if count == 1 {
                            if !self.assign(grid, last_r, last_c, digit) {
                                return false;
                            }
                            changed = true;
                        } else if count == 0 {
                            return false; // Digit cannot be placed anywhere in box
                        }
                    }
                }
            }
        }

        true // Success - reached fixpoint
    }

    fn find_mrv_cell(grid: &CPGrid) -> Option<(usize, usize)> {
        let mut min_candidates = 10; // More than 9
        let mut result = None;

        for row in 0..9 {
            for col in 0..9 {
                if grid.values[row][col] == 0 {
                    let num_candidates = Self::count_candidates(grid.candidates[row][col]);
                    if num_candidates < min_candidates {
                        min_candidates = num_candidates;
                        result = Some((row, col));
                    }
                }
            }
        }

        result
    }

    fn search(&mut self, grid: &mut CPGrid) -> bool {
        // Base case: check if grid is complete
        let mrv_cell = match Self::find_mrv_cell(grid) {
            Some(cell) => cell,
            None => return true, // No empty cells - grid is complete
        };

        let (mrv_row, mrv_col) = mrv_cell;

        // Recursive case: try each candidate for the MRV cell
        let candidates = grid.candidates[mrv_row][mrv_col];

        for digit in 1..=9 {
            if has_candidate!(candidates, digit) {
                // Save grid state for backtracking
                let grid_copy = grid.clone();

                // Try assigning this digit
                if self.assign(grid, mrv_row, mrv_col, digit) {
                    // Assignment succeeded, propagate constraints
                    if self.propagate(grid) {
                        // Propagation succeeded, recurse
                        if self.search(grid) {
                            return true; // Found solution
                        }
                    }
                }

                // Failed - restore grid state and try next candidate
                *grid = grid_copy;
            }
        }

        // All candidates exhausted - dead end
        false
    }
}

fn read_puzzle(filename: &str) -> [[i32; 9]; 9] {
    let contents = fs::read_to_string(filename).expect("Failed to read file");

    let mut puzzle = [[0; 9]; 9];
    let mut row_num = 0;

    println!("{}", filename);
    for line in contents.lines() {
        if line.starts_with('#') || line.trim().is_empty() {
            continue;
        }

        let values: Vec<i32> = line
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();

        if values.len() == 9 && row_num < 9 {
            // Print row as we read it
            for &val in &values {
                print!("{} ", val);
            }
            println!();

            for col in 0..9 {
                puzzle[row_num][col] = values[col];
            }
            row_num += 1;
        }
    }

    puzzle
}

fn print_puzzle(grid: &CPGrid) {
    println!("\nPuzzle:");
    for row in 0..9 {
        for col in 0..9 {
            print!("{} ", grid.values[row][col]);
        }
        println!();
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <matrix_file>", args[0]);
        return;
    }

    let start = Instant::now();

    let puzzle = read_puzzle(&args[1]);

    let mut solver = CPSolver::new();
    let mut grid = CPSolver::init_grid(&puzzle);

    print_puzzle(&grid);

    if solver.search(&mut grid) {
        print_puzzle(&grid);
        println!("\nSolved in Iterations={}\n", solver.iterations);
    } else {
        println!("No solution found");
    }

    let elapsed = start.elapsed();
    println!("Seconds to process {:.3}", elapsed.as_secs_f64());
}
