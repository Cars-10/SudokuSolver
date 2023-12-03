// Importing necessary modules
use std::{fs, env, fmt};

// Step 1: Reading the Sudoku board from a file
fn read_board_from_file(file_path: &str) -> Result<Vec<u8>, &'static str> {
    let mut board = vec![];
    let file_content = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(_) => return Err("Failed to read file"),
    };

    for line in file_content.lines() {
        if line.starts_with('#') {
            continue;
// Ignoring comments
        }
        for char in line.split_whitespace() {
            match char.parse::<u8>() {
                Ok(num) if num >= 0 && num <= 9 => board.push(num),
                _ => return Err("Invalid character in file"),
            }
        }
    }

    if board.len() != 81 {
        return Err("Invalid board size");
    }

    Ok(board)
}

// Step 2: Calculating the complexity of the Sudoku board
fn calculate_complexity(board: &[u8]) -> u32 {
    board.iter().filter(|&&cell| cell == 0).count() as u32
}

// Step 3: Printing the Sudoku board
fn print_board(board: &[u8]) {
    for (index, &value) in board.iter().enumerate() {
        if index % 9 == 0 {
            println!("");
        }
        print!("{} ", value);
    }
    println!("");
}

// Step 4: Solving the Sudoku board
fn solve_board(board: &mut Vec<u8>) -> (bool, u32) {
    let mut iterations: u32 = 0;

    fn is_valid(board: &[u8], row: usize, col: usize, num: u8) -> bool {
        // Check the row and column
        for i in 0..9 {
            if board[row * 9 + i] == num || board[i * 9 + col] == num {
                return false;
            }
        }

        // Check the 3x3 square
        let start_row = row - row % 3;
        let start_col = col - col % 3;
        for r in start_row..start_row + 3 {
            for c in start_col..start_col + 3 {
                if board[r * 9 + c] == num {
                    return false;
                }
            }
        }

        true
    }

    fn solve(board: &mut [u8], iterations: &mut u32) -> bool {
        for row in 0..9 {
            for col in 0..9 {
                if board[row * 9 + col] == 0 {
                    for num in 1..=9 {
                        if is_valid(board, row, col, num) {
                            board[row * 9 + col] = num;

                            if solve(board, iterations) {
                                return true;
                            } else {
                                board[row * 9 + col] = 0;
                            }
                        }
                    }
                    return false;
                }
            }
        }
        true
    }

    let solved = solve(board, &mut iterations);
    (solved, iterations)
}

// Main function to run the solver
fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return;
    }

    let file_path = &args[1];
    let mut board = match read_board_from_file(file_path) {
        Ok(board) => board,
        Err(error) => {
            eprintln!("Error: {}", error);
            return;
        }
    };

    println!("Unsolved Sudoku board:");
    print_board(&board);
    let complexity = calculate_complexity(&board);
    println!("Complexity of the board: {}", complexity);

    let (solved, iterations) = solve_board(&mut board);
    if solved {
        println!("Solved Sudoku board:");
        print_board(&board);
        println!("Solved in {} iterations", iterations);
    } else {
        println!("No solution found");
    }
}

// Implementing Display trait to format the number of iterations with commas
impl fmt::Display for u32 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut num = self.to_string();
        let mut offset = 0;
        if num.len() > 3 {
            for i in (1..=num.len()/3).rev() {
                let index = num.len() - 3 * i - offset;
                if index > 0 {
                    num.insert_str(index, ",");
                    offset += 1;
                }
            }
        }
        write!(f, "{}", num)
    }
}
