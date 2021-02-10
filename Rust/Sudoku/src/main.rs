#![allow(non_snake_case)]

use std::env;
use std::fs;
use num_integer::div_floor;
use std::borrow::BorrowMut;
use std::time::Instant;


const DEBUG: u8 = 0;

fn print_puzzle(puzzle: Vec<Vec<u8>>) {
    println!("\nPuzzle:");
    for j in 0..9 {
        for i in 0..9 {
            print!("{} ", puzzle[j][i]);
        }
        println!("");
    }
}

fn read_matrix_file(filename: &str, puzzle: &mut Vec<Vec<u8>>) {
    //let mut puzzle = vec![vec![0u8; 9];9];
    let mut line_num = 0;

    println!("{}", filename);
    let contents = fs::read_to_string(filename)
    .expect("Something went wrong reading the file");

    for line in contents.split("\n") {
        if line.starts_with('#') {}
        else {
            //println!("{:?}", line);
            let mut col = 0;
            for value in line.split_whitespace() {
                puzzle[line_num][col] = value.parse::<u8>().unwrap();
                //println!("{:?}", puzzle[line_num][col]);
                col +=1;
            }
            line_num +=1;
        }
    }
}


fn is_possible(puzzle: &Vec<Vec<u8>>, y: u8, x: u8, val: u8) -> bool {
    if DEBUG>1 {
        println!("Is possible {}, {}, {}" ,y ,x ,val);
    }
    // Find if a matching number (val) already exists
    // in the same row (y) or column (x) or within its rectangle
    for i in 0..9 {
        if puzzle[i as usize][x as usize]==val {return false;}
    }
    for i in 0..9 {
        if puzzle[y as usize][i as usize]==val {return false; }
    }
    
    // Search the Rectangle containing x & y
    // Find which 3x3 square we are in using the floor quotient
    let x0=(div_floor(x,3))*3;
    let y0=(div_floor(y,3))*3;
    if DEBUG>0 {
        println!("Is possible x={} x0={}, y={} y0={}, val={}" , x, x0, y, y0, val);
    }

    for i in 0..3 {
        for j in 0..3 {
            if DEBUG>1 {println!("y0+i={} i={}, x0+j={} j={} Puzzle[y0+i][x0+j]={}, val={}", y0+i,i, x0+j,j, puzzle[(y0+i) as usize][(x0+j) as usize] , val);}
            if puzzle[(y0+i) as usize][(x0+j) as usize] == val {return false;}
        }
    }
    if DEBUG>0 {println!("YES possible");}
    return true;
}

fn solve(puzzle: &mut Vec<Vec<u8>>, count: &mut u64) -> u8  {
    for j in 0..9 {
        for i in 0..9 {
            if puzzle[j][i] == 0 {
                if DEBUG>1 {println!("Solve: j={},i={}: {}" ,j,i,puzzle[j][i]);}
                for val in 1..10 {
                    *count.borrow_mut() += 1;
                    if DEBUG>0 {println!("Count= {}", count);}
                    if is_possible(puzzle, j as u8,i as u8,val as u8) {
                        puzzle[j][i] = val;
                        if solve(puzzle, count) == 2 {return 2;} //Makes sure to do a quick exit when solution was found
                        puzzle[j][i] = 0;
                    }
                }
                return 0;
            }
        }
    }
    print_puzzle(puzzle.to_vec());
    println!("\nSolved in Iterations={}\n", count);
    return 2;
}

fn main() {
    let start = Instant::now();
    let mut puzzle = vec![vec![0u8; 9];9];
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    for arg in args.iter() {
         if DEBUG>1 {println!("Main: {}", arg);}
         let split: Vec<&str> = arg.rsplit(".").collect();
         if split[0] == "matrix" {
             read_matrix_file(arg, &mut puzzle);
             print_puzzle(puzzle.to_vec());
             let mut count: u64 = 0;
             solve (&mut puzzle, &mut count);
         }
    }
    let elapsed = start.elapsed();
    println!("Seconds to process {}", elapsed.as_millis() as f64/1000.0); 
}
