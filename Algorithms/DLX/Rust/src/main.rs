use std::cell::RefCell;
use std::env;
use std::fs;
use std::rc::Rc;
use std::time::Instant;

type NodeRef = Rc<RefCell<DlxNode>>;

#[derive(Clone)]
struct DlxNode {
    left: Option<NodeRef>,
    right: Option<NodeRef>,
    up: Option<NodeRef>,
    down: Option<NodeRef>,
    column: Option<NodeRef>,
    size: usize,
    row_id: i32,
    col_id: i32,
}

impl DlxNode {
    fn new_header(col_id: i32) -> NodeRef {
        let node = Rc::new(RefCell::new(DlxNode {
            left: None,
            right: None,
            up: None,
            down: None,
            column: None,
            size: 0,
            row_id: -1,
            col_id,
        }));

        // Self-reference for circular structure
        node.borrow_mut().left = Some(Rc::clone(&node));
        node.borrow_mut().right = Some(Rc::clone(&node));
        node.borrow_mut().up = Some(Rc::clone(&node));
        node.borrow_mut().down = Some(Rc::clone(&node));
        node.borrow_mut().column = Some(Rc::clone(&node));

        node
    }

    fn new_regular(row_id: i32, col_id: i32, column: NodeRef) -> NodeRef {
        let node = Rc::new(RefCell::new(DlxNode {
            left: None,
            right: None,
            up: None,
            down: None,
            column: Some(Rc::clone(&column)),
            size: 0,
            row_id,
            col_id,
        }));

        node
    }
}

struct DlxSolver {
    root: NodeRef,
    columns: Vec<NodeRef>,
    iterations: i32,
}

impl DlxSolver {
    fn new(num_cols: usize) -> Self {
        let root = DlxNode::new_header(-1);
        let mut columns = Vec::new();

        // Create column headers
        for col_id in 0..num_cols {
            let col_header = DlxNode::new_header(col_id as i32);
            columns.push(Rc::clone(&col_header));

            // Link into header list
            let root_left = Rc::clone(root.borrow().left.as_ref().unwrap());
            root.borrow_mut().left = Some(Rc::clone(&col_header));
            col_header.borrow_mut().right = Some(Rc::clone(&root));
            col_header.borrow_mut().left = Some(Rc::clone(&root_left));
            root_left.borrow_mut().right = Some(Rc::clone(&col_header));
        }

        DlxSolver {
            root,
            columns,
            iterations: 0,
        }
    }

    fn add_row(&mut self, row_id: i32, cols: &[usize]) {
        if cols.is_empty() {
            return;
        }

        let mut row_nodes = Vec::new();

        // Create nodes for this row
        for &col_idx in cols {
            let col_header = Rc::clone(&self.columns[col_idx]);
            let node = DlxNode::new_regular(row_id, col_idx as i32, Rc::clone(&col_header));

            // Link vertically into column
            let col_up = Rc::clone(col_header.borrow().up.as_ref().unwrap());
            col_header.borrow_mut().up = Some(Rc::clone(&node));
            node.borrow_mut().down = Some(Rc::clone(&col_header));
            node.borrow_mut().up = Some(Rc::clone(&col_up));
            col_up.borrow_mut().down = Some(Rc::clone(&node));

            col_header.borrow_mut().size += 1;

            row_nodes.push(node);
        }

        // Link nodes horizontally in a circle
        for i in 0..row_nodes.len() {
            let next = (i + 1) % row_nodes.len();
            let prev = if i == 0 { row_nodes.len() - 1 } else { i - 1 };

            row_nodes[i].borrow_mut().right = Some(Rc::clone(&row_nodes[next]));
            row_nodes[i].borrow_mut().left = Some(Rc::clone(&row_nodes[prev]));
        }
    }

    fn cover(&self, col: &NodeRef) {
        // Remove column from header list
        let left = Rc::clone(col.borrow().left.as_ref().unwrap());
        let right = Rc::clone(col.borrow().right.as_ref().unwrap());
        left.borrow_mut().right = Some(Rc::clone(&right));
        right.borrow_mut().left = Some(Rc::clone(&left));

        // Remove all rows in this column
        let mut row_node = Rc::clone(col.borrow().down.as_ref().unwrap());
        while !Rc::ptr_eq(&row_node, col) {
            let mut right_node = Rc::clone(row_node.borrow().right.as_ref().unwrap());
            while !Rc::ptr_eq(&right_node, &row_node) {
                let up = Rc::clone(right_node.borrow().up.as_ref().unwrap());
                let down = Rc::clone(right_node.borrow().down.as_ref().unwrap());
                up.borrow_mut().down = Some(Rc::clone(&down));
                down.borrow_mut().up = Some(Rc::clone(&up));

                let column = Rc::clone(right_node.borrow().column.as_ref().unwrap());
                column.borrow_mut().size -= 1;

                let next = Rc::clone(right_node.borrow().right.as_ref().unwrap());
                right_node = next;
            }

            let next = Rc::clone(row_node.borrow().down.as_ref().unwrap());
            row_node = next;
        }
    }

    fn uncover(&self, col: &NodeRef) {
        // Restore all rows in this column (in reverse order)
        let mut row_node = Rc::clone(col.borrow().up.as_ref().unwrap());
        while !Rc::ptr_eq(&row_node, col) {
            let mut left_node = Rc::clone(row_node.borrow().left.as_ref().unwrap());
            while !Rc::ptr_eq(&left_node, &row_node) {
                let column = Rc::clone(left_node.borrow().column.as_ref().unwrap());
                column.borrow_mut().size += 1;

                let up = Rc::clone(left_node.borrow().up.as_ref().unwrap());
                let down = Rc::clone(left_node.borrow().down.as_ref().unwrap());
                up.borrow_mut().down = Some(Rc::clone(&left_node));
                down.borrow_mut().up = Some(Rc::clone(&left_node));

                let prev = Rc::clone(left_node.borrow().left.as_ref().unwrap());
                left_node = prev;
            }

            let prev = Rc::clone(row_node.borrow().up.as_ref().unwrap());
            row_node = prev;
        }

        // Restore column to header list
        let left = Rc::clone(col.borrow().left.as_ref().unwrap());
        let right = Rc::clone(col.borrow().right.as_ref().unwrap());
        left.borrow_mut().right = Some(Rc::clone(col));
        right.borrow_mut().left = Some(Rc::clone(col));
    }

    fn choose_column(&self) -> Option<NodeRef> {
        let binding = self.root.borrow();
        let root_right = binding.right.as_ref()?;

        if Rc::ptr_eq(root_right, &self.root) {
            return None; // Empty matrix
        }

        let mut best = Rc::clone(root_right);
        let mut min_size = best.borrow().size;

        let mut col = Rc::clone(best.borrow().right.as_ref().unwrap());
        while !Rc::ptr_eq(&col, &self.root) {
            let size = col.borrow().size;
            if size < min_size {
                min_size = size;
                best = Rc::clone(&col);
            }

            let next = Rc::clone(col.borrow().right.as_ref().unwrap());
            col = next;
        }

        Some(best)
    }

    fn cover_clue_row(&mut self, row_id: i32) {
        // Find a node with this row_id and cover all its columns
        for col in &self.columns {
            let mut node = Rc::clone(col.borrow().down.as_ref().unwrap());
            while !Rc::ptr_eq(&node, col) {
                if node.borrow().row_id == row_id {
                    // Found the row, cover all its columns
                    let mut curr = Rc::clone(&node);
                    loop {
                        let curr_col = Rc::clone(curr.borrow().column.as_ref().unwrap());
                        self.cover(&curr_col);

                        let next = Rc::clone(curr.borrow().right.as_ref().unwrap());
                        if Rc::ptr_eq(&next, &node) {
                            break;
                        }
                        curr = next;
                    }
                    return;
                }

                let next = Rc::clone(node.borrow().down.as_ref().unwrap());
                node = next;
            }
        }
    }

    fn search(&mut self, solution: &mut Vec<i32>) -> bool {
        self.iterations += 1;

        // Check if matrix is empty (solution found)
        let root_right = Rc::clone(self.root.borrow().right.as_ref().unwrap());
        if Rc::ptr_eq(&root_right, &self.root) {
            return true;
        }

        // Choose column with minimum size
        let col = match self.choose_column() {
            Some(c) => c,
            None => return false,
        };

        // If column has no rows, no solution
        if col.borrow().size == 0 {
            return false;
        }

        self.cover(&col);

        // Try each row in this column
        let mut row_node = Rc::clone(col.borrow().down.as_ref().unwrap());
        while !Rc::ptr_eq(&row_node, &col) {
            solution.push(row_node.borrow().row_id);

            // Cover all other columns in this row
            let mut right_node = Rc::clone(row_node.borrow().right.as_ref().unwrap());
            while !Rc::ptr_eq(&right_node, &row_node) {
                let right_col = Rc::clone(right_node.borrow().column.as_ref().unwrap());
                self.cover(&right_col);

                let next = Rc::clone(right_node.borrow().right.as_ref().unwrap());
                right_node = next;
            }

            // Recurse
            if self.search(solution) {
                return true;
            }

            // Backtrack
            solution.pop();

            // Uncover columns in reverse order
            let mut left_node = Rc::clone(row_node.borrow().left.as_ref().unwrap());
            while !Rc::ptr_eq(&left_node, &row_node) {
                let left_col = Rc::clone(left_node.borrow().column.as_ref().unwrap());
                self.uncover(&left_col);

                let prev = Rc::clone(left_node.borrow().left.as_ref().unwrap());
                left_node = prev;
            }

            let next = Rc::clone(row_node.borrow().down.as_ref().unwrap());
            row_node = next;
        }

        self.uncover(&col);
        false
    }
}

fn read_puzzle(filename: &str) -> Vec<Vec<i32>> {
    let contents = fs::read_to_string(filename)
        .expect("Failed to read file");

    let mut puzzle = Vec::new();

    println!("{}", filename);
    for line in contents.lines() {
        if line.starts_with('#') || line.trim().is_empty() {
            continue;
        }

        let row: Vec<i32> = line
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();

        if row.len() == 9 {
            // Print row as we read it
            for &val in &row {
                print!("{} ", val);
            }
            println!();
            puzzle.push(row);
        }
    }

    puzzle
}

fn print_puzzle(puzzle: &[Vec<i32>]) {
    println!("\nPuzzle:");
    for row in puzzle {
        for &val in row {
            print!("{} ", val);
        }
        println!();
    }
}

fn build_exact_cover_matrix(puzzle: &[Vec<i32>]) -> (DlxSolver, Vec<i32>) {
    // 324 constraints: 81 cell + 81 row + 81 col + 81 box
    let mut solver = DlxSolver::new(324);
    let mut clue_rows = Vec::new();

    for row in 0..9 {
        for col in 0..9 {
            let given = puzzle[row][col];
            let digits = if given == 0 {
                vec![1, 2, 3, 4, 5, 6, 7, 8, 9]
            } else {
                vec![given]
            };

            for digit in digits {
                let box_id = (row / 3) * 3 + (col / 3);
                let d = digit as usize;

                // Row ID encodes position and digit: row*81 + col*9 + (digit-1)
                let row_id = (row * 81 + col * 9 + d - 1) as i32;

                // Four constraints for this placement
                let constraints = vec![
                    row * 9 + col,              // Cell constraint
                    81 + row * 9 + d - 1,       // Row constraint
                    162 + col * 9 + d - 1,      // Col constraint
                    243 + box_id * 9 + d - 1,   // Box constraint
                ];

                solver.add_row(row_id, &constraints);

                // Record if this is a clue
                if given != 0 {
                    clue_rows.push(row_id);
                }
            }
        }
    }

    (solver, clue_rows)
}

fn decode_solution(solution: &[i32]) -> Vec<Vec<i32>> {
    let mut grid = vec![vec![0; 9]; 9];

    for &row_id in solution {
        let digit = (row_id % 9) + 1;
        let col = (row_id / 9) % 9;
        let row = row_id / 81;

        grid[row as usize][col as usize] = digit;
    }

    grid
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <matrix_file>", args[0]);
        return;
    }

    let start = Instant::now();

    let puzzle = read_puzzle(&args[1]);
    print_puzzle(&puzzle);

    let (mut solver, clue_rows) = build_exact_cover_matrix(&puzzle);

    // Cover pre-filled clues before search
    for &clue_row in &clue_rows {
        solver.cover_clue_row(clue_row);
    }

    let mut solution = Vec::new();

    if solver.search(&mut solution) {
        // Add clue rows to solution for complete grid
        solution.extend(&clue_rows);
        let solved = decode_solution(&solution);
        print_puzzle(&solved);
        println!("\nSolved in Iterations={}\n", solver.iterations);
    } else {
        println!("No solution found");
    }

    let elapsed = start.elapsed();
    println!("Seconds to process {:.3}", elapsed.as_secs_f64());
}
