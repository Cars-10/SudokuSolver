#!/usr/bin/env pike
// Constraint Propagation (CP) Sudoku Solver - Pike Implementation
// Algorithm: Constraint propagation with MRV heuristic

int cp_iterations = 0;

// Grid structure
class CPGrid {
    array(array(int)) values = allocate(9, allocate(9));
    array(array(int)) candidates = allocate(9, allocate(9));
}

// Bitset operations
int has_candidate(int candidate_set, int digit) {
    return (candidate_set & (1 << digit)) != 0;
}

int remove_candidate(int candidate_set, int digit) {
    return candidate_set & ~(1 << digit);
}

int count_candidates(int candidate_set) {
    int count = 0;
    for(int i = 1; i <= 9; i++) {
        if(has_candidate(candidate_set, i)) count++;
    }
    return count;
}

int get_first_candidate(int candidate_set) {
    for(int i = 1; i <= 9; i++) {
        if(has_candidate(candidate_set, i)) return i;
    }
    return 0;
}

// Initialize grid from puzzle
void init_grid(CPGrid grid, array(array(int)) puzzle) {
    for(int row = 0; row < 9; row++) {
        for(int col = 0; col < 9; col++) {
            if(puzzle[row][col] == 0) {
                grid->values[row][col] = 0;
                grid->candidates[row][col] = 0x3FE;  // bits 1-9 set
            } else {
                int digit = puzzle[row][col];
                grid->values[row][col] = digit;
                grid->candidates[row][col] = (1 << digit);
            }
        }
    }
}

// Forward declaration
int assign_value(CPGrid grid, int row, int col, int digit);

// Eliminate digit from candidates at (row, col)
int eliminate(CPGrid grid, int row, int col, int digit) {
    // Check if already eliminated
    if(!has_candidate(grid->candidates[row][col], digit))
        return 1;
    
    // Remove digit
    grid->candidates[row][col] = remove_candidate(grid->candidates[row][col], digit);
    
    // Check for contradiction
    int remaining = count_candidates(grid->candidates[row][col]);
    if(remaining == 0)
        return 0;
    
    // Singleton elimination
    if(remaining == 1 && grid->values[row][col] == 0) {
        int last_digit = get_first_candidate(grid->candidates[row][col]);
        if(!assign_value(grid, row, col, last_digit))
            return 0;
    }
    
    return 1;
}

// Assign digit to cell at (row, col)
int assign_value(CPGrid grid, int row, int col, int digit) {
    cp_iterations++;
    
    grid->values[row][col] = digit;
    grid->candidates[row][col] = (1 << digit);
    
    // Eliminate from row
    for(int c = 0; c < 9; c++) {
        if(c != col) {
            if(!eliminate(grid, row, c, digit))
                return 0;
        }
    }
    
    // Eliminate from column
    for(int r = 0; r < 9; r++) {
        if(r != row) {
            if(!eliminate(grid, r, col, digit))
                return 0;
        }
    }
    
    // Eliminate from box
    int box_row = (row / 3) * 3;
    int box_col = (col / 3) * 3;
    for(int r = box_row; r < box_row + 3; r++) {
        for(int c = box_col; c < box_col + 3; c++) {
            if(r != row && c != col) {
                if(!eliminate(grid, r, c, digit))
                    return 0;
            }
        }
    }
    
    return 1;
}

// Apply constraint propagation
int propagate(CPGrid grid) {
    int changed = 1;
    
    while(changed) {
        changed = 0;
        
        // Singleton elimination
        for(int row = 0; row < 9; row++) {
            for(int col = 0; col < 9; col++) {
                if(grid->values[row][col] == 0) {
                    int num_candidates = count_candidates(grid->candidates[row][col]);
                    if(num_candidates == 0)
                        return 0;
                    if(num_candidates == 1) {
                        int digit = get_first_candidate(grid->candidates[row][col]);
                        if(!assign_value(grid, row, col, digit))
                            return 0;
                        changed = 1;
                    }
                }
            }
        }
        
        // Hidden singles - rows
        for(int row = 0; row < 9; row++) {
            for(int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_col = -1;
                int already_assigned = 0;
                
                for(int col = 0; col < 9; col++) {
                    if(grid->values[row][col] == digit) {
                        already_assigned = 1;
                        break;
                    }
                    if(has_candidate(grid->candidates[row][col], digit)) {
                        count++;
                        last_col = col;
                    }
                }
                
                if(already_assigned) continue;
                
                if(count == 1) {
                    if(!assign_value(grid, row, last_col, digit))
                        return 0;
                    changed = 1;
                } else if(count == 0) {
                    return 0;
                }
            }
        }
        
        // Hidden singles - columns
        for(int col = 0; col < 9; col++) {
            for(int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_row = -1;
                int already_assigned = 0;
                
                for(int row = 0; row < 9; row++) {
                    if(grid->values[row][col] == digit) {
                        already_assigned = 1;
                        break;
                    }
                    if(has_candidate(grid->candidates[row][col], digit)) {
                        count++;
                        last_row = row;
                    }
                }
                
                if(already_assigned) continue;
                
                if(count == 1) {
                    if(!assign_value(grid, last_row, col, digit))
                        return 0;
                    changed = 1;
                } else if(count == 0) {
                    return 0;
                }
            }
        }
        
        // Hidden singles - boxes
        for(int box = 0; box < 9; box++) {
            int box_row = (box / 3) * 3;
            int box_col = (box % 3) * 3;
            
            for(int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_r = -1;
                int last_c = -1;
                int already_assigned = 0;
                
                for(int r = box_row; r < box_row + 3; r++) {
                    for(int c = box_col; c < box_col + 3; c++) {
                        if(grid->values[r][c] == digit) {
                            already_assigned = 1;
                            goto box_done;
                        }
                        if(has_candidate(grid->candidates[r][c], digit)) {
                            count++;
                            last_r = r;
                            last_c = c;
                        }
                    }
                }
                
                box_done:
                if(already_assigned) continue;
                
                if(count == 1) {
                    if(!assign_value(grid, last_r, last_c, digit))
                        return 0;
                    changed = 1;
                } else if(count == 0) {
                    return 0;
                }
            }
        }
    }
    
    return 1;
}

// Check if grid is complete
int is_complete(CPGrid grid) {
    for(int row = 0; row < 9; row++) {
        for(int col = 0; col < 9; col++) {
            if(grid->values[row][col] == 0)
                return 0;
        }
    }
    return 1;
}

// Find cell with minimum remaining values (MRV)
array(int) find_mrv_cell(CPGrid grid) {
    int min_count = 10;
    int min_row = -1;
    int min_col = -1;
    
    for(int row = 0; row < 9; row++) {
        for(int col = 0; col < 9; col++) {
            if(grid->values[row][col] == 0) {
                int count = count_candidates(grid->candidates[row][col]);
                if(count < min_count) {
                    min_count = count;
                    min_row = row;
                    min_col = col;
                }
            }
        }
    }
    
    return ({ min_row, min_col });
}

// Save grid state
CPGrid save_state(CPGrid grid) {
    CPGrid saved = CPGrid();
    for(int row = 0; row < 9; row++) {
        for(int col = 0; col < 9; col++) {
            saved->values[row][col] = grid->values[row][col];
            saved->candidates[row][col] = grid->candidates[row][col];
        }
    }
    return saved;
}

// Restore grid state
void restore_state(CPGrid grid, CPGrid saved) {
    for(int row = 0; row < 9; row++) {
        for(int col = 0; col < 9; col++) {
            grid->values[row][col] = saved->values[row][col];
            grid->candidates[row][col] = saved->candidates[row][col];
        }
    }
}

// Solve with search
int cp_search(CPGrid grid) {
    if(!propagate(grid))
        return 0;
    
    if(is_complete(grid))
        return 1;
    
    array(int) mrv = find_mrv_cell(grid);
    int row = mrv[0];
    int col = mrv[1];
    
    if(row == -1)
        return 0;
    
    int candidates_set = grid->candidates[row][col];
    
    for(int digit = 1; digit <= 9; digit++) {
        if(has_candidate(candidates_set, digit)) {
            CPGrid saved = save_state(grid);
            
            if(assign_value(grid, row, col, digit)) {
                if(cp_search(grid))
                    return 1;
            }
            
            restore_state(grid, saved);
        }
    }
    
    return 0;
}

// Read puzzle from file
array(array(int)) read_puzzle(string filename) {
    array(array(int)) puzzle = allocate(9, allocate(9));
    
    string content = Stdio.read_file(filename);
    if(!content) {
        werror("Error reading file: %s\n", filename);
        exit(1);
    }
    
    array(string) lines = content / "\n";
    int row = 0;
    
    foreach(lines, string line) {
        if(row >= 9) break;
        
        array(string) values = array_sscanf(line, "%{%d%*[ ]%}")[0];
        if(!values || sizeof(values) == 0) continue;
        
        int col = 0;
        foreach(values, array val) {
            if(col >= 9) break;
            puzzle[row][col] = val[0];
            col++;
        }
        
        if(col == 9) row++;
    }
    
    return puzzle;
}

// Print grid
void print_grid(CPGrid grid) {
    write("\nPuzzle:\n");
    for(int row = 0; row < 9; row++) {
        for(int col = 0; col < 9; col++) {
            write("%d ", grid->values[row][col]);
        }
        write("\n");
    }
}

// Main
int main(int argc, array(string) argv) {
    if(argc < 2) {
        werror("Usage: pike cp.pike <matrix_file>\n");
        return 1;
    }
    
    string filename = argv[1];
    write("%s\n", filename);
    
    array(array(int)) puzzle = read_puzzle(filename);
    CPGrid grid = CPGrid();
    init_grid(grid, puzzle);
    print_grid(grid);
    
    float start_time = System.get_micros() / 1000000.0;
    int result = cp_search(grid);
    float end_time = System.get_micros() / 1000000.0;
    
    print_grid(grid);
    
    if(result) {
        write("\nSolved in Iterations=%d\n", cp_iterations);
    } else {
        write("\nNo solution found\n");
    }
    
    write("\nSeconds to process %.3f\n", end_time - start_time);
    
    return 0;
}
