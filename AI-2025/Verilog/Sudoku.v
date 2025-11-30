/* Verilog Sudoku Solver */

module sudoku;
    integer puzzle [0:8][0:8];
    reg [31:0] count;
    integer r, c, i, j;
    integer file, scan, temp_val;
    reg [8*100:1] filename;
    
    function automatic integer is_possible(input integer r_in, input integer c_in, input integer val_in);
        integer k, m;
        integer r0, c0;
        begin
            is_possible = 1;
            for (k = 0; k < 9; k = k + 1) begin
                if (puzzle[k][c_in] == val_in) is_possible = 0;
                if (puzzle[r_in][k] == val_in) is_possible = 0;
            end
            
            r0 = (r_in / 3) * 3;
            c0 = (c_in / 3) * 3;
            
            for (k = 0; k < 3; k = k + 1) begin
                for (m = 0; m < 3; m = m + 1) begin
                    if (puzzle[r0 + k][c0 + m] == val_in) is_possible = 0;
                end
            end
        end
    endfunction

    function automatic integer solve_sudoku(input integer depth);
        integer r_s, c_s, val_s;
        integer found;
        begin
            solve_sudoku = 0;
            found = 0;
            // Find empty cell
            for (r_s = 0; r_s < 9; r_s = r_s + 1) begin
                for (c_s = 0; c_s < 9; c_s = c_s + 1) begin
                    if (puzzle[r_s][c_s] == 0 && found == 0) begin
                        found = 1;
                        // Try values
                        for (val_s = 1; val_s <= 9; val_s = val_s + 1) begin
                            count = count + 1;
                            if (is_possible(r_s, c_s, val_s)) begin
                                puzzle[r_s][c_s] = val_s;
                                if (solve_sudoku(depth + 1)) begin
                                    solve_sudoku = 1;
                                    found = 2; // Break out
                                end else begin
                                    puzzle[r_s][c_s] = 0;
                                end
                            end
                            if (found == 2) val_s = 10; // Break
                        end
                        if (found == 1) solve_sudoku = 0; // Failed to find value
                    end
                end
            end
            if (found == 0) solve_sudoku = 1; // No empty cells
            if (found == 2) solve_sudoku = 1; // Solved in recursion
        end
    endfunction

    task print_puzzle;
        integer r_p;
        begin
            $display("\nPuzzle:");
            for (r_p = 0; r_p < 9; r_p = r_p + 1) begin
                $write("%0d %0d %0d %0d %0d %0d %0d %0d %0d\n", 
                    puzzle[r_p][0], puzzle[r_p][1], puzzle[r_p][2],
                    puzzle[r_p][3], puzzle[r_p][4], puzzle[r_p][5],
                    puzzle[r_p][6], puzzle[r_p][7], puzzle[r_p][8]);
            end
        end
    endtask

    initial begin
        if ($value$plusargs("FILE=%s", filename)) begin
            $display("%0s", filename);
            
            file = $fopen(filename, "r");
            if (file == 0) begin
                $display("Error opening file");
                $finish;
            end
            
            for (r = 0; r < 9; r = r + 1) begin
                for (c = 0; c < 9; c = c + 1) begin
                    scan = $fscanf(file, "%d", temp_val);
                    puzzle[r][c] = temp_val;
                end
            end
            $fclose(file);
            
            print_puzzle();
            count = 0;
            if (solve_sudoku(0)) begin
                print_puzzle();
                $display("\nSolved in Iterations=%0d\n", count);
            end else begin
                $display("No solution found");
            end
        end
        $finish;
    end
endmodule
