/* Verilog Sudoku Solver */

module sudoku;
    reg [3:0] puzzle [0:8][0:8];
    reg [31:0] count;
    integer r, c, i, j;
    integer file, scan;
    reg [8*100:1] filename;
    reg [31:0] start_time, end_time;
    
    // We need to implement the solver in a task or function
    // Since Verilog is hardware description, recursive functions are tricky (automatic).
    // SystemVerilog supports automatic functions. Icarus Verilog supports them too.
    
    function automatic integer is_possible;
        input integer r_in, c_in, val_in;
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

    function automatic integer solve;
        input integer depth; // Dummy arg to make it recursive? No, just call solve()
        integer r_s, c_s, val_s;
        integer found;
        begin
            solve = 0;
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
                                if (solve(depth + 1)) begin
                                    solve = 1;
                                    found = 2; // Break out
                                end else begin
                                    puzzle[r_s][c_s] = 0;
                                end
                            end
                            if (found == 2) val_s = 10; // Break
                        end
                        if (found == 1) solve = 0; // Failed to find value
                    end
                end
            end
            if (found == 0) solve = 1; // No empty cells
            if (found == 2) solve = 1; // Solved in recursion
        end
    endfunction

    task print_puzzle;
        integer r_p, c_p;
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
            
            // Read file
            // Verilog file reading is primitive. We might need to pre-process or use $readmemh
            // But $readmemh expects hex. $readmemb binary.
            // Let's assume the input is formatted for $readmemh or we parse it.
            // Actually, let's use $fscanf
            
            file = $fopen(filename, "r");
            if (file == 0) begin
                $display("Error opening file");
                $finish;
            end
            
            for (r = 0; r < 9; r = r + 1) begin
                for (c = 0; c < 9; c = c + 1) begin
                    scan = $fscanf(file, "%d", puzzle[r][c]);
                end
            end
            $fclose(file);
            
            print_puzzle();
            count = 0;
            if (solve(0)) begin
                print_puzzle();
                $display("\nSolved in Iterations=%0d\n", count);
            end else begin
                $display("No solution found");
            end
        end
        $finish;
    end
endmodule
