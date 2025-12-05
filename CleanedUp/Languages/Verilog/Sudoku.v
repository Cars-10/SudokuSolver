module Sudoku;
    integer board[0:8][0:8];
    integer iterations;
    integer file, r, c, code, char;
    reg [8*100:1] filename;

    initial begin
        if ($value$plusargs("FILE=%s", filename)) begin
            // OK
        end else begin
            $display("Usage: iverilog -o Sudoku Sudoku.v && vvp Sudoku +FILE=<input_file>");
            $finish;
        end

        // Read file
        file = $fopen(filename, "r");
        if (file == 0) begin
            $display("Error opening file");
            $finish;
        end

        r = 0;
        c = 0;
        while (!$feof(file) && r < 9) begin
            char = $fgetc(file);
            if (char >= "0" && char <= "9") begin
                board[r][c] = char - "0";
                c = c + 1;
                if (c == 9) begin
                    c = 0;
                    r = r + 1;
                end
            end
        end
        $fclose(file);

        iterations = 0;
        if (solve(0, 0)) begin
            $display("Solved in Iterations= %0d", iterations);
            print_board;
        end else begin
            $display("No solution found.");
        end
        $finish;
    end

    function integer solve;
        input integer r, c;
        integer nr, nc, num;
        begin
            // Find next empty
            while (r < 9 && board[r][c] != 0) begin
                c = c + 1;
                if (c == 9) begin
                    c = 0;
                    r = r + 1;
                end
            end

            if (r == 9) begin
                solve = 1; // Solved
            end else begin
                solve = 0;
                for (num = 1; num <= 9; num = num + 1) begin
                    if (is_valid(r, c, num)) begin
                        board[r][c] = num;
                        iterations = iterations + 1;
                        
                        // Next cell
                        nc = c + 1;
                        nr = r;
                        if (nc == 9) begin
                            nc = 0;
                            nr = r + 1;
                        end

                        if (solve(nr, nc)) begin
                            solve = 1;
                            num = 10; // Break
                        end else begin
                            board[r][c] = 0; // Backtrack
                        end
                    end
                end
            end
        end
    endfunction

    function integer is_valid;
        input integer r, c, num;
        integer i, j, sr, sc;
        begin
            is_valid = 1;
            
            // Check row
            for (j = 0; j < 9; j = j + 1) begin
                if (board[r][j] == num) is_valid = 0;
            end

            // Check col
            for (i = 0; i < 9; i = i + 1) begin
                if (board[i][c] == num) is_valid = 0;
            end

            // Check box
            sr = (r / 3) * 3;
            sc = (c / 3) * 3;
            for (i = 0; i < 3; i = i + 1) begin
                for (j = 0; j < 3; j = j + 1) begin
                    if (board[sr + i][sc + j] == num) is_valid = 0;
                end
            end
        end
    endfunction

    task print_board;
        integer i, j;
        begin
            for (i = 0; i < 9; i = i + 1) begin
                for (j = 0; j < 9; j = j + 1) begin
                    $write("%0d", board[i][j]);
                    if (j < 8) $write(" ");
                end
                $write("\n");
            end
        end
    endtask

endmodule
