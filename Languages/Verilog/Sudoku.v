module Sudoku;
    integer board[0:8][0:8];
    integer iterations;
    integer file, r, c, code, char;
    reg [8*256:1] filename;

    initial begin
        // Initialize board to 0
        for (integer i = 0; i < 9; i = i + 1) begin
            for (integer j = 0; j < 9; j = j + 1) begin
                board[i][j] = 0;
            end
        end

        if ($value$plusargs("FILE=%s", filename)) begin
            // OK
        end else begin
            $display("Usage: vvp Sudoku +FILE=<input_file>");
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
        while (r < 9) begin
            char = $fgetc(file);
            if (char == -1) begin // EOF
                r = 10;
            end else if (char >= 48 && char <= 57) begin // '0' through '9'
                board[r][c] = char - 48;
                c = c + 1;
                if (c == 9) begin
                    c = 0;
                    r = r + 1;
                end
            end
        end
        $fclose(file);

        $display("\nPuzzle:");
        print_board;

        iterations = 0;
        if (solve(0)) begin
            $display("\nPuzzle:");
            print_board;
            $display("\nSolved in Iterations=%0d", iterations);
        end else begin
            $display("\nNo solution found.");
        end
        $finish;
    end

    function automatic integer solve;
        input integer dummy;
        integer row, col, found, i, j, val;
        begin
            row = -1;
            col = -1;
            found = 0;

            // Find first empty cell
            for (i = 0; i < 9 && found == 0; i = i + 1) begin
                for (j = 0; j < 9 && found == 0; j = j + 1) begin
                    if (board[i][j] == 0) begin
                        row = i;
                        col = j;
                        found = 1;
                    end
                end
            end

            if (found == 0) begin
                solve = 1; // Solved
            end else begin
                solve = 0;
                for (val = 1; val <= 9; val = val + 1) begin
                    iterations = iterations + 1;
                    if (is_valid(row, col, val)) begin
                        board[row][col] = val;
                        if (solve(0)) begin
                            solve = 1;
                            val = 10; // Break
                        end else begin
                            board[row][col] = 0; // Backtrack
                        end
                    end
                end
            end
        end
    endfunction

    function automatic integer is_valid;
        input integer r, c, num;
        integer i, j, sr, sc;
        begin
            is_valid = 1;
            
            // Check row
            for (j = 0; j < 9; j = j + 1) begin
                if (board[r][j] == num) is_valid = 0;
            end

            // Check col
            if (is_valid) begin
                for (i = 0; i < 9; i = i + 1) begin
                    if (board[i][c] == num) is_valid = 0;
                end
            end

            // Check box
            if (is_valid) begin
                sr = (r / 3) * 3;
                sc = (c / 3) * 3;
                for (i = 0; i < 3; i = i + 1) begin
                    for (j = 0; j < 3; j = j + 1) begin
                        if (board[sr + i][sc + j] == num) is_valid = 0;
                    end
                end
            end
        end
    endfunction

    task print_board;
        integer i, j;
        begin
            for (i = 0; i < 9; i = i + 1) begin
                for (j = 0; j < 9; j = j + 1) begin
                    $write("%0d ", board[i][j]);
                end
                $write("\n");
            end
        end
    endtask

endmodule