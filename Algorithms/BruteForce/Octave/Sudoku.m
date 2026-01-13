#!/usr/bin/env octave
% Sudoku Solver in GNU Octave
% Uses 1-based indexing (Octave convention)

global puzzle;
global count;

function result = is_valid(row, col, val)
    global puzzle;

    % Check row (Octave is 1-based)
    for i = 1:9
        if puzzle(row, i) == val
            result = 0;
            return;
        endif
    endfor

    % Check column
    for i = 1:9
        if puzzle(i, col) == val
            result = 0;
            return;
        endif
    endfor

    % Check 3x3 box
    br = floor((row-1)/3)*3 + 1;
    bc = floor((col-1)/3)*3 + 1;
    for i = 0:2
        for j = 0:2
            if puzzle(br+i, bc+j) == val
                result = 0;
                return;
            endif
        endfor
    endfor

    result = 1;
endfunction

function result = solve()
    global puzzle count;

    % Find first empty cell (row-major order)
    found_row = -1;
    found_col = -1;

    for row = 1:9
        for col = 1:9
            if puzzle(row, col) == 0
                found_row = row;
                found_col = col;
                break;
            endif
        endfor
        if found_row > 0
            break;
        endif
    endfor

    % If no empty cell found, puzzle is solved
    if found_row == -1
        result = 1;
        return;
    endif

    % Try values 1-9 in order
    for val = 1:9
        count = count + 1;  % COUNT BEFORE validity check
        if is_valid(found_row, found_col, val)
            puzzle(found_row, found_col) = val;
            if solve() == 1
                result = 1;
                return;
            endif
            puzzle(found_row, found_col) = 0;  % Backtrack
        endif
    endfor

    result = 0;
endfunction

function print_puzzle()
    global puzzle;
    fprintf('\n');
    fprintf('Puzzle:\n');
    for row = 1:9
        for col = 1:9
            fprintf('%d ', puzzle(row, col));
        endfor
        fprintf('\n');
    endfor
endfunction

function read_matrix_file(filename)
    global puzzle;

    % Normalize path for output (convert absolute to relative)
    display_path = filename;
    if strncmp(filename, '/app/Matrices/', 14) == 1
        display_path = filename(6:end);  % Skip "/app/" to get "Matrices/..."
        fprintf('../%s\n', display_path);
    else
        fprintf('%s\n', filename);
    endif

    fid = fopen(filename, 'r');
    if fid == -1
        error('Error opening file %s', filename);
    endif

    line_count = 0;

    while ~feof(fid)
        line = fgetl(fid);
        if ischar(line)
            % Skip comments and empty lines
            line = strtrim(line);
            if ~isempty(line) && line(1) ~= '#'
                values = sscanf(line, '%d');
                if length(values) == 9
                    line_count = line_count + 1;
                    for i = 1:9
                        puzzle(line_count, i) = values(i);
                        fprintf('%d ', values(i));
                    endfor
                    fprintf('\n');
                    if line_count == 9
                        break;
                    endif
                endif
            endif
        endif
    endwhile

    fclose(fid);
endfunction

% Main script
function main()
    global puzzle count;
    puzzle = zeros(9, 9);

    args = argv();
    start_time = time();

    for i = 1:length(args)
        filename = args{i};
        if length(filename) > 7 && strcmp(filename(end-6:end), '.matrix') == 1
            read_matrix_file(filename);
            print_puzzle();

            count = 0;
            if solve() == 1
                print_puzzle();
                fprintf('\n');
                fprintf('Solved in Iterations=%d\n', count);
                fprintf('\n');
            else
                fprintf('No solution found\n');
            endif
        endif
    endfor

    elapsed = time() - start_time;
    fprintf('Seconds to process %.3f\n', elapsed);
endfunction

% Execute main
main();
