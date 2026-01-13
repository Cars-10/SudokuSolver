#!/usr/bin/env octave
% Sudoku Solver - Constraint Propagation (CP) Implementation in GNU Octave
% Uses bitsets for candidate tracking and MRV heuristic for cell selection

global cp_iterations;

% ============================================================================
% BITSET HELPER FUNCTIONS
% ============================================================================

% Test if digit is a candidate (bit set)
function result = has_candidate(set, digit)
    result = bitand(set, bitshift(1, digit)) != 0;
endfunction

% Add digit as candidate (set bit)
function result = add_candidate(set, digit)
    result = bitor(set, bitshift(1, digit));
endfunction

% Remove digit as candidate (clear bit)
function result = remove_candidate(set, digit)
    mask = bitshift(1, digit);
    result = bitand(set, bitcmp(mask, 16));  % 16 bits for uint16
endfunction

% Count number of candidates (popcount)
function result = count_candidates(set)
    result = 0;
    for d = 1:9
        if has_candidate(set, d)
            result = result + 1;
        endif
    endfor
endfunction

% Get first candidate digit
function result = get_first_candidate(set)
    for d = 1:9
        if has_candidate(set, d)
            result = d;
            return;
        endif
    endfor
    result = 0;
endfunction

% ============================================================================
% GRID STRUCTURE
% ============================================================================

function grid = create_grid()
    grid = struct('values', zeros(9, 9), 'candidates', zeros(9, 9, 'uint16'));
endfunction

% ============================================================================
% INITIALIZATION
% ============================================================================

function grid = init_grid(puzzle)
    grid = create_grid();

    for row = 1:9
        for col = 1:9
            if puzzle(row, col) == 0
                % Empty cell: set all candidates 1-9
                grid.values(row, col) = 0;
                grid.candidates(row, col) = hex2dec('3FE');  % bits 1-9 set
            else
                % Given clue: set single value
                digit = puzzle(row, col);
                grid.values(row, col) = digit;
                grid.candidates(row, col) = bitshift(1, digit);
            endif
        endfor
    endfor
endfunction

% ============================================================================
% PEER CALCULATION
% ============================================================================

% Get all 20 peers for a cell (row, col, box) - returns [20 x 2] array
function peers = get_peers(row, col)
    peers = zeros(20, 2);
    idx = 1;

    % Same row (8 cells, excluding self)
    for c = 1:9
        if c != col
            peers(idx, :) = [row, c];
            idx = idx + 1;
        endif
    endfor

    % Same column (8 cells, excluding self)
    for r = 1:9
        if r != row
            peers(idx, :) = [r, col];
            idx = idx + 1;
        endif
    endfor

    % Same 3x3 box (4 cells, excluding self and already counted)
    box_row = floor((row - 1) / 3) * 3 + 1;
    box_col = floor((col - 1) / 3) * 3 + 1;

    for r = box_row:(box_row + 2)
        for c = box_col:(box_col + 2)
            if r != row && c != col
                peers(idx, :) = [r, c];
                idx = idx + 1;
            endif
        endfor
    endfor
endfunction

% ============================================================================
% CONSTRAINT PROPAGATION
% ============================================================================

% Eliminate a digit from a cell's candidates
function [grid, success] = eliminate(grid, row, col, digit)
    % Check if digit is already eliminated
    if ~has_candidate(grid.candidates(row, col), digit)
        success = 1;
        return;
    endif

    % Remove digit from candidates
    grid.candidates(row, col) = remove_candidate(grid.candidates(row, col), digit);

    % Check for contradiction
    remaining = count_candidates(grid.candidates(row, col));
    if remaining == 0
        success = 0;
        return;
    endif

    % If only one candidate left, assign it (singleton elimination)
    if remaining == 1 && grid.values(row, col) == 0
        last_digit = get_first_candidate(grid.candidates(row, col));
        [grid, success] = assign(grid, row, col, last_digit);
        return;
    endif

    success = 1;
endfunction

% Assign a value to a cell
function [grid, success] = assign(grid, row, col, digit)
    global cp_iterations;

    % Increment iteration counter
    cp_iterations = cp_iterations + 1;

    % Set value
    grid.values(row, col) = digit;
    grid.candidates(row, col) = bitshift(1, digit);

    % Eliminate digit from all peers
    peers = get_peers(row, col);

    for i = 1:20
        peer_row = peers(i, 1);
        peer_col = peers(i, 2);

        [grid, success] = eliminate(grid, peer_row, peer_col, digit);
        if success == 0
            return;
        endif
    endfor

    success = 1;
endfunction

% Propagate constraints until quiescence
function [grid, success] = propagate(grid)
    changed = 1;

    while changed
        changed = 0;

        % Strategy 1: Singleton elimination
        for row = 1:9
            for col = 1:9
                if grid.values(row, col) == 0
                    num_candidates = count_candidates(grid.candidates(row, col));
                    if num_candidates == 0
                        success = 0;
                        return;
                    endif
                    if num_candidates == 1
                        digit = get_first_candidate(grid.candidates(row, col));
                        [grid, success] = assign(grid, row, col, digit);
                        if success == 0
                            return;
                        endif
                        changed = 1;
                    endif
                endif
            endfor
        endfor

        % Strategy 2: Hidden singles in rows
        for row = 1:9
            for digit = 1:9
                count = 0;
                last_col = -1;
                % Check if digit is already assigned
                already_assigned = 0;
                for col = 1:9
                    if grid.values(row, col) == digit
                        already_assigned = 1;
                        break;
                    endif
                endfor
                if already_assigned
                    continue;
                endif

                % Count positions where digit is possible
                for col = 1:9
                    if has_candidate(grid.candidates(row, col), digit)
                        count = count + 1;
                        last_col = col;
                    endif
                endfor

                if count == 1
                    [grid, success] = assign(grid, row, last_col, digit);
                    if success == 0
                        return;
                    endif
                    changed = 1;
                elseif count == 0
                    success = 0;
                    return;
                endif
            endfor
        endfor

        % Strategy 3: Hidden singles in columns
        for col = 1:9
            for digit = 1:9
                count = 0;
                last_row = -1;
                % Check if digit is already assigned
                already_assigned = 0;
                for row = 1:9
                    if grid.values(row, col) == digit
                        already_assigned = 1;
                        break;
                    endif
                endfor
                if already_assigned
                    continue;
                endif

                % Count positions where digit is possible
                for row = 1:9
                    if has_candidate(grid.candidates(row, col), digit)
                        count = count + 1;
                        last_row = row;
                    endif
                endfor

                if count == 1
                    [grid, success] = assign(grid, last_row, col, digit);
                    if success == 0
                        return;
                    endif
                    changed = 1;
                elseif count == 0
                    success = 0;
                    return;
                endif
            endfor
        endfor

        % Strategy 4: Hidden singles in boxes
        for box = 0:8
            box_row = floor(box / 3) * 3 + 1;
            box_col = mod(box, 3) * 3 + 1;

            for digit = 1:9
                count = 0;
                last_r = -1;
                last_c = -1;

                % Check if digit is already assigned
                already_assigned = 0;
                for r = box_row:(box_row + 2)
                    for c = box_col:(box_col + 2)
                        if grid.values(r, c) == digit
                            already_assigned = 1;
                            break;
                        endif
                    endfor
                    if already_assigned
                        break;
                    endif
                endfor
                if already_assigned
                    continue;
                endif

                % Count positions where digit is possible
                for r = box_row:(box_row + 2)
                    for c = box_col:(box_col + 2)
                        if has_candidate(grid.candidates(r, c), digit)
                            count = count + 1;
                            last_r = r;
                            last_c = c;
                        endif
                    endfor
                endfor

                if count == 1
                    [grid, success] = assign(grid, last_r, last_c, digit);
                    if success == 0
                        return;
                    endif
                    changed = 1;
                elseif count == 0
                    success = 0;
                    return;
                endif
            endfor
        endfor
    endwhile

    success = 1;
endfunction

% ============================================================================
% SEARCH
% ============================================================================

% Find cell with Minimum Remaining Values
function [found, mrv_row, mrv_col] = find_mrv_cell(grid)
    min_candidates = 10;
    found = 0;
    mrv_row = -1;
    mrv_col = -1;

    for r = 1:9
        for c = 1:9
            if grid.values(r, c) == 0
                num_candidates = count_candidates(grid.candidates(r, c));
                if num_candidates < min_candidates
                    min_candidates = num_candidates;
                    mrv_row = r;
                    mrv_col = c;
                    found = 1;
                endif
            endif
        endfor
    endfor
endfunction

% CP Search with backtracking
function [grid, success] = cp_search(grid)
    % Check if grid is complete
    [found, mrv_row, mrv_col] = find_mrv_cell(grid);

    if found == 0
        % No empty cells - grid is complete
        success = 1;
        return;
    endif

    % Try each candidate for the MRV cell
    candidates = grid.candidates(mrv_row, mrv_col);

    for digit = 1:9
        if has_candidate(candidates, digit)
            % Save grid state for backtracking
            grid_copy = grid;

            % Try assigning this digit
            [grid, success1] = assign(grid, mrv_row, mrv_col, digit);
            if success1 == 1
                % Assignment succeeded, propagate constraints
                [grid, success2] = propagate(grid);
                if success2 == 1
                    % Propagation succeeded, recurse
                    [grid, success3] = cp_search(grid);
                    if success3 == 1
                        success = 1;
                        return;
                    endif
                endif
            endif

            % Failed - restore grid state and try next candidate
            grid = grid_copy;
        endif
    endfor

    % All candidates exhausted - dead end
    success = 0;
endfunction

% ============================================================================
% I/O FUNCTIONS
% ============================================================================

function print_puzzle(grid)
    fprintf('\nPuzzle:\n');
    for row = 1:9
        for col = 1:9
            fprintf('%d ', grid(row, col));
        endfor
        fprintf('\n');
    endfor
endfunction

% ============================================================================
% MAIN FUNCTION
% ============================================================================

function main()
    global cp_iterations;

    args = argv();
    start_time = time();

    for i = 1:length(args)
        filename = args{i};
        if length(filename) > 7 && strcmp(filename(end-6:end), '.matrix') == 1
            % Normalize path for output
            display_path = filename;
            if strncmp(filename, '/app/Matrices/', 14) == 1
                display_path = filename(6:end);
                fprintf('../%s\n', display_path);
            else
                fprintf('%s\n', filename);
            endif

            % Read puzzle
            puzzle = zeros(9, 9);
            fid = fopen(filename, 'r');
            if fid == -1
                error('Cannot open file: %s', filename);
            endif

            line_count = 0;
            while ~feof(fid)
                line = fgetl(fid);
                if ischar(line)
                    line = strtrim(line);
                    if ~isempty(line) && line(1) != '#'
                        values = sscanf(line, '%d');
                        if length(values) == 9
                            line_count = line_count + 1;
                            puzzle(line_count, :) = values';
                            if line_count == 9
                                break;
                            endif
                        endif
                    endif
                endif
            endwhile
            fclose(fid);

            % Print initial puzzle
            for row = 1:9
                for col = 1:9
                    fprintf('%d ', puzzle(row, col));
                endfor
                fprintf('\n');
            endfor
            print_puzzle(puzzle);

            % Initialize grid with puzzle
            grid = init_grid(puzzle);

            % Solve using CP
            cp_iterations = 0;
            [grid, success] = cp_search(grid);

            if success == 1
                print_puzzle(grid.values);
                fprintf('\nSolved in Iterations=%d\n\n', cp_iterations);
            else
                fprintf('No solution found\n');
            endif
        endif
    endfor

    elapsed = time() - start_time;
    fprintf('Seconds to process %.3f\n', elapsed);
endfunction

main();
