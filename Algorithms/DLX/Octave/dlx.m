#!/usr/bin/env octave
% Sudoku Solver - Dancing Links (DLX) Implementation in GNU Octave
% Exact cover problem solved with Algorithm X using Dancing Links
% Uses global arrays for node storage (Octave passes structs by value)

global dlx_iterations;
global nodes;  % Array of all nodes
global columns;  % Array of column headers
global node_count;
global col_count;

% ============================================================================
% NODE MANAGEMENT
% ============================================================================

% Node fields: left, right, up, down, column (index), row_id
% Column fields: size, name (stored separately)

function init_globals()
    global nodes columns node_count col_count;
    nodes = [];
    columns = [];
    node_count = 0;
    col_count = 0;
endfunction

function idx = create_node(row_id)
    global nodes node_count;
    node_count = node_count + 1;
    idx = node_count;
    nodes(idx).left = idx;
    nodes(idx).right = idx;
    nodes(idx).up = idx;
    nodes(idx).down = idx;
    nodes(idx).column = 0;
    nodes(idx).row_id = row_id;
endfunction

function idx = create_column(name)
    global nodes columns node_count col_count;

    % Create node for column header
    node_idx = create_node(-1);

    % Create column entry
    col_count = col_count + 1;
    idx = col_count;
    columns(idx).node = node_idx;
    columns(idx).size = 0;
    columns(idx).name = name;

    nodes(node_idx).column = idx;
endfunction

% ============================================================================
% DLX CORE OPERATIONS
% ============================================================================

function cover_column(c)
    global nodes columns;

    col_node = columns(c).node;

    % Remove column header from header list
    nodes(nodes(col_node).right).left = nodes(col_node).left;
    nodes(nodes(col_node).left).right = nodes(col_node).right;

    % For each row in this column
    i = nodes(col_node).down;
    while i != col_node
        % For each node in this row
        j = nodes(i).right;
        while j != i
            % Remove node from its column
            nodes(nodes(j).down).up = nodes(j).up;
            nodes(nodes(j).up).down = nodes(j).down;
            columns(nodes(j).column).size = columns(nodes(j).column).size - 1;
            j = nodes(j).right;
        endwhile
        i = nodes(i).down;
    endwhile
endfunction

function uncover_column(c)
    global nodes columns;

    col_node = columns(c).node;

    % For each row in this column (reverse order)
    i = nodes(col_node).up;
    while i != col_node
        % For each node in this row (reverse order)
        j = nodes(i).left;
        while j != i
            % Restore node to its column
            columns(nodes(j).column).size = columns(nodes(j).column).size + 1;
            nodes(nodes(j).down).up = j;
            nodes(nodes(j).up).down = j;
            j = nodes(j).left;
        endwhile
        i = nodes(i).up;
    endwhile

    % Restore column header
    nodes(nodes(col_node).right).left = col_node;
    nodes(nodes(col_node).left).right = col_node;
endfunction

function c = choose_column(root)
    global nodes columns;

    c = 0;
    min_size = inf;

    j = nodes(root).right;
    while j != root
        col_idx = nodes(j).column;
        if columns(col_idx).size < min_size
            min_size = columns(col_idx).size;
            c = col_idx;
        endif
        j = nodes(j).right;
    endwhile
endfunction

function [success, solution] = search(root, k, solution)
    global dlx_iterations nodes columns;

    dlx_iterations = dlx_iterations + 1;

    % If matrix is empty, solution found
    if nodes(root).right == root
        success = 1;
        return;
    endif

    % Choose column with minimum size
    c = choose_column(root);

    if c == 0 || columns(c).size == 0
        success = 0;
        return;
    endif

    % Cover this column
    cover_column(c);

    % Try each row in this column
    r = nodes(columns(c).node).down;
    while r != columns(c).node
        % Add row to solution
        solution(k) = nodes(r).row_id;

        % Cover all other columns in this row
        j = nodes(r).right;
        while j != r
            cover_column(nodes(j).column);
            j = nodes(j).right;
        endwhile

        % Recurse
        [success, solution] = search(root, k + 1, solution);
        if success == 1
            return;
        endif

        % Backtrack: uncover columns
        j = nodes(r).left;
        while j != r
            uncover_column(nodes(j).column);
            j = nodes(j).left;
        endwhile

        r = nodes(r).down;
    endwhile

    % Uncover column
    uncover_column(c);

    success = 0;
endfunction

% ============================================================================
% MATRIX CONSTRUCTION
% ============================================================================

function root = build_matrix()
    global nodes columns col_count;

    % Create root
    root = create_column("root");
    root_node = columns(root).node;

    % Create 324 columns
    for i = 1:324
        c = create_column(sprintf("C%d", i-1));
    endfor

    % Link columns in circular list
    % root is at index 1, columns 1-324 are at indices 2-325
    for i = 2:325
        if i == 2
            nodes(columns(i).node).left = root_node;
            nodes(root_node).right = columns(i).node;
        else
            nodes(columns(i).node).left = columns(i-1).node;
            nodes(columns(i-1).node).right = columns(i).node;
        endif

        if i == 325
            nodes(columns(i).node).right = root_node;
            nodes(root_node).left = columns(i).node;
        endif
    endfor
endfunction

function first_node = add_row_to_matrix(row_id, cols)
    global nodes columns;

    row_nodes = [];

    % Create nodes for this row
    for i = 1:length(cols)
        col_idx = cols(i) + 2;  % +2 because root is at index 1, columns start at index 2

        node_idx = create_node(row_id);
        nodes(node_idx).column = col_idx;

        % Insert at end of column
        col_node = columns(col_idx).node;
        nodes(node_idx).up = nodes(col_node).up;
        nodes(node_idx).down = col_node;
        nodes(nodes(col_node).up).down = node_idx;
        nodes(col_node).up = node_idx;
        columns(col_idx).size = columns(col_idx).size + 1;

        row_nodes(end+1) = node_idx;
    endfor

    % Link row nodes horizontally
    if length(row_nodes) > 0
        for i = 1:length(row_nodes)
            if i == 1
                nodes(row_nodes(i)).left = row_nodes(end);
                nodes(row_nodes(end)).right = row_nodes(i);
            else
                nodes(row_nodes(i)).left = row_nodes(i-1);
                nodes(row_nodes(i-1)).right = row_nodes(i);
            endif
        endfor
        first_node = row_nodes(1);
    else
        first_node = 0;
    endif
endfunction

% ============================================================================
% PUZZLE PARSING
% ============================================================================

function [root, puzzle, row_info, row_starts] = parse_matrix(filename)
    init_globals();

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

    % Build matrix
    root = build_matrix();

    % Add rows for all possibilities (not just valid ones)
    % Store row metadata
    row_info = cell(729, 1);
    row_starts = zeros(729, 1);
    row_id_idx = 1;

    for row = 0:8
        for col = 0:8
            if puzzle(row+1, col+1) != 0
                % Cell has a clue - create only one row
                digit = puzzle(row+1, col+1);
                c1 = row * 9 + col;
                c2 = 81 + row * 9 + (digit - 1);
                c3 = 162 + col * 9 + (digit - 1);
                box = floor(row / 3) * 3 + floor(col / 3);
                c4 = 243 + box * 9 + (digit - 1);

                row_id = row * 81 + col * 9 + digit;
                first_node = add_row_to_matrix(row_id, [c1, c2, c3, c4]);
                row_info{row_id_idx} = struct('row', row, 'col', col, 'digit', digit);
                row_starts(row_id_idx) = first_node;
                row_id_idx = row_id_idx + 1;
            else
                % Cell is empty - create rows for all possible values
                for digit = 1:9
                    c1 = row * 9 + col;
                    c2 = 81 + row * 9 + (digit - 1);
                    c3 = 162 + col * 9 + (digit - 1);
                    box = floor(row / 3) * 3 + floor(col / 3);
                    c4 = 243 + box * 9 + (digit - 1);

                    row_id = row * 81 + col * 9 + digit;
                    first_node = add_row_to_matrix(row_id, [c1, c2, c3, c4]);
                    row_info{row_id_idx} = struct('row', row, 'col', col, 'digit', digit);
                    row_starts(row_id_idx) = first_node;
                    row_id_idx = row_id_idx + 1;
                endfor
            endif
        endfor
    endfor
endfunction

% ============================================================================
% CLUE COVERING
% ============================================================================

function cover_clues_func(puzzle, row_info, row_starts)
    global nodes;

    for row = 0:8
        for col = 0:8
            if puzzle(row+1, col+1) != 0
                digit = puzzle(row+1, col+1);

                % Find the row for this clue
                for i = 1:length(row_info)
                    if row_starts(i) != 0 && ~isempty(row_info{i})
                        if row_info{i}.row == row && row_info{i}.col == col && row_info{i}.digit == digit
                            % Cover all columns in this row
                            node = row_starts(i);
                            curr = node;
                            while true
                                cover_column(nodes(curr).column);
                                curr = nodes(curr).right;
                                if curr == node
                                    break;
                                endif
                            endwhile
                            break;
                        endif
                    endif
                endfor
            endif
        endfor
    endfor
endfunction

% ============================================================================
% SOLUTION EXTRACTION
% ============================================================================

function grid = extract_solution(solution, num_rows)
    grid = zeros(9, 9);

    for i = 1:num_rows
        row_id = solution(i);

        if row_id > 0
            digit = mod(row_id, 9);
            if digit == 0
                digit = 9;
            endif

            temp = floor((row_id - digit) / 9);
            col = mod(temp, 9);
            row = floor(temp / 9);

            if row >= 0 && row < 9 && col >= 0 && col < 9
                grid(row+1, col+1) = digit;
            endif
        endif
    endfor
endfunction

% ============================================================================
% I/O
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
% MAIN
% ============================================================================

function main()
    global dlx_iterations columns;

    args = argv();
    start_time = time();

    for i = 1:length(args)
        filename = args{i};
        if length(filename) > 7 && strcmp(filename(end-6:end), '.matrix') == 1
            % Print filename
            display_path = filename;
            if strncmp(filename, '/app/Matrices/', 14) == 1
                display_path = filename(6:end);
                fprintf('../%s\n', display_path);
            else
                fprintf('%s\n', filename);
            endif

            % Parse and build matrix
            [root, puzzle, row_info, row_starts] = parse_matrix(filename);

            % Print initial puzzle
            for row = 1:9
                for col = 1:9
                    fprintf('%d ', puzzle(row, col));
                endfor
                fprintf('\n');
            endfor
            print_puzzle(puzzle);

            % Cover pre-filled clues
            cover_clues_func(puzzle, row_info, row_starts);

            % Solve
            dlx_iterations = 0;
            solution = zeros(81, 1);
            [success, solution] = search(columns(root).node, 1, solution);

            if success == 1
                % Start with original puzzle (includes clues)
                grid = puzzle;
                % Add solution values
                for i = 1:81
                    if solution(i) > 0
                        row_id = solution(i);
                        digit = mod(row_id, 9);
                        if digit == 0
                            digit = 9;
                        endif
                        temp = floor((row_id - digit) / 9);
                        col = mod(temp, 9);
                        row = floor(temp / 9);
                        if row >= 0 && row < 9 && col >= 0 && col < 9
                            grid(row+1, col+1) = digit;
                        endif
                    endif
                endfor
                print_puzzle(grid);
                fprintf('\nSolved in Iterations=%d\n\n', dlx_iterations);
            else
                fprintf('No solution found\n');
            endif
        endif
    endfor

    elapsed = time() - start_time;
    fprintf('Seconds to process %.3f\n', elapsed);
endfunction

main();
