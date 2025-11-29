% Octave Sudoku Solver

args = argv();
for i = 1:length(args)
    filename = args{i};
    if (length(filename) > 7 && strcmp(filename(end-6:end), ".matrix"))
        disp(filename);
        
        % Read file
        % Octave load is easy if file is just numbers
        % But we have comments.
        fid = fopen(filename, 'r');
        puzzle = [];
        row = 1;
        while ~feof(fid)
            line = fgetl(fid);
            if ischar(line)
                line = strtrim(line);
                if ~isempty(line) && line(1) ~= '#'
                    vals = sscanf(line, '%d');
                    if length(vals) == 9
                        puzzle(row, :) = vals';
                        row = row + 1;
                    end
                end
            end
        end
        fclose(fid);
        
        disp('Puzzle:');
        disp(puzzle);
        
        global count;
        count = 0;
        
        tic;
        [solved, solution] = solve_sudoku(puzzle);
        t = toc;
        
        if solved
            disp('Puzzle:'); % Re-print solution in format?
            % Octave prints matrix nicely by default, but let's match format if needed
            for r=1:9
                fprintf('%d %d %d %d %d %d %d %d %d \n', solution(r,:));
            end
            fprintf('\nSolved in Iterations=%d\n\n', count);
        else
            disp('No solution found');
        end
        fprintf('Seconds to process %.3f\n', t);
    end
end

function [solved, puzzle] = solve_sudoku(puzzle)
    global count;
    
    % Find empty
    [r, c] = find(puzzle == 0, 1);
    
    if isempty(r)
        solved = true;
        return;
    end
    
    for val = 1:9
        count = count + 1;
        if is_possible(puzzle, r, c, val)
            puzzle(r, c) = val;
            [s, p] = solve_sudoku(puzzle);
            if s
                solved = true;
                puzzle = p;
                return;
            end
            puzzle(r, c) = 0;
        end
    end
    
    solved = false;
end

function possible = is_possible(puzzle, r, c, val)
    if any(puzzle(r, :) == val)
        possible = false;
        return;
    end
    if any(puzzle(:, c) == val)
        possible = false;
        return;
    end
    
    r0 = floor((r-1)/3)*3 + 1;
    c0 = floor((c-1)/3)*3 + 1;
    
    block = puzzle(r0:r0+2, c0:c0+2);
    if any(block(:) == val)
        possible = false;
        return;
    end
    possible = true;
end
