1; % Prevent Octave from treating the file as a function file

global iterations;
iterations = 0;

function print_board(board)
  for i = 1:9
    for j = 1:9
      printf("%d ", board(i, j));
    end
    printf("\n");
  end
end

function [row, col] = find_empty(board)
  for r = 1:9
    for c = 1:9
      if board(r, c) == 0
        row = r;
        col = c;
        return;
      end
    end
  end
  row = -1;
  col = -1;
end

function valid = is_valid(board, row, col, num)
  % Row check
  if any(board(row, :) == num)
    valid = false;
    return;
  end

  % Col check
  if any(board(:, col) == num)
    valid = false;
    return;
  end

  % Box check
  box_row = floor((row - 1) / 3) * 3 + 1;
  box_col = floor((col - 1) / 3) * 3 + 1;
  
  subgrid = board(box_row:box_row+2, box_col:box_col+2);
  if any(subgrid(:) == num)
    valid = false;
    return;
  end

  valid = true;
end

function [solved, new_board] = solve(board)
  global iterations;
  
  [row, col] = find_empty(board);
  
  if row == -1
    solved = true;
    new_board = board;
    return;
  end

  for num = 1:9
    iterations = iterations + 1;
    % printf("Checking %d at (%d, %d). Iteration: %d\n", num, row, col, iterations);
    if is_valid(board, row, col, num)
      board(row, col) = num;
      [result, final_board] = solve(board);
      if result
        solved = true;
        new_board = final_board;
        return;
      end
      board(row, col) = 0;
    end
  end

  solved = false;
  new_board = board;
end

% Main execution
args = argv();
if length(args) < 1
  printf("Usage: octave sudoku.m <matrix_file>\n");
  return;
end

filename = args{1};
fid = fopen(filename, 'r');

if fid == -1
  error("Could not open file");
end

board = zeros(9, 9);
row = 1;

while ~feof(fid)
  line = fgetl(fid);
  if ischar(line)
    line = strtrim(line);
    if length(line) > 0 && line(1) ~= '#'
      col = 1;
      for i = 1:length(line)
        char_val = line(i);
        if char_val >= '0' && char_val <= '9'
          if col <= 9
            board(row, col) = str2double(char_val);
            col = col + 1;
          end
        elseif char_val == '.'
           if col <= 9
            board(row, col) = 0;
            col = col + 1;
          end
        end
      end
      row = row + 1;
      if row > 9
        break;
      end
    end
  end
end

fclose(fid);

printf("Puzzle:\n");
print_board(board);

[solved, final_board] = solve(board);

if solved
  printf("Puzzle:\n");
  print_board(final_board);
  printf("Solved in Iterations=%d\n", iterations);
else
  printf("No solution found.\n");
end
