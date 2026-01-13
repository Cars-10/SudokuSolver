program CPSudoku;
{$mode objfpc}{$H+}
uses SysUtils;

type
  { Candidate tracking using bitsets (9 bits for digits 1-9) }
  TCandidateSet = Word;

  { Grid structure with assigned values and candidate tracking }
  TCPGrid = record
    values: array[0..8, 0..8] of Integer;        { Assigned values (0 = empty) }
    candidates: array[0..8, 0..8] of TCandidateSet;  { Possible values per cell (bitset) }
  end;

  { Peers array - 20 peers per cell, each with row and col }
  TPeersArray = array[0..19, 0..1] of Integer;

var
  { Global iteration counter }
  cp_iterations: Int64;

  { Puzzle data }
  puzzle: array[0..8, 0..8] of Integer;

{ Candidate manipulation functions }
function HasCandidate(cs: TCandidateSet; digit: Integer): Boolean;
begin
  Result := (cs and (1 shl digit)) <> 0;
end;

procedure RemoveCandidate(var cs: TCandidateSet; digit: Integer);
begin
  cs := cs and not (1 shl digit);
end;

function CountCandidates(cs: TCandidateSet): Integer;
var
  count, i: Integer;
begin
  count := 0;
  for i := 1 to 9 do
    if HasCandidate(cs, i) then
      Inc(count);
  Result := count;
end;

function GetFirstCandidate(cs: TCandidateSet): Integer;
var
  digit: Integer;
begin
  for digit := 1 to 9 do
    if HasCandidate(cs, digit) then
      Exit(digit);
  Result := 0;
end;

{ Get all 20 peers for a cell (row, col, box) }
procedure GetPeers(row, col: Integer; var peers: TPeersArray);
var
  idx, r, c, box_row, box_col: Integer;
begin
  idx := 0;

  { Same row (9 cells minus self = 8) }
  for c := 0 to 8 do
    if c <> col then
    begin
      peers[idx][0] := row;
      peers[idx][1] := c;
      Inc(idx);
    end;

  { Same column (9 cells minus self = 8) }
  for r := 0 to 8 do
    if r <> row then
    begin
      peers[idx][0] := r;
      peers[idx][1] := col;
      Inc(idx);
    end;

  { Same 3x3 box (9 cells minus self minus already counted = 4) }
  box_row := (row div 3) * 3;
  box_col := (col div 3) * 3;
  for r := box_row to box_row + 2 do
    for c := box_col to box_col + 2 do
      if (r <> row) and (c <> col) then
      begin
        peers[idx][0] := r;
        peers[idx][1] := c;
        Inc(idx);
      end;
end;

{ Forward declarations }
function Eliminate(var grid: TCPGrid; row, col, digit: Integer): Boolean; forward;

{ Assign a digit to a cell and propagate constraints }
function Assign(var grid: TCPGrid; row, col, digit: Integer): Boolean;
var
  peers: TPeersArray;
  i, peer_row, peer_col: Integer;
begin
  { Increment iteration counter (this is our benchmark metric) }
  Inc(cp_iterations);

  { Set value }
  grid.values[row, col] := digit;
  grid.candidates[row, col] := 1 shl digit;

  { Eliminate digit from all peers }
  GetPeers(row, col, peers);

  for i := 0 to 19 do
  begin
    peer_row := peers[i][0];
    peer_col := peers[i][1];

    if not Eliminate(grid, peer_row, peer_col, digit) then
      Exit(False);  { Contradiction in peer elimination }
  end;

  Result := True;
end;

{ Eliminate a digit from a cell's candidates }
function Eliminate(var grid: TCPGrid; row, col, digit: Integer): Boolean;
var
  remaining, last_digit: Integer;
begin
  { Check if digit is already eliminated }
  if not HasCandidate(grid.candidates[row, col], digit) then
    Exit(True);  { Already eliminated, no change }

  { Remove digit from candidates }
  RemoveCandidate(grid.candidates[row, col], digit);

  { Check for contradiction (no candidates left) }
  remaining := CountCandidates(grid.candidates[row, col]);
  if remaining = 0 then
    Exit(False);  { Contradiction }

  { If only one candidate left, assign it (singleton elimination) }
  if (remaining = 1) and (grid.values[row, col] = 0) then
  begin
    last_digit := GetFirstCandidate(grid.candidates[row, col]);
    if not Assign(grid, row, col, last_digit) then
      Exit(False);  { Assignment caused contradiction }
  end;

  Result := True;
end;

{ Initialize grid from puzzle }
procedure InitGrid(var grid: TCPGrid);
var
  row, col, digit: Integer;
begin
  for row := 0 to 8 do
    for col := 0 to 8 do
    begin
      if puzzle[row, col] = 0 then
      begin
        { Empty cell: set all candidates 1-9 (bits 1-9 set) }
        grid.values[row, col] := 0;
        grid.candidates[row, col] := $3FE;  { Binary: 0011 1111 1110 (bits 1-9) }
      end
      else
      begin
        { Given clue: set single value }
        digit := puzzle[row, col];
        grid.values[row, col] := digit;
        grid.candidates[row, col] := 1 shl digit;
      end;
    end;
end;

{ Propagate constraints until fixpoint }
function Propagate(var grid: TCPGrid): Boolean;
label
  next_box_digit, found_box_digit;
var
  changed: Boolean;
  row, col, digit, num_candidates: Integer;
  count, last_row, last_col: Integer;
  box, box_row, box_col, r, c: Integer;
  found: Boolean;
begin
  changed := True;

  while changed do
  begin
    changed := False;

    { Strategy 1: Singleton elimination }
    { If a cell has only one candidate, assign it }
    for row := 0 to 8 do
      for col := 0 to 8 do
        if grid.values[row, col] = 0 then
        begin
          num_candidates := CountCandidates(grid.candidates[row, col]);
          if num_candidates = 0 then
            Exit(False);  { Contradiction }
          if num_candidates = 1 then
          begin
            digit := GetFirstCandidate(grid.candidates[row, col]);
            if not Assign(grid, row, col, digit) then
              Exit(False);  { Assignment caused contradiction }
            changed := True;
          end;
        end;

    { Strategy 2: Hidden singles }

    { Check rows }
    for row := 0 to 8 do
      for digit := 1 to 9 do
      begin
        count := 0;
        last_col := -1;
        for col := 0 to 8 do
        begin
          if grid.values[row, col] = digit then
          begin
            count := 0;  { Already assigned }
            Break;
          end;
          if HasCandidate(grid.candidates[row, col], digit) then
          begin
            Inc(count);
            last_col := col;
          end;
        end;

        if count = 1 then
        begin
          if not Assign(grid, row, last_col, digit) then
            Exit(False);
          changed := True;
        end
        else if count = 0 then
        begin
          { Check if digit is already assigned in this row }
          found := False;
          for col := 0 to 8 do
            if grid.values[row, col] = digit then
            begin
              found := True;
              Break;
            end;
          if not found then
            Exit(False);  { Digit cannot be placed anywhere in row }
        end;
      end;

    { Check columns }
    for col := 0 to 8 do
      for digit := 1 to 9 do
      begin
        count := 0;
        last_row := -1;
        for row := 0 to 8 do
        begin
          if grid.values[row, col] = digit then
          begin
            count := 0;  { Already assigned }
            Break;
          end;
          if HasCandidate(grid.candidates[row, col], digit) then
          begin
            Inc(count);
            last_row := row;
          end;
        end;

        if count = 1 then
        begin
          if not Assign(grid, last_row, col, digit) then
            Exit(False);
          changed := True;
        end
        else if count = 0 then
        begin
          { Check if digit is already assigned in this column }
          found := False;
          for row := 0 to 8 do
            if grid.values[row, col] = digit then
            begin
              found := True;
              Break;
            end;
          if not found then
            Exit(False);  { Digit cannot be placed anywhere in column }
        end;
      end;

    { Check boxes }
    for box := 0 to 8 do
    begin
      box_row := (box div 3) * 3;
      box_col := (box mod 3) * 3;

      for digit := 1 to 9 do
      begin
        count := 0;
        last_row := -1;
        last_col := -1;

        for r := box_row to box_row + 2 do
          for c := box_col to box_col + 2 do
          begin
            if grid.values[r, c] = digit then
            begin
              count := 0;  { Already assigned }
              goto next_box_digit;
            end;
            if HasCandidate(grid.candidates[r, c], digit) then
            begin
              Inc(count);
              last_row := r;
              last_col := c;
            end;
          end;

        if count = 1 then
        begin
          if not Assign(grid, last_row, last_col, digit) then
            Exit(False);
          changed := True;
        end
        else if count = 0 then
        begin
          { Check if digit is already assigned in this box }
          found := False;
          for r := box_row to box_row + 2 do
            for c := box_col to box_col + 2 do
              if grid.values[r, c] = digit then
              begin
                found := True;
                goto found_box_digit;
              end;
          found_box_digit:
          if not found then
            Exit(False);  { Digit cannot be placed anywhere in box }
        end;

        next_box_digit:
      end;
    end;
  end;

  Result := True;  { Success - reached fixpoint }
end;

{ Find cell with minimum remaining values (MRV heuristic) }
function FindMrvCell(const grid: TCPGrid; var row, col: Integer): Boolean;
var
  min_candidates, num_candidates, r, c: Integer;
  found: Boolean;
begin
  min_candidates := 10;  { More than 9, so any cell will be smaller }
  found := False;

  for r := 0 to 8 do
    for c := 0 to 8 do
      if grid.values[r, c] = 0 then
      begin
        num_candidates := CountCandidates(grid.candidates[r, c]);
        if num_candidates < min_candidates then
        begin
          min_candidates := num_candidates;
          row := r;
          col := c;
          found := True;
        end;
      end;

  Result := found;  { False if no empty cells (grid complete), True if cell found }
end;

{ CP Search with backtracking }
function CPSearch(var grid: TCPGrid; var solution: array of Integer): Boolean;
var
  mrv_row, mrv_col, digit: Integer;
  candidates: TCandidateSet;
  grid_copy: TCPGrid;
  r, c: Integer;
begin
  { Base case: check if grid is complete }
  if not FindMrvCell(grid, mrv_row, mrv_col) then
  begin
    { No empty cells - grid is complete, extract solution }
    for r := 0 to 8 do
      for c := 0 to 8 do
        solution[r * 9 + c] := grid.values[r, c];
    Exit(True);  { Solved }
  end;

  { Recursive case: try each candidate for the MRV cell }
  candidates := grid.candidates[mrv_row, mrv_col];

  for digit := 1 to 9 do
    if HasCandidate(candidates, digit) then
    begin
      { Save grid state for backtracking }
      grid_copy := grid;

      { Try assigning this digit }
      if Assign(grid, mrv_row, mrv_col, digit) then
      begin
        { Assignment succeeded, propagate constraints }
        if Propagate(grid) then
        begin
          { Propagation succeeded, recurse }
          if CPSearch(grid, solution) then
            Exit(True);  { Found solution }
        end;
      end;

      { Failed - restore grid state and try next candidate }
      grid := grid_copy;
    end;

  { All candidates exhausted - dead end }
  Result := False;
end;

{ Print puzzle }
procedure PrintPuzzle;
var
  r, c: Integer;
begin
  WriteLn;
  WriteLn('Puzzle:');
  for r := 0 to 8 do
  begin
    for c := 0 to 8 do
      Write(puzzle[r, c], ' ');
    WriteLn;
  end;
end;

procedure PrintSolution(const solution: array of Integer);
var
  r, c: Integer;
begin
  WriteLn;
  WriteLn('Puzzle:');
  for r := 0 to 8 do
  begin
    for c := 0 to 8 do
      Write(solution[r * 9 + c], ' ');
    WriteLn;
  end;
end;

{ Read matrix file }
function ReadMatrixFile(const filename: string): Boolean;
var
  f: TextFile;
  line: string;
  r, c, idx: Integer;
  display_path: string;
begin
  Result := False;

  { Print filename (normalize /app/Matrices to ../Matrices) }
  if Pos('/app/Matrices/', filename) = 1 then
  begin
    display_path := '../' + Copy(filename, 6, Length(filename));
    WriteLn(display_path);
  end
  else
    WriteLn(filename);

  AssignFile(f, filename);
  try
    Reset(f);
  except
    WriteLn(StdErr, 'Error opening file: ', filename);
    Exit;
  end;

  r := 0;
  while (not Eof(f)) and (r < 9) do
  begin
    ReadLn(f, line);
    line := Trim(line);

    { Skip comments and empty lines }
    if (line = '') then
      Continue;
    if (Length(line) > 0) and (line[1] = '#') then
      Continue;

    { Parse 9 space-separated integers }
    c := 0;
    idx := 1;
    while (idx <= Length(line)) and (c < 9) do
    begin
      { Skip spaces }
      while (idx <= Length(line)) and (line[idx] = ' ') do
        Inc(idx);

      if idx <= Length(line) then
      begin
        puzzle[r, c] := Ord(line[idx]) - Ord('0');
        Write(puzzle[r, c], ' ');
        Inc(c);
        Inc(idx);
      end;
    end;
    WriteLn;
    Inc(r);
  end;

  CloseFile(f);
  Result := True;
end;

{ Main program }
var
  i: Integer;
  filename: string;
  grid: TCPGrid;
  solution: array[0..80] of Integer;
  solved: Boolean;
  start_time, end_time: TDateTime;
begin
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'Usage: cp <matrix_file>');
    Halt(1);
  end;

  start_time := Now;

  for i := 1 to ParamCount do
  begin
    filename := ParamStr(i);
    if Pos('.matrix', filename) > 0 then
    begin
      if ReadMatrixFile(filename) then
      begin
        PrintPuzzle;

        { Initialize CP grid }
        InitGrid(grid);

        { Apply initial propagation }
        cp_iterations := 0;
        if not Propagate(grid) then
        begin
          WriteLn;
          WriteLn('No solution found (contradiction during initial propagation)');
        end
        else
        begin
          { Run search }
          FillChar(solution, SizeOf(solution), 0);
          solved := CPSearch(grid, solution);

          if solved then
          begin
            PrintSolution(solution);
            WriteLn;
            WriteLn('Solved in Iterations=', cp_iterations);
            WriteLn;
          end
          else
            WriteLn('No solution found');
        end;
      end;
    end;
  end;

  end_time := Now;
  WriteLn(Format('Seconds to process %.3f', [(end_time - start_time) * 86400]));
end.
