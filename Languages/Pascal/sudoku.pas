program Sudoku;
{$mode objfpc}{$H+}
uses SysUtils;

var
  puzzle: array[0..8, 0..8] of Integer;
  count: LongInt;

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

function ReadMatrix(const filename: string): Boolean;
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

function IsValid(row, col, val: Integer): Boolean;
var
  i, box_row, box_col, br, bc: Integer;
begin
  { Check row }
  for i := 0 to 8 do
    if puzzle[row, i] = val then
      Exit(False);

  { Check column }
  for i := 0 to 8 do
    if puzzle[i, col] = val then
      Exit(False);

  { Check 3x3 box }
  box_row := (row div 3) * 3;
  box_col := (col div 3) * 3;
  for br := 0 to 2 do
    for bc := 0 to 2 do
      if puzzle[box_row + br, box_col + bc] = val then
        Exit(False);

  Result := True;
end;

function Solve: Boolean;
var
  r, c, row, col, val: Integer;
begin
  { Find first empty cell (row-major order) }
  row := -1;
  col := -1;
  for r := 0 to 8 do
  begin
    for c := 0 to 8 do
    begin
      if puzzle[r, c] = 0 then
      begin
        row := r;
        col := c;
        Break;
      end;
    end;
    if row <> -1 then
      Break;
  end;


  { If no empty cell, puzzle is solved }
  if row = -1 then
  begin
    PrintPuzzle;
    WriteLn;
    WriteLn('Solved in Iterations=', count);
    WriteLn;
    Exit(True);
  end;

  { Try values 1-9 in order }
  for val := 1 to 9 do
  begin
    Inc(count);  { Count EVERY attempt }

    if IsValid(row, col, val) then
    begin
      puzzle[row, col] := val;

      if Solve() then
        Exit(True);

      puzzle[row, col] := 0;  { Backtrack }
    end;
  end;

  Result := False;
end;

var
  i: Integer;
  filename: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'Usage: Sudoku <matrix_file>');
    Halt(1);
  end;

  for i := 1 to ParamCount do
  begin
    filename := ParamStr(i);
    if Pos('.matrix', filename) > 0 then
    begin
      if ReadMatrix(filename) then
      begin
        PrintPuzzle;
        count := 0;
        if not Solve() then
          WriteLn('No solution found');
      end;
    end;
  end;
end.
