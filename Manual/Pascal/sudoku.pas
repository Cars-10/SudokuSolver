program Sudoku;
{$mode objfpc}{$H+}
uses sysutils, classes;

var
  board: array[0..80] of integer;
  iterations: longint;

procedure ReadBoard(filename: string);
var
  f: text;
  c: char;
  i: integer;
begin
  assign(f, filename);
  reset(f);
  i := 0;
  while not eof(f) do
  begin
    read(f, c);
    if (c >= '0') and (c <= '9') then
    begin
      board[i] := ord(c) - ord('0');
      inc(i);
    end;
  end;
  close(f);
end;

function IsValid(idx, num: integer): boolean;
var
  r, c, br, bc, i, j: integer;
begin
  r := idx div 9;
  c := idx mod 9;
  
  { Check row }
  for i := 0 to 8 do
    if board[r * 9 + i] = num then exit(false);
    
  { Check col }
  for i := 0 to 8 do
    if board[i * 9 + c] = num then exit(false);
    
  { Check box }
  br := (r div 3) * 3;
  bc := (c div 3) * 3;
  for i := 0 to 2 do
    for j := 0 to 2 do
      if board[(br + i) * 9 + (bc + j)] = num then exit(false);
      
  IsValid := true;
end;

function Solve(idx: integer): boolean;
var
  num: integer;
begin
  if idx = 81 then exit(true);
  
  if board[idx] <> 0 then exit(Solve(idx + 1));
  
  for num := 1 to 9 do
  begin
    if IsValid(idx, num) then
    begin
      board[idx] := num;
      inc(iterations);
      if Solve(idx + 1) then exit(true);
      board[idx] := 0;
    end;
  end;
  
  Solve := false;
end;

procedure PrintBoard;
var
  i: integer;
begin
  for i := 0 to 80 do
    write(board[i]);
  writeln;
end;

begin
  if ParamCount < 1 then
  begin
    writeln('Usage: Sudoku <input_file>');
    halt(1);
  end;
  
  ReadBoard(ParamStr(1));
  iterations := 0;
  
  if Solve(0) then
  begin
    writeln('Solved in Iterations=', iterations);
    PrintBoard;
  end
  else
    writeln('No solution found.');
end.
