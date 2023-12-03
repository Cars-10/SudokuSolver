program SudokuSolver;
uses SysUtils, Classes;

const
  N = 9;

type
  TBoard = array[1..N * N] of Integer;

var
  board: TBoard;
  iterations: Int64;

function ReadBoardFromFile(const FileName: string): TBoard;
var
  f: TextFile;
  i, j, idx: Integer;
  line, snum: string;
  ss: TStringList;
begin
  Assign(f, FileName);
  Reset(f);
  idx := 1;
  ss := TStringList.Create;
  while not Eof(f) do
  begin
    Readln(f, line);
    if (line = '') or (line[1] = '#') then Continue;
    ss.Clear;
    ss.Delimiter := ' ';
    ss.DelimitedText := line;
    for snum in ss do
    begin
      if TryStrToInt(snum, j) then
      begin
        board[idx] := j;
        Inc(idx);
      end;
    end;
  end;
  ss.Free;
  Close(f);
  Result := board;
end;

function ComputeComplexity(const board: TBoard): Integer;
var
  i, count: Integer;
begin
  count := 0;
  for i := 1 to N * N do
    if board[i] = 0 then
      Inc(count);
  Result := count;
end;

procedure PrintBoard(const board: TBoard);
var
  i, j: Integer;
begin
  for i := 1 to N do
  begin
    for j := 1 to N do
      Write(board[(i - 1) * N + j], ' ');
    Writeln;
  end;
end;

function IsValidPlacement(const board: TBoard; row, col, num: Integer): Boolean;
var
  i, j, startRow, startCol: Integer;
begin
  for i := 1 to N do
    if (board[(row - 1) * N + i] = num) or (board[(i - 1) * N + col] = num) then
      Exit(False);
  startRow := ((row - 1) div 3) * 3 + 1;
  startCol := ((col - 1) div 3) * 3 + 1;
  for i := 0 to 2 do
    for j := 0 to 2 do
      if board[(startRow + i - 1) * N + (startCol + j)] = num then
        Exit(False);
  Result := True;
end;

function SolveSudoku(var board: TBoard; var iterations: Int64): Boolean;
var
  row, col, num: Integer;
  isEmpty: Boolean;
begin
  isEmpty := True;
  for row := 1 to N do
  begin
    for col := 1 to N do
      if board[(row - 1) * N + col] = 0 then
      begin
        isEmpty := False;
        Break;
      end;
    if not isEmpty then Break;
  end;

  if isEmpty then
    Exit(True);

  for num := 1 to N do
  begin
    if IsValidPlacement(board, row, col, num) then
    begin
      board[(row - 1) * N + col] := num;
      Inc(iterations);
      if SolveSudoku(board, iterations) then
        Exit(True);
      board[(row - 1) * N + col] := 0;
    end;
end;