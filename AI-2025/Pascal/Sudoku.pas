program Sudoku;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Unix;

var
  Board: array[0..8, 0..8] of Integer;
  Iterations: LongInt;
  Filename: String;

procedure ReadMatrix(const FName: String);
var
  F: TextFile;
  Line: String;
  Row, Col, Code: Integer;
  Parts: TStringList;
  I: Integer;
begin
  AssignFile(F, FName);
  Reset(F);
  Row := 0;
  Parts := TStringList.Create;
  try
    while (not Eof(F)) and (Row < 9) do
    begin
      ReadLn(F, Line);
      if (Length(Line) > 0) and (Line[1] <> '#') then
      begin
        Parts.Clear;
        Parts.Delimiter := ' ';
        Parts.DelimitedText := Line;
        Col := 0;
        for I := 0 to Parts.Count - 1 do
        begin
          if (Parts[I] <> '') and (Col < 9) then
          begin
            Val(Parts[I], Board[Row, Col], Code);
            Inc(Col);
          end;
        end;
        if Col = 9 then Inc(Row);
      end;
    end;
  finally
    Parts.Free;
    CloseFile(F);
  end;
end;

procedure PrintBoard;
var
  R, C: Integer;
begin
  for R := 0 to 8 do
  begin
    for C := 0 to 8 do
      Write(Board[R, C], ' ');
    WriteLn;
  end;
end;

function IsValid(Row, Col, Num: Integer): Boolean;
var
  R, C, BoxRow, BoxCol: Integer;
begin
  Result := True;
  
  // Row check
  for C := 0 to 8 do
    if Board[Row, C] = Num then Exit(False);
    
  // Col check
  for R := 0 to 8 do
    if Board[R, Col] = Num then Exit(False);
    
  // Box check
  BoxRow := (Row div 3) * 3;
  BoxCol := (Col div 3) * 3;
  for R := 0 to 2 do
    for C := 0 to 2 do
      if Board[BoxRow + R, BoxCol + C] = Num then Exit(False);
end;

function FindEmpty(var Row, Col: Integer): Boolean;
var
  R, C: Integer;
begin
  for R := 0 to 8 do
    for C := 0 to 8 do
      if Board[R, C] = 0 then
      begin
        Row := R;
        Col := C;
        Exit(True);
      end;
  Result := False;
end;

function SolveSudoku(Depth: Integer): Boolean;
var
  Row, Col, Num: Integer;
begin
  if not FindEmpty(Row, Col) then Exit(True);

  for Num := 1 to 9 do
  begin
    Inc(Iterations);
    if IsValid(Row, Col, Num) then
    begin
      Board[Row, Col] := Num;
      if SolveSudoku(Depth + 1) then Exit(True);
      Board[Row, Col] := 0;
    end;
  end;
  
  Result := False;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: sudoku input_file');
    Halt(1);
  end;

  Filename := ParamStr(1);
  WriteLn(Filename);
  
  ReadMatrix(Filename);
  
  WriteLn('Puzzle:');
  PrintBoard;
  
  Iterations := 0;
  if SolveSudoku(0) then
  begin
    WriteLn('Puzzle:');
    PrintBoard;
    WriteLn('Solved in Iterations=', Iterations);
  end
  else
    WriteLn('No solution found.');
end.
