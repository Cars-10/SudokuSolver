program DLXSudoku;
{$mode objfpc}{$H+}
uses SysUtils;

type
  { Forward declarations }
  PDlxNode = ^TDlxNode;
  PDlxColumn = ^TDlxColumn;

  { DLX Node - basic node in the dancing links matrix }
  TDlxNode = record
    up, down, left, right: PDlxNode;
    column: PDlxColumn;
    row_id: Integer;
  end;

  { DLX Column - column header with size tracking }
  TDlxColumn = record
    node: TDlxNode;  { Inherits from DlxNode }
    size: Integer;
    name: String[16];
  end;

  { Row metadata to map DLX rows back to Sudoku (row, col, num) }
  TRowInfo = record
    row, col, num: Integer;
  end;

var
  { Global DLX structures }
  root: PDlxColumn;
  columns: array[0..323] of PDlxColumn;
  nodes: array of TDlxNode;
  node_count: Integer;
  row_info: array[0..728] of TRowInfo;
  row_starts: array[0..728] of PDlxNode;

  { Puzzle data }
  puzzle: array[0..8, 0..8] of Integer;
  solution_grid: array[0..8, 0..8] of Integer;

  { Iteration counter }
  dlx_iterations: Integer;

{ Calculate constraint column indices }
function GetPositionCol(r, c: Integer): Integer;
begin
  Result := r * 9 + c;
end;

function GetRowCol(r, n: Integer): Integer;
begin
  Result := 81 + r * 9 + (n - 1);
end;

function GetColCol(c, n: Integer): Integer;
begin
  Result := 162 + c * 9 + (n - 1);
end;

function GetBoxCol(r, c, n: Integer): Integer;
var
  box: Integer;
begin
  box := (r div 3) * 3 + (c div 3);
  Result := 243 + box * 9 + (n - 1);
end;

{ Initialize DLX matrix structure }
procedure InitDlxMatrix;
var
  i: Integer;
begin
  { Allocate root column }
  New(root);
  FillChar(root^, SizeOf(TDlxColumn), 0);
  root^.name := 'root';
  root^.node.left := @root^.node;
  root^.node.right := @root^.node;
  root^.node.up := @root^.node;
  root^.node.down := @root^.node;
  root^.node.column := root;
  root^.node.row_id := -1;
  root^.size := 0;

  { Allocate 324 column headers }
  for i := 0 to 323 do
  begin
    New(columns[i]);
    FillChar(columns[i]^, SizeOf(TDlxColumn), 0);
    columns[i]^.name := 'C' + IntToStr(i);
    columns[i]^.size := 0;

    { Initialize as circular list }
    columns[i]^.node.up := @columns[i]^.node;
    columns[i]^.node.down := @columns[i]^.node;
    columns[i]^.node.column := columns[i];
    columns[i]^.node.row_id := -1;

    { Link into header list }
    columns[i]^.node.left := root^.node.left;
    columns[i]^.node.right := @root^.node;
    root^.node.left^.right := @columns[i]^.node;
    root^.node.left := @columns[i]^.node;
  end;

  { Allocate node pool - 729 rows * 4 constraints each }
  SetLength(nodes, 729 * 4);
  node_count := 0;
end;

{ Add a node to the DLX matrix }
function AddNode(col: PDlxColumn; row_id: Integer): PDlxNode;
var
  node: PDlxNode;
begin
  if node_count >= Length(nodes) then
  begin
    WriteLn(StdErr, 'ERROR: Exceeded maximum node count');
    Halt(1);
  end;

  node := @nodes[node_count];
  Inc(node_count);

  node^.column := col;
  node^.row_id := row_id;

  { Insert at end of column's circular list }
  node^.down := @col^.node;
  node^.up := col^.node.up;
  col^.node.up^.down := node;
  col^.node.up := node;
  Inc(col^.size);

  Result := node;
end;

{ Build a DLX row for Sudoku cell (r,c) with value n }
procedure BuildDlxRow(r, c, n, row_id: Integer);
var
  n1, n2, n3, n4: PDlxNode;
begin
  { Store row metadata }
  row_info[row_id].row := r;
  row_info[row_id].col := c;
  row_info[row_id].num := n;

  { Create nodes for the 4 constraints }
  n1 := AddNode(columns[GetPositionCol(r, c)], row_id);
  n2 := AddNode(columns[GetRowCol(r, n)], row_id);
  n3 := AddNode(columns[GetColCol(c, n)], row_id);
  n4 := AddNode(columns[GetBoxCol(r, c, n)], row_id);

  { Link nodes horizontally in circular list }
  n1^.right := n2;
  n2^.right := n3;
  n3^.right := n4;
  n4^.right := n1;

  n1^.left := n4;
  n2^.left := n1;
  n3^.left := n2;
  n4^.left := n3;

  { Store first node for this row }
  row_starts[row_id] := n1;
end;

{ Build the complete DLX matrix from the puzzle }
procedure BuildDlxMatrixFromPuzzle;
var
  r, c, n, row_id: Integer;
begin
  row_id := 0;

  for r := 0 to 8 do
  begin
    for c := 0 to 8 do
    begin
      if puzzle[r, c] <> 0 then
      begin
        { Cell has a clue - create only one row for that value }
        BuildDlxRow(r, c, puzzle[r, c], row_id);
        Inc(row_id);
      end
      else
      begin
        { Cell is empty - create rows for all possible values }
        for n := 1 to 9 do
        begin
          BuildDlxRow(r, c, n, row_id);
          Inc(row_id);
        end;
      end;
    end;
  end;
end;

{ Cover a column in the DLX matrix }
procedure DlxCoverColumn(c: PDlxColumn);
var
  col_node, row_node, right_node: PDlxNode;
begin
  col_node := @c^.node;

  { Remove column header from the header list }
  col_node^.right^.left := col_node^.left;
  col_node^.left^.right := col_node^.right;

  { For each row in this column }
  row_node := col_node^.down;
  while row_node <> col_node do
  begin
    { For each node in this row (excluding the column itself) }
    right_node := row_node^.right;
    while right_node <> row_node do
    begin
      { Remove this node from its column }
      right_node^.down^.up := right_node^.up;
      right_node^.up^.down := right_node^.down;
      Dec(right_node^.column^.size);
      right_node := right_node^.right;
    end;
    row_node := row_node^.down;
  end;
end;

{ Uncover a column (exact reverse of cover) }
procedure DlxUncoverColumn(c: PDlxColumn);
var
  col_node, row_node, left_node: PDlxNode;
begin
  col_node := @c^.node;

  { For each row in this column (in reverse order) }
  row_node := col_node^.up;
  while row_node <> col_node do
  begin
    { For each node in this row (in reverse order) }
    left_node := row_node^.left;
    while left_node <> row_node do
    begin
      { Restore this node to its column }
      Inc(left_node^.column^.size);
      left_node^.down^.up := left_node;
      left_node^.up^.down := left_node;
      left_node := left_node^.left;
    end;
    row_node := row_node^.up;
  end;

  { Restore column header to the header list }
  col_node^.right^.left := col_node;
  col_node^.left^.right := col_node;
end;

{ Choose column with minimum size (Knuth's S heuristic) }
function ChooseColumn(root: PDlxColumn): PDlxColumn;
var
  root_node, col_node: PDlxNode;
  best: PDlxColumn;
  min_size: Integer;
  col: PDlxColumn;
begin
  root_node := @root^.node;
  best := nil;
  min_size := High(Integer);

  col_node := root_node^.right;
  while col_node <> root_node do
  begin
    col := PDlxColumn(col_node);
    if col^.size < min_size then
    begin
      min_size := col^.size;
      best := col;
    end;
    col_node := col_node^.right;
  end;

  Result := best;
end;

{ DLX Search - Algorithm X with Dancing Links }
function DlxSearch(root: PDlxColumn; k: Integer; var solution: array of Integer): Boolean;
var
  root_node, row_node, right_node, left_node: PDlxNode;
  col: PDlxColumn;
begin
  { Count every search call }
  Inc(dlx_iterations);

  root_node := @root^.node;

  { If matrix is empty, we found a solution }
  if root_node^.right = root_node then
    Exit(True);

  { Choose column with minimum size }
  col := ChooseColumn(root);

  { If column has no rows, no solution possible }
  if col^.size = 0 then
    Exit(False);

  { Cover this column }
  DlxCoverColumn(col);

  { Try each row in this column }
  row_node := col^.node.down;
  while row_node <> @col^.node do
  begin
    { Add row to partial solution }
    solution[k] := row_node^.row_id;

    { Cover all other columns in this row }
    right_node := row_node^.right;
    while right_node <> row_node do
    begin
      DlxCoverColumn(right_node^.column);
      right_node := right_node^.right;
    end;

    { Recurse }
    if DlxSearch(root, k + 1, solution) then
      Exit(True);

    { Backtrack: uncover all columns in this row }
    left_node := row_node^.left;
    while left_node <> row_node do
    begin
      DlxUncoverColumn(left_node^.column);
      left_node := left_node^.left;
    end;

    row_node := row_node^.down;
  end;

  { Uncover column }
  DlxUncoverColumn(col);

  Result := False;
end;

{ Cover given clues (pre-selected rows) }
procedure CoverClues;
var
  r, c, n, row_id: Integer;
  node, curr: PDlxNode;
begin
  for r := 0 to 8 do
  begin
    for c := 0 to 8 do
    begin
      if puzzle[r, c] <> 0 then
      begin
        n := puzzle[r, c];

        { Find the row for this clue }
        for row_id := 0 to 728 do
        begin
          if (row_starts[row_id] <> nil) and
             (row_info[row_id].row = r) and
             (row_info[row_id].col = c) and
             (row_info[row_id].num = n) then
          begin
            { Cover all columns in this row }
            node := row_starts[row_id];
            curr := node;
            repeat
              DlxCoverColumn(curr^.column);
              curr := curr^.right;
            until curr = node;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

{ Extract solution from DLX and populate solution_grid }
procedure ExtractSolution(var solution: array of Integer; solution_len: Integer);
var
  i, row_id: Integer;
begin
  { Initialize solution grid - start with the original puzzle }
  solution_grid := puzzle;

  { Each solution entry is a row_id }
  for i := 0 to solution_len - 1 do
  begin
    row_id := solution[i];
    if (row_id >= 0) and (row_id < 729) then
    begin
      solution_grid[row_info[row_id].row, row_info[row_id].col] := row_info[row_id].num;
    end;
  end;
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

procedure PrintSolution;
var
  r, c: Integer;
begin
  WriteLn;
  WriteLn('Puzzle:');
  for r := 0 to 8 do
  begin
    for c := 0 to 8 do
      Write(solution_grid[r, c], ' ');
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

{ Free DLX matrix memory }
procedure FreeDlxMatrix;
var
  i: Integer;
begin
  if root <> nil then
    Dispose(root);
  for i := 0 to 323 do
    if columns[i] <> nil then
      Dispose(columns[i]);
  SetLength(nodes, 0);
end;

{ Main program }
var
  i: Integer;
  filename: string;
  solution: array[0..80] of Integer;
  result: Boolean;
  start_time, end_time: TDateTime;
begin
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'Usage: dlx <matrix_file>');
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

        { Initialize DLX matrix }
        InitDlxMatrix;

        { Build matrix from puzzle }
        BuildDlxMatrixFromPuzzle;

        { Cover pre-filled clues }
        CoverClues;

        { Solve using DLX }
        dlx_iterations := 0;
        FillChar(solution, SizeOf(solution), 0);
        result := DlxSearch(root, 0, solution);

        if result then
        begin
          ExtractSolution(solution, 81);
          PrintSolution;
          WriteLn;
          WriteLn('Solved in Iterations=', dlx_iterations);
          WriteLn;
        end
        else
          WriteLn('No solution found');

        { Cleanup }
        FreeDlxMatrix;
      end;
    end;
  end;

  end_time := Now;
  WriteLn(Format('Seconds to process %.3f', [(end_time - start_time) * 86400]));
end.
