with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Real_Time; use Ada.Real_Time;

procedure Dlx is
   -- DLX Node structure
   type DlxNode;
   type DlxNode_Access is access DlxNode;

   type DlxColumn;
   type DlxColumn_Access is access DlxColumn;

   type DlxNode is record
      Up      : DlxNode_Access;
      Down    : DlxNode_Access;
      Left    : DlxNode_Access;
      Right   : DlxNode_Access;
      Column  : DlxColumn_Access;
      Row_Id  : Integer;
   end record;

   type DlxColumn is record
      Node : DlxNode_Access;
      Size : Integer;
   end record;

   -- Row metadata for mapping back to Sudoku
   type RowInfo is record
      Row : Integer;
      Col : Integer;
      Num : Integer;
   end record;

   -- Constants
   Max_Columns : constant Integer := 324;
   Max_Rows    : constant Integer := 729;
   Max_Nodes   : constant Integer := 729 * 4;

   -- Array type definitions
   type Grid_Type is array (0 .. 8, 0 .. 8) of Integer;
   type Solution_Array is array (0 .. 80) of Integer;

   -- Global data structures
   Puzzle : Grid_Type := (others => (others => 0));
   Solution_Grid : Grid_Type := (others => (others => 0));

   Root : DlxColumn_Access;
   Columns : array (0 .. Max_Columns - 1) of DlxColumn_Access;
   Row_Metadata : array (0 .. Max_Rows - 1) of RowInfo;
   Row_Starts : array (0 .. Max_Rows - 1) of DlxNode_Access;

   Dlx_Iterations : Integer := 0;

   -- Constraint column index calculators
   function Get_Position_Col(R, C : Integer) return Integer is
   begin
      return R * 9 + C;
   end Get_Position_Col;

   function Get_Row_Col(R, N : Integer) return Integer is
   begin
      return 81 + R * 9 + (N - 1);
   end Get_Row_Col;

   function Get_Col_Col(C, N : Integer) return Integer is
   begin
      return 162 + C * 9 + (N - 1);
   end Get_Col_Col;

   function Get_Box_Col(R, C, N : Integer) return Integer is
      Box : Integer := (R / 3) * 3 + (C / 3);
   begin
      return 243 + Box * 9 + (N - 1);
   end Get_Box_Col;

   -- Cover a column
   procedure Cover_Column(Col : DlxColumn_Access) is
      Row_Node : DlxNode_Access;
      Right_Node : DlxNode_Access;
   begin
      -- Remove column from header list
      Col.Node.Right.Left := Col.Node.Left;
      Col.Node.Left.Right := Col.Node.Right;

      -- Remove all rows in this column
      Row_Node := Col.Node.Down;
      while Row_Node /= Col.Node loop
         Right_Node := Row_Node.Right;
         while Right_Node /= Row_Node loop
            Right_Node.Down.Up := Right_Node.Up;
            Right_Node.Up.Down := Right_Node.Down;
            Right_Node.Column.Size := Right_Node.Column.Size - 1;
            Right_Node := Right_Node.Right;
         end loop;
         Row_Node := Row_Node.Down;
      end loop;
   end Cover_Column;

   -- Uncover a column (exact reverse of cover)
   procedure Uncover_Column(Col : DlxColumn_Access) is
      Row_Node : DlxNode_Access;
      Left_Node : DlxNode_Access;
   begin
      -- Restore rows (in reverse order)
      Row_Node := Col.Node.Up;
      while Row_Node /= Col.Node loop
         Left_Node := Row_Node.Left;
         while Left_Node /= Row_Node loop
            Left_Node.Column.Size := Left_Node.Column.Size + 1;
            Left_Node.Down.Up := Left_Node;
            Left_Node.Up.Down := Left_Node;
            Left_Node := Left_Node.Left;
         end loop;
         Row_Node := Row_Node.Up;
      end loop;

      -- Restore column to header list
      Col.Node.Right.Left := Col.Node;
      Col.Node.Left.Right := Col.Node;
   end Uncover_Column;

   -- Choose column with minimum size
   function Choose_Column return DlxColumn_Access is
      Best : DlxColumn_Access := null;
      Min_Size : Integer := Integer'Last;
      Current : DlxNode_Access;
   begin
      Current := Root.Node.Right;
      while Current /= Root.Node loop
         if Current.Column.Size < Min_Size then
            Min_Size := Current.Column.Size;
            Best := Current.Column;
         end if;
         Current := Current.Right;
      end loop;
      return Best;
   end Choose_Column;

   -- DLX Search
   function Search(K : Integer; Solution : in out Solution_Array) return Boolean is
      Col : DlxColumn_Access;
      Row_Node : DlxNode_Access;
      Right_Node : DlxNode_Access;
      Left_Node : DlxNode_Access;
   begin
      Dlx_Iterations := Dlx_Iterations + 1;

      -- If matrix is empty, solution found
      if Root.Node.Right = Root.Node then
         return True;
      end if;

      -- Choose column with minimum size
      Col := Choose_Column;

      if Col = null or else Col.Size = 0 then
         return False;
      end if;

      -- Cover this column
      Cover_Column(Col);

      -- Try each row in this column
      Row_Node := Col.Node.Down;
      while Row_Node /= Col.Node loop
         Solution(K) := Row_Node.Row_Id;

         -- Cover all columns in this row
         Right_Node := Row_Node.Right;
         while Right_Node /= Row_Node loop
            Cover_Column(Right_Node.Column);
            Right_Node := Right_Node.Right;
         end loop;

         -- Recurse
         if Search(K + 1, Solution) then
            return True;
         end if;

         -- Backtrack: uncover columns
         Left_Node := Row_Node.Left;
         while Left_Node /= Row_Node loop
            Uncover_Column(Left_Node.Column);
            Left_Node := Left_Node.Left;
         end loop;

         Row_Node := Row_Node.Down;
      end loop;

      -- Uncover column
      Uncover_Column(Col);

      return False;
   end Search;

   -- Initialize DLX matrix
   procedure Init_Matrix is
      Last_Node : DlxNode_Access;
   begin
      -- Create root
      Root := new DlxColumn;
      Root.Node := new DlxNode;
      Root.Node.Left := Root.Node;
      Root.Node.Right := Root.Node;
      Root.Node.Up := Root.Node;
      Root.Node.Down := Root.Node;
      Root.Node.Column := Root;
      Root.Node.Row_Id := -1;
      Root.Size := 0;

      -- Create column headers
      Last_Node := Root.Node;
      for I in 0 .. Max_Columns - 1 loop
         Columns(I) := new DlxColumn;
         Columns(I).Node := new DlxNode;
         Columns(I).Size := 0;
         Columns(I).Node.Up := Columns(I).Node;
         Columns(I).Node.Down := Columns(I).Node;
         Columns(I).Node.Column := Columns(I);
         Columns(I).Node.Row_Id := -1;

         -- Link into header list
         Columns(I).Node.Left := Last_Node;
         Columns(I).Node.Right := Root.Node;
         Last_Node.Right := Columns(I).Node;
         Root.Node.Left := Columns(I).Node;
         Last_Node := Columns(I).Node;
      end loop;
   end Init_Matrix;

   -- Add a node to a column
   function Add_Node(Col : DlxColumn_Access; Row_Id : Integer) return DlxNode_Access is
      Node : DlxNode_Access;
   begin
      Node := new DlxNode;
      Node.Column := Col;
      Node.Row_Id := Row_Id;

      -- Insert at end of column's circular list
      Node.Down := Col.Node;
      Node.Up := Col.Node.Up;
      Col.Node.Up.Down := Node;
      Col.Node.Up := Node;
      Col.Size := Col.Size + 1;

      return Node;
   end Add_Node;

   -- Build DLX row
   procedure Build_Dlx_Row(R, C, N, Row_Id : Integer) is
      N1, N2, N3, N4 : DlxNode_Access;
   begin
      Row_Metadata(Row_Id).Row := R;
      Row_Metadata(Row_Id).Col := C;
      Row_Metadata(Row_Id).Num := N;

      N1 := Add_Node(Columns(Get_Position_Col(R, C)), Row_Id);
      N2 := Add_Node(Columns(Get_Row_Col(R, N)), Row_Id);
      N3 := Add_Node(Columns(Get_Col_Col(C, N)), Row_Id);
      N4 := Add_Node(Columns(Get_Box_Col(R, C, N)), Row_Id);

      -- Link horizontally
      N1.Right := N2;
      N2.Right := N3;
      N3.Right := N4;
      N4.Right := N1;

      N1.Left := N4;
      N2.Left := N1;
      N3.Left := N2;
      N4.Left := N3;

      Row_Starts(Row_Id) := N1;
   end Build_Dlx_Row;

   -- Build matrix from puzzle
   procedure Build_Matrix is
      Row_Id : Integer := 0;
   begin
      for R in 0 .. 8 loop
         for C in 0 .. 8 loop
            if Puzzle(R, C) /= 0 then
               Build_Dlx_Row(R, C, Puzzle(R, C), Row_Id);
               Row_Id := Row_Id + 1;
            else
               for N in 1 .. 9 loop
                  Build_Dlx_Row(R, C, N, Row_Id);
                  Row_Id := Row_Id + 1;
               end loop;
            end if;
         end loop;
      end loop;
   end Build_Matrix;

   -- Cover clues
   procedure Cover_Clues is
      Node : DlxNode_Access;
      Curr : DlxNode_Access;
   begin
      for R in 0 .. 8 loop
         for C in 0 .. 8 loop
            if Puzzle(R, C) /= 0 then
               declare
                  N : Integer := Puzzle(R, C);
               begin
                  for Row_Id in 0 .. Max_Rows - 1 loop
                     if Row_Starts(Row_Id) /= null and then
                        Row_Metadata(Row_Id).Row = R and then
                        Row_Metadata(Row_Id).Col = C and then
                        Row_Metadata(Row_Id).Num = N then

                        Node := Row_Starts(Row_Id);
                        Curr := Node;
                        loop
                           Cover_Column(Curr.Column);
                           Curr := Curr.Right;
                           exit when Curr = Node;
                        end loop;
                        exit;
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end loop;
   end Cover_Clues;

   -- Extract solution
   procedure Extract_Solution(Solution : Solution_Array) is
   begin
      Solution_Grid := Puzzle;
      for I in 0 .. 80 loop
         declare
            Row_Id : Integer := Solution(I);
         begin
            if Row_Id >= 0 and Row_Id < Max_Rows then
               Solution_Grid(Row_Metadata(Row_Id).Row, Row_Metadata(Row_Id).Col) := Row_Metadata(Row_Id).Num;
            end if;
         end;
      end loop;
   end Extract_Solution;

   -- Print puzzle
   procedure Print_Puzzle(Grid : Grid_Type) is
   begin
      Put_Line("");
      Put_Line("Puzzle:");
      for R in 0 .. 8 loop
         for C in 0 .. 8 loop
            Put(Item => Grid(R, C), Width => 1);
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Print_Puzzle;

   -- Read matrix file
   procedure Read_Matrix_File(Filename : String) is
      File : File_Type;
      Line : String(1 .. 100);
      Last : Natural;
      Row  : Integer := 0;
      Col  : Integer := 0;
      Val  : Integer;
      Idx  : Integer;
   begin
      Put_Line(Filename);
      Open(File, In_File, Filename);
      while not End_Of_File(File) and Row < 9 loop
         Get_Line(File, Line, Last);
         if Last > 0 and then Line(1) /= '#' then
            Col := 0;
            Idx := 1;
            while Col < 9 and Idx <= Last loop
               if Line(Idx) in '0' .. '9' then
                  Val := Character'Pos(Line(Idx)) - Character'Pos('0');
                  Puzzle(Row, Col) := Val;
                  Col := Col + 1;
               end if;
               Idx := Idx + 1;
            end loop;
            if Col = 9 then
               Row := Row + 1;
            end if;
         end if;
      end loop;
      Close(File);

      -- Print initial puzzle
      for R in 0 .. 8 loop
         for C in 0 .. 8 loop
            Put(Item => Puzzle(R, C), Width => 1);
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Read_Matrix_File;

   Start_Time, End_Time : Time;
   Elapsed : Time_Span;
   Seconds : Float;
   Solution : Solution_Array;
   Result : Boolean;
begin
   Start_Time := Clock;

   for I in 1 .. Argument_Count loop
      if Index(Argument(I), ".matrix") > 0 then
         Read_Matrix_File(Argument(I));
         Print_Puzzle(Puzzle);

         Init_Matrix;
         Build_Matrix;
         Cover_Clues;

         Dlx_Iterations := 0;
         Solution := (others => -1);
         Result := Search(0, Solution);

         if Result then
            Extract_Solution(Solution);
            Print_Puzzle(Solution_Grid);
            Put_Line("");
            Put("Solved in Iterations=");
            Put(Item => Dlx_Iterations, Width => 1);
            New_Line;
            New_Line;
         else
            Put_Line("");
            Put_Line("No solution found");
         end if;
      end if;
   end loop;

   End_Time := Clock;
   Elapsed := End_Time - Start_Time;
   Seconds := Float(To_Duration(Elapsed));
   Put("Seconds to process ");
   Ada.Float_Text_IO.Put(Item => Seconds, Fore => 1, Aft => 3, Exp => 0);
   New_Line;
end Dlx;
