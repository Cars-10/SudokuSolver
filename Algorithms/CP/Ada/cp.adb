with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;

procedure Cp is
   -- Type definitions
   subtype CandidateSet is Interfaces.Unsigned_16;
   type Grid_Values is array (0 .. 8, 0 .. 8) of Integer;
   type Grid_Candidates is array (0 .. 8, 0 .. 8) of CandidateSet;

   type CPGrid is record
      Values : Grid_Values;
      Candidates : Grid_Candidates;
   end record;

   -- Global variables
   Puzzle : Grid_Values := (others => (others => 0));
   Cp_Iterations : Long_Long_Integer := 0;

   -- Helper functions for bitset manipulation
   function Has_Candidate(Set : CandidateSet; Digit : Integer) return Boolean is
   begin
      return (Set and Shift_Left(1, Digit)) /= 0;
   end Has_Candidate;

   procedure Remove_Candidate(Set : in out CandidateSet; Digit : Integer) is
   begin
      Set := Set and not Shift_Left(1, Digit);
   end Remove_Candidate;

   function Count_Candidates(Set : CandidateSet) return Integer is
      Count : Integer := 0;
      Temp : CandidateSet := Set;
   begin
      while Temp /= 0 loop
         if (Temp and 1) /= 0 then
            Count := Count + 1;
         end if;
         Temp := Shift_Right(Temp, 1);
      end loop;
      return Count;
   end Count_Candidates;

   function Get_First_Candidate(Set : CandidateSet) return Integer is
   begin
      for Digit in 1 .. 9 loop
         if Has_Candidate(Set, Digit) then
            return Digit;
         end if;
      end loop;
      return 0;
   end Get_First_Candidate;

   -- Initialize grid
   procedure Init_Grid(Grid : in out CPGrid) is
   begin
      for Row in 0 .. 8 loop
         for Col in 0 .. 8 loop
            if Puzzle(Row, Col) = 0 then
               Grid.Values(Row, Col) := 0;
               Grid.Candidates(Row, Col) := 16#03FE#;  -- Bits 1-9 set
            else
               declare
                  Digit : Integer := Puzzle(Row, Col);
               begin
                  Grid.Values(Row, Col) := Digit;
                  Grid.Candidates(Row, Col) := Shift_Left(1, Digit);
               end;
            end if;
         end loop;
      end loop;
   end Init_Grid;

   -- Get all peers for a cell (20 cells in same row, col, box)
   type Peer_Cell is record
      Row : Integer;
      Col : Integer;
   end record;
   type Peer_Array is array (0 .. 19) of Peer_Cell;

   procedure Get_Peers(Row, Col : Integer; Peers : out Peer_Array) is
      Idx : Integer := 0;
      Box_Row : Integer := (Row / 3) * 3;
      Box_Col : Integer := (Col / 3) * 3;
   begin
      -- Same row
      for C in 0 .. 8 loop
         if C /= Col then
            Peers(Idx).Row := Row;
            Peers(Idx).Col := C;
            Idx := Idx + 1;
         end if;
      end loop;

      -- Same column
      for R in 0 .. 8 loop
         if R /= Row then
            Peers(Idx).Row := R;
            Peers(Idx).Col := Col;
            Idx := Idx + 1;
         end if;
      end loop;

      -- Same box (excluding already counted cells)
      for R in Box_Row .. Box_Row + 2 loop
         for C in Box_Col .. Box_Col + 2 loop
            if R /= Row and C /= Col then
               Peers(Idx).Row := R;
               Peers(Idx).Col := C;
               Idx := Idx + 1;
            end if;
         end loop;
      end loop;
   end Get_Peers;

   -- Forward declarations
   function Assign(Grid : in out CPGrid; Row, Col, Digit : Integer) return Boolean;

   -- Eliminate a digit from a cell's candidates
   function Eliminate(Grid : in out CPGrid; Row, Col, Digit : Integer) return Boolean is
      Remaining : Integer;
      Last_Digit : Integer;
   begin
      -- Already eliminated
      if not Has_Candidate(Grid.Candidates(Row, Col), Digit) then
         return True;
      end if;

      -- Remove digit
      Remove_Candidate(Grid.Candidates(Row, Col), Digit);

      -- Check for contradiction
      Remaining := Count_Candidates(Grid.Candidates(Row, Col));
      if Remaining = 0 then
         return False;
      end if;

      -- Singleton elimination: if only one candidate left, assign it
      if Remaining = 1 and Grid.Values(Row, Col) = 0 then
         Last_Digit := Get_First_Candidate(Grid.Candidates(Row, Col));
         if not Assign(Grid, Row, Col, Last_Digit) then
            return False;
         end if;
      end if;

      return True;
   end Eliminate;

   -- Assign a digit to a cell
   function Assign(Grid : in out CPGrid; Row, Col, Digit : Integer) return Boolean is
      Peers : Peer_Array;
   begin
      -- Increment iteration counter
      Cp_Iterations := Cp_Iterations + 1;

      -- Set value
      Grid.Values(Row, Col) := Digit;
      Grid.Candidates(Row, Col) := Shift_Left(1, Digit);

      -- Eliminate from all peers
      Get_Peers(Row, Col, Peers);
      for I in 0 .. 19 loop
         if not Eliminate(Grid, Peers(I).Row, Peers(I).Col, Digit) then
            return False;
         end if;
      end loop;

      return True;
   end Assign;

   -- Propagate constraints using singleton and hidden singles
   function Propagate(Grid : in out CPGrid) return Boolean is
      Changed : Boolean := True;
      Num_Candidates : Integer;
      Digit : Integer;
      Count : Integer;
      Last_Col, Last_Row : Integer;
      Last_R, Last_C : Integer;
      Box_Row, Box_Col : Integer;
      Found : Boolean;
   begin
      while Changed loop
         Changed := False;

         -- Strategy 1: Singleton elimination
         for Row in 0 .. 8 loop
            for Col in 0 .. 8 loop
               if Grid.Values(Row, Col) = 0 then
                  Num_Candidates := Count_Candidates(Grid.Candidates(Row, Col));
                  if Num_Candidates = 0 then
                     return False;
                  end if;
                  if Num_Candidates = 1 then
                     Digit := Get_First_Candidate(Grid.Candidates(Row, Col));
                     if not Assign(Grid, Row, Col, Digit) then
                        return False;
                     end if;
                     Changed := True;
                  end if;
               end if;
            end loop;
         end loop;

         -- Strategy 2: Hidden singles in rows
         for Row in 0 .. 8 loop
            for Digit in 1 .. 9 loop
               Count := 0;
               Last_Col := -1;
               for Col in 0 .. 8 loop
                  if Grid.Values(Row, Col) = Digit then
                     Count := 0;
                     exit;
                  end if;
                  if Has_Candidate(Grid.Candidates(Row, Col), Digit) then
                     Count := Count + 1;
                     Last_Col := Col;
                  end if;
               end loop;
               if Count = 1 then
                  if not Assign(Grid, Row, Last_Col, Digit) then
                     return False;
                  end if;
                  Changed := True;
               elsif Count = 0 then
                  Found := False;
                  for Col in 0 .. 8 loop
                     if Grid.Values(Row, Col) = Digit then
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  if not Found then
                     return False;
                  end if;
               end if;
            end loop;
         end loop;

         -- Strategy 3: Hidden singles in columns
         for Col in 0 .. 8 loop
            for Digit in 1 .. 9 loop
               Count := 0;
               Last_Row := -1;
               for Row in 0 .. 8 loop
                  if Grid.Values(Row, Col) = Digit then
                     Count := 0;
                     exit;
                  end if;
                  if Has_Candidate(Grid.Candidates(Row, Col), Digit) then
                     Count := Count + 1;
                     Last_Row := Row;
                  end if;
               end loop;
               if Count = 1 then
                  if not Assign(Grid, Last_Row, Col, Digit) then
                     return False;
                  end if;
                  Changed := True;
               elsif Count = 0 then
                  Found := False;
                  for Row in 0 .. 8 loop
                     if Grid.Values(Row, Col) = Digit then
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  if not Found then
                     return False;
                  end if;
               end if;
            end loop;
         end loop;

         -- Strategy 4: Hidden singles in boxes
         for Box in 0 .. 8 loop
            Box_Row := (Box / 3) * 3;
            Box_Col := (Box mod 3) * 3;
            for Digit in 1 .. 9 loop
               Count := 0;
               Last_R := -1;
               Last_C := -1;
               for R in Box_Row .. Box_Row + 2 loop
                  for C in Box_Col .. Box_Col + 2 loop
                     if Grid.Values(R, C) = Digit then
                        Count := 0;
                        goto Next_Box_Digit;
                     end if;
                     if Has_Candidate(Grid.Candidates(R, C), Digit) then
                        Count := Count + 1;
                        Last_R := R;
                        Last_C := C;
                     end if;
                  end loop;
               end loop;
               if Count = 1 then
                  if not Assign(Grid, Last_R, Last_C, Digit) then
                     return False;
                  end if;
                  Changed := True;
               elsif Count = 0 then
                  Found := False;
                  for R in Box_Row .. Box_Row + 2 loop
                     for C in Box_Col .. Box_Col + 2 loop
                        if Grid.Values(R, C) = Digit then
                           Found := True;
                           goto Found_Box_Digit;
                        end if;
                     end loop;
                  end loop;
                  <<Found_Box_Digit>>
                  if not Found then
                     return False;
                  end if;
               end if;
               <<Next_Box_Digit>>
               null;
            end loop;
         end loop;
      end loop;

      return True;
   end Propagate;

   -- Find MRV cell (minimum remaining values heuristic)
   function Find_Mrv_Cell(Grid : CPGrid; Mrv_Row, Mrv_Col : out Integer) return Boolean is
      Min_Candidates : Integer := 10;
      Num_Candidates : Integer;
      Found : Boolean := False;
   begin
      for R in 0 .. 8 loop
         for C in 0 .. 8 loop
            if Grid.Values(R, C) = 0 then
               Num_Candidates := Count_Candidates(Grid.Candidates(R, C));
               if Num_Candidates < Min_Candidates then
                  Min_Candidates := Num_Candidates;
                  Mrv_Row := R;
                  Mrv_Col := C;
                  Found := True;
               end if;
            end if;
         end loop;
      end loop;
      return Found;
   end Find_Mrv_Cell;

   -- CP Search with MRV and constraint propagation
   function Cp_Search(Grid : in out CPGrid; Solution : out Grid_Values) return Boolean is
      Mrv_Row, Mrv_Col : Integer;
      Candidates : CandidateSet;
      Grid_Copy : CPGrid;
   begin
      -- Check if complete
      if not Find_Mrv_Cell(Grid, Mrv_Row, Mrv_Col) then
         Solution := Grid.Values;
         return True;
      end if;

      -- Try each candidate for MRV cell
      Candidates := Grid.Candidates(Mrv_Row, Mrv_Col);
      for Digit in 1 .. 9 loop
         if Has_Candidate(Candidates, Digit) then
            -- Save state
            Grid_Copy := Grid;

            -- Try assigning this digit
            if Assign(Grid, Mrv_Row, Mrv_Col, Digit) then
               if Propagate(Grid) then
                  if Cp_Search(Grid, Solution) then
                     return True;
                  end if;
               end if;
            end if;

            -- Restore state
            Grid := Grid_Copy;
         end if;
      end loop;

      return False;
   end Cp_Search;

   -- Print puzzle
   procedure Print_Puzzle(Grid : Grid_Values) is
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
   Grid : CPGrid;
   Solution : Grid_Values;
   Result : Boolean;
begin
   Start_Time := Clock;

   for I in 1 .. Argument_Count loop
      if Index(Argument(I), ".matrix") > 0 then
         Read_Matrix_File(Argument(I));
         Print_Puzzle(Puzzle);

         Init_Grid(Grid);

         Cp_Iterations := 0;
         Result := Cp_Search(Grid, Solution);

         if Result then
            Print_Puzzle(Solution);
            Put_Line("");
            Put("Solved in Iterations=");
            Put(Long_Long_Integer'Image(Cp_Iterations));
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
end Cp;
