with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Real_Time; use Ada.Real_Time;

procedure Sudoku is
   type Board_Type is array (0 .. 8, 0 .. 8) of Integer;
   Puzzle : Board_Type := (others => (others => 0));
   Count  : Integer := 0;
   
   procedure Print_Puzzle is
   begin
      Put_Line("");
      Put_Line("Puzzle:");
      for J in 0 .. 8 loop
         for I in 0 .. 8 loop
            Put(Item => Puzzle(J, I), Width => 1);
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Print_Puzzle;

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
   end Read_Matrix_File;

   function Is_Possible(Y, X, Val : Integer) return Boolean is
      X0 : Integer := (X / 3) * 3;
      Y0 : Integer := (Y / 3) * 3;
   begin
      for I in 0 .. 8 loop
         if Puzzle(I, X) = Val then return False; end if;
         if Puzzle(Y, I) = Val then return False; end if;
      end loop;

      for I in 0 .. 2 loop
         for J in 0 .. 2 loop
            if Puzzle(Y0 + I, X0 + J) = Val then return False; end if;
         end loop;
      end loop;
      return True;
   end Is_Possible;

   function Solve return Boolean is
   begin
      for J in 0 .. 8 loop
         for I in 0 .. 8 loop
            if Puzzle(J, I) = 0 then
               for Val in 1 .. 9 loop
                  Count := Count + 1;
                  if Is_Possible(J, I, Val) then
                     Puzzle(J, I) := Val;
                     if Solve then return True; end if;
                     Puzzle(J, I) := 0;
                  end if;
               end loop;
               return False;
            end if;
         end loop;
      end loop;
      Print_Puzzle;
      Put_Line("");
      Put("Solved in Iterations=");
      Put(Item => Count, Width => 1);
      New_Line;
      New_Line;
      return True;
   end Solve;

   Start_Time, End_Time : Time;
   Elapsed : Time_Span;
   Seconds : Float;
begin
   Start_Time := Clock;
   for I in 1 .. Argument_Count loop
      if Index(Argument(I), ".matrix") > 0 then
         Read_Matrix_File(Argument(I));
         Print_Puzzle;
         Count := 0;
         if Solve then null; end if;
      end if;
   end loop;
   End_Time := Clock;
   Elapsed := End_Time - Start_Time;
   Seconds := Float(To_Duration(Elapsed));
   Put("Seconds to process ");
   Ada.Float_Text_IO.Put(Item => Seconds, Fore => 1, Aft => 3, Exp => 0);
   New_Line;
end Sudoku;
