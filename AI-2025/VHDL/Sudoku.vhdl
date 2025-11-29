-- Sudoku Solver in VHDL
-- Placeholder

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity sudoku is
end sudoku;

architecture behavior of sudoku is
begin
    process
        variable l : line;
    begin
        write(l, string'("VHDL Sudoku Solver"));
        writeline(output, l);
        write(l, string'("Requires GHDL"));
        writeline(output, l);
        wait;
    end process;
end behavior;
