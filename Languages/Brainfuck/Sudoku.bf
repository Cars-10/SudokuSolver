[
  Brainfuck Sudoku Solver
  =======================

  THE ENTITY ON A TAPE PHILOSOPHY
  ===============================

  Imagine you are a small creature living on an infinite tape of cells
  Each cell holds a single number from 0 to 255
  You can only see the cell you are standing on
  You can only move left or right one step at a time
  You can only increment or decrement the current cell
  You have no memory except what you write on the tape itself

  This is your world The tape IS your memory your workspace your everything

  To solve a Sudoku you must
  1 Walk the tape to find empty cells cells with value 0
  2 Try values 1 through 9 in each empty cell
  3 Walk to check rows columns and boxes for conflicts
  4 Backtrack when you hit a dead end
  5 Celebrate when no empty cells remain

  TAPE MEMORY LAYOUT
  ==================

  Positions 0 to 161 The Sudoku Grid
    Each of the 81 cells uses 2 tape positions
    Even positions 0 2 4 etc cell value 0 to 9
    Odd positions 1 3 5 etc fixed flag 0=editable 1=given

    Cell N maps to tape position N times 2
    Cell 0 row 0 col 0 is at tape 0 and 1
    Cell 40 row 4 col 4 center is at tape 80 and 81
    Cell 80 row 8 col 8 is at tape 160 and 161

  Positions 162 onwards Working Memory
    This is where we store temporary values during computation
    Current position being examined
    Trial value being tested
    Scratch space for arithmetic
    Navigation markers
    Backtrack stack

  THE JOURNEY BEGINS
  ==================

  First we must initialize the tape with Matrix 1
  Then we walk forward seeking empty clay to mold
  When we find emptiness we try to fill it
  If our attempts fail we step back and try again
  Eventually either we fill all cells or we are done

  This file will grow as each story adds more functionality
  For now it stands as a monument to what will be built
]
