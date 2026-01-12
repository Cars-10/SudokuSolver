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

  TAPE MEMORY LAYOUT QUICK REFERENCE
  ==================================

  GRID REGION positions 0 to 161
  Each Sudoku cell uses 2 tape positions
    Cell N value at position N times 2
    Cell N fixed flag at position N times 2 plus 1

  Layout
    Cell  0 row0 col0 at tape   0   1
    Cell  8 row0 col8 at tape  16  17
    Cell  9 row1 col0 at tape  18  19
    Cell 40 row4 col4 at tape  80  81  center
    Cell 80 row8 col8 at tape 160 161

  WORKING MEMORY positions 162 onwards

  Core State 162 to 165
    162 curr pos   current cell index 0 to 80
    163 trial val  value being tested 1 to 9
    164 result     validity flag 0=invalid 1=valid
    165 solved     completion flag 0=no 1=yes

  Scratch Space 166 to 175
    166 to 175 temp1 through temp10 for intermediate calculations

  Navigation 176 to 180
    176 nav pos    saved position during navigation
    177 nav cnt    step counter for movement
    178 row num    current row 0 to 8
    179 col num    current column 0 to 8
    180 box idx    current box index 0 to 8

  Arithmetic 181 to 185
    181 div q      division quotient
    182 div r      division remainder
    183 mul a      multiplication operand A
    184 mul b      multiplication operand B
    185 mul r      multiplication result

  Backtrack Stack 186 onwards
    186 stack sp   stack pointer
    187 onwards    stack entries storing filled cell positions

  KEY FORMULAS
  ============
    tape pos for cell value = cell index times 2
    tape pos for fixed flag = cell index times 2 plus 1
    row = cell index divided by 9
    col = cell index modulo 9
    row start = row times 9
    box row = row divided by 3 times 3
    box col = col divided by 3 times 3

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
