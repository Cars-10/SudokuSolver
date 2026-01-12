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

  MATRIX 1 PUZZLE
  ===============

  The grid we are solving
  Row 0  9 2 0 0 0 0 5 8 4
  Row 1  0 0 0 5 0 0 0 0 3
  Row 2  0 8 3 0 9 2 0 0 0
  Row 3  2 6 0 8 5 4 0 0 1
  Row 4  0 0 5 3 6 1 0 9 0
  Row 5  1 0 0 0 0 9 0 0 0
  Row 6  8 5 0 2 0 3 0 1 0
  Row 7  4 1 2 9 8 0 0 3 0
  Row 8  3 9 0 0 0 6 8 0 0

  We start at tape position 0
  For each cell we set the value then move to fixed flag then move to next value
  Given cells fixed flag equals 1 empty cells fixed flag equals 0
]

TAPE INITIALIZATION
===================
We begin our journey at position 0 on a blank tape
Each cell of the Sudoku grid will be laid out as value then fixed flag
We walk forward setting each value and marking given cells as fixed

Row 0 of puzzle  9 2 0 0 0 0 5 8 4
Cell 0 value 9 fixed 1
+++++++++>+>
Cell 1 value 2 fixed 1
++>+>
Cell 2 value 0 fixed 0
>>
Cell 3 value 0 fixed 0
>>
Cell 4 value 0 fixed 0
>>
Cell 5 value 0 fixed 0
>>
Cell 6 value 5 fixed 1
+++++>+>
Cell 7 value 8 fixed 1
++++++++>+>
Cell 8 value 4 fixed 1
++++>+>

Row 1 of puzzle  0 0 0 5 0 0 0 0 3
Cell 9 value 0 fixed 0
>>
Cell 10 value 0 fixed 0
>>
Cell 11 value 0 fixed 0
>>
Cell 12 value 5 fixed 1
+++++>+>
Cell 13 value 0 fixed 0
>>
Cell 14 value 0 fixed 0
>>
Cell 15 value 0 fixed 0
>>
Cell 16 value 0 fixed 0
>>
Cell 17 value 3 fixed 1
+++>+>

Row 2 of puzzle  0 8 3 0 9 2 0 0 0
Cell 18 value 0 fixed 0
>>
Cell 19 value 8 fixed 1
++++++++>+>
Cell 20 value 3 fixed 1
+++>+>
Cell 21 value 0 fixed 0
>>
Cell 22 value 9 fixed 1
+++++++++>+>
Cell 23 value 2 fixed 1
++>+>
Cell 24 value 0 fixed 0
>>
Cell 25 value 0 fixed 0
>>
Cell 26 value 0 fixed 0
>>

Row 3 of puzzle  2 6 0 8 5 4 0 0 1
Cell 27 value 2 fixed 1
++>+>
Cell 28 value 6 fixed 1
++++++>+>
Cell 29 value 0 fixed 0
>>
Cell 30 value 8 fixed 1
++++++++>+>
Cell 31 value 5 fixed 1
+++++>+>
Cell 32 value 4 fixed 1
++++>+>
Cell 33 value 0 fixed 0
>>
Cell 34 value 0 fixed 0
>>
Cell 35 value 1 fixed 1
+>+>

Row 4 of puzzle  0 0 5 3 6 1 0 9 0
Cell 36 value 0 fixed 0
>>
Cell 37 value 0 fixed 0
>>
Cell 38 value 5 fixed 1
+++++>+>
Cell 39 value 3 fixed 1
+++>+>
Cell 40 value 6 fixed 1
++++++>+>
Cell 41 value 1 fixed 1
+>+>
Cell 42 value 0 fixed 0
>>
Cell 43 value 9 fixed 1
+++++++++>+>
Cell 44 value 0 fixed 0
>>

Row 5 of puzzle  1 0 0 0 0 9 0 0 0
Cell 45 value 1 fixed 1
+>+>
Cell 46 value 0 fixed 0
>>
Cell 47 value 0 fixed 0
>>
Cell 48 value 0 fixed 0
>>
Cell 49 value 0 fixed 0
>>
Cell 50 value 9 fixed 1
+++++++++>+>
Cell 51 value 0 fixed 0
>>
Cell 52 value 0 fixed 0
>>
Cell 53 value 0 fixed 0
>>

Row 6 of puzzle  8 5 0 2 0 3 0 1 0
Cell 54 value 8 fixed 1
++++++++>+>
Cell 55 value 5 fixed 1
+++++>+>
Cell 56 value 0 fixed 0
>>
Cell 57 value 2 fixed 1
++>+>
Cell 58 value 0 fixed 0
>>
Cell 59 value 3 fixed 1
+++>+>
Cell 60 value 0 fixed 0
>>
Cell 61 value 1 fixed 1
+>+>
Cell 62 value 0 fixed 0
>>

Row 7 of puzzle  4 1 2 9 8 0 0 3 0
Cell 63 value 4 fixed 1
++++>+>
Cell 64 value 1 fixed 1
+>+>
Cell 65 value 2 fixed 1
++>+>
Cell 66 value 9 fixed 1
+++++++++>+>
Cell 67 value 8 fixed 1
++++++++>+>
Cell 68 value 0 fixed 0
>>
Cell 69 value 0 fixed 0
>>
Cell 70 value 3 fixed 1
+++>+>
Cell 71 value 0 fixed 0
>>

Row 8 of puzzle  3 9 0 0 0 6 8 0 0
Cell 72 value 3 fixed 1
+++>+>
Cell 73 value 9 fixed 1
+++++++++>+>
Cell 74 value 0 fixed 0
>>
Cell 75 value 0 fixed 0
>>
Cell 76 value 0 fixed 0
>>
Cell 77 value 6 fixed 1
++++++>+>
Cell 78 value 8 fixed 1
++++++++>+>
Cell 79 value 0 fixed 0
>>
Cell 80 value 0 fixed 0
>>

GRID INITIALIZATION COMPLETE
============================
We have now walked from position 0 to position 162
The tape behind us contains the entire Sudoku grid
81 cells times 2 positions each equals 162 tape cells
Position 162 marks the start of our working memory

We are now at position 162 which is curr pos in working memory
The entity stands at the threshold between grid and workspace
Behind us lies the puzzle we must solve
Ahead lies empty tape waiting to hold our thoughts

END OF INITIALIZATION SECTION
