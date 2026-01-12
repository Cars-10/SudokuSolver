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

OUTPUT ROUTINE
==============
Now we must output the grid to show our progress
The entity walks back to the beginning of the tape
Then we traverse each cell printing its value as a digit
Between digits in the same row we print a space
After each row we print a newline

First we need to return to position 0 from position 162
We move left 162 times

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

We are now at position 0 the start of the grid
The entity stands before cell 0 row 0 column 0

OUTPUT THE GRID
===============
We will output all 81 cells
Each cell value needs ASCII 48 added to become a printable digit
0 becomes 48 which is ASCII zero
1 becomes 49 which is ASCII one
and so on through 9 becomes 57

For each cell we
1 Add 48 to value and print
2 Subtract 48 to restore the value
3 Move to fixed flag position
4 Clear it then build 32 for space or 10 for newline
5 Print the separator
6 Clear the separator
7 Restore fixed flag to original value
8 Move to next cell value

The key insight is we must clear the fixed flag before building our character
Then restore it after printing

ROW 0 OUTPUT cells 0 to 8
Cell 0 value 9
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 1 value 2
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 2 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 3 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 4 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 5 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 6 value 5
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 7 value 8
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 8 value 4 end of row print newline
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]+>

ROW 1 OUTPUT cells 9 to 17
Cell 9 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 10 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 11 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 12 value 5
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 13 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 14 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 15 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 16 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 17 value 3 end of row
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]+>

ROW 2 OUTPUT cells 18 to 26
Cell 18 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 19 value 8
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 20 value 3
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 21 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 22 value 9
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 23 value 2
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 24 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 25 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 26 value 0 end of row
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]>

ROW 3 OUTPUT cells 27 to 35
Cell 27 value 2
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 28 value 6
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 29 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 30 value 8
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 31 value 5
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 32 value 4
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 33 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 34 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 35 value 1 end of row
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]+>

ROW 4 OUTPUT cells 36 to 44
Cell 36 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 37 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 38 value 5
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 39 value 3
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 40 value 6
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 41 value 1
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 42 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 43 value 9
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 44 value 0 end of row
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]>

ROW 5 OUTPUT cells 45 to 53
Cell 45 value 1
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 46 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 47 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 48 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 49 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 50 value 9
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 51 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 52 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 53 value 0 end of row
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]>

ROW 6 OUTPUT cells 54 to 62
Cell 54 value 8
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 55 value 5
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 56 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 57 value 2
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 58 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 59 value 3
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 60 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 61 value 1
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 62 value 0 end of row
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]>

ROW 7 OUTPUT cells 63 to 71
Cell 63 value 4
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 64 value 1
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 65 value 2
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 66 value 9
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 67 value 8
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 68 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 69 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 70 value 3
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 71 value 0 end of row
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]>

ROW 8 OUTPUT cells 72 to 80
Cell 72 value 3
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 73 value 9
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 74 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 75 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 76 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 77 value 6
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 78 value 8
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]+>
Cell 79 value 0
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++++++++++++++++++++++++.[-]>
Cell 80 value 0 end of grid print final newline
++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------
>[-]++++++++++.[-]>

OUTPUT COMPLETE
===============
We have printed all 81 cells as a 9x9 grid
Each row is space separated with newline at end
The entity now stands at position 162 the start of working memory
Ready for the next phase of our journey

END OF OUTPUT SECTION

CELL NAVIGATION HELPERS
=======================

The entity needs to travel between working memory and grid cells
These routines allow us to walk to any cell and find our way back

Working memory position 162 is curr pos which holds our target cell index
Position 176 is nav pos which saves our current tape position
Position 177 is nav cnt which counts steps during navigation

The fundamental insight is that cell N lives at tape position N times 2
From position 162 we must go left to reach the grid and right to return

NAVIGATION FORMULAS
===================

To reach cell N from position 162
  tape position of cell N = N times 2
  steps left from 162 = 162 minus tape position = 162 minus 2N

To return home from tape position P
  steps right = 162 minus P

Example for cell 40 center of grid
  tape position = 40 times 2 = 80
  steps left from 162 = 162 minus 80 = 82 steps

NAVIGATION TEST CELL 40
=======================

We will demonstrate navigation by traveling to cell 40 and back
Cell 40 is row 4 column 4 the center of the grid
Its value in Matrix 1 is 6

Currently at position 162 after output routine
First set curr pos to 40 as our target
Clear current value then add 40

[-]++++++++++++++++++++++++++++++++++++++++

Position 162 now holds 40 our target cell index

GO TO CELL 40
=============

From position 162 navigate to cell 40 at tape position 80
Steps needed = 162 minus 80 = 82 left moves

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

The entity has arrived at position 80 which is cell 40 value position
Cell 40 is row 4 column 4 the very heart of the puzzle

VERIFY ARRIVAL
==============

Print cell 40 value to confirm we navigated correctly
Add 48 for ASCII digit then print then subtract 48 to restore

++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------

You should see digit 6 which confirms cell 40 value
The entity successfully found its way to the center cell

RETURN HOME
===========

Navigate back to position 162 working memory
From position 80 we need 82 right moves

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

The entity has returned to position 162
Working memory is home base for all operations

Print a newline to separate this test output
Go to temp cell position 166 build 10 print clear return

>>>>[-]++++++++++.[-]<<<<

Back at position 162 after navigation test

NAVIGATION TEST SUMMARY
=======================

Successfully demonstrated
  go to cell navigates from position 162 to any grid cell
  return home navigates back from grid to position 162
  Position tracking via curr pos at 162

For cell N the formulas are
  tape position = N times 2
  steps from 162 = 162 minus tape position

The entity can now explore the grid and return home
This is the foundation for constraint checking

END OF NAVIGATION HELPERS
