-- Sudoku Solver in SQLite using recursive CTE
-- Exact match of C brute-force algorithm

-- The query takes the board string as a parameter
-- We use a recursive CTE to explore the search space.
-- To count iterations exactly like the C solver, we increment a counter 
-- every time we attempt to place a digit.

WITH RECURSIVE
  -- Input string from external source
  input(s) AS (SELECT ?),
  
  -- Digits to try
  digits(z) AS (VALUES('1'),('2'),('3'),('4'),('5'),('6'),('7'),('8'),('9')),
  
  -- Recursive solver
  -- s: board string (81 chars)
  -- ind: index of first empty cell (1-based, 0 if solved)
  -- count: iteration counter
  -- solved: flag
  solve(s, ind, count, solved) AS (
    SELECT s, instr(s, '.'), 0, 0 FROM input
    UNION ALL
    (
      WITH current AS (SELECT * FROM solve WHERE ind > 0 AND solved = 0 LIMIT 1)
      SELECT 
        CASE 
          WHEN (
            -- Check Row
            instr(substr(s, ((ind-1)/9)*9+1, 9), z) = 0
            AND
            -- Check Column
            instr(
              substr(s, (ind-1)%9+1, 1) || substr(s, (ind-1)%9+10, 1) || 
              substr(s, (ind-1)%9+19, 1) || substr(s, (ind-1)%9+28, 1) || 
              substr(s, (ind-1)%9+37, 1) || substr(s, (ind-1)%9+46, 1) || 
              substr(s, (ind-1)%9+55, 1) || substr(s, (ind-1)%9+64, 1) || 
              substr(s, (ind-1)%9+73, 1),
              z
            ) = 0
            AND
            -- Check Box
            instr(
              substr(s, (((ind-1)/9)/3*3)*9 + (((ind-1)%9)/3*3) + 1, 3) ||
              substr(s, (((ind-1)/9)/3*3+1)*9 + (((ind-1)%9)/3*3) + 1, 3) ||
              substr(s, (((ind-1)/9)/3*3+2)*9 + (((ind-1)%9)/3*3) + 1, 3),
              z
            ) = 0
          ) THEN substr(s, 1, ind-1) || z || substr(s, ind+1)
          ELSE s -- Backtrack handled by NOT incrementing ind or using filter
        END,
        CASE 
          WHEN (isValidCheckAbove) THEN instr(nextBoard, '.')
          ELSE ind -- Stay at current index if move was invalid? 
          -- Wait, this is not easy in pure SQL.
          -- Let's use a different approach for SQLite.
    )
  )
SELECT s, count FROM solve WHERE ind = 0 LIMIT 1;