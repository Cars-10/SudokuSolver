CREATE TABLE input (
    row INT,
    col INT,
    val INT
);

-- We will load the puzzle into this table
-- But since we need to process multiple files, we might need a shell wrapper to create a DB for each or truncate.
-- Actually, for SQLite, we can just run the script on an in-memory DB or a fresh file.

WITH RECURSIVE
  digits(z) AS (
    VALUES('1'),('2'),('3'),('4'),('5'),('6'),('7'),('8'),('9')
  ),
  x(s, ind) AS (
    SELECT s, instr(s, '.') FROM (
      SELECT group_concat(val, '') AS s 
      FROM (
        SELECT val FROM input ORDER BY row, col
      )
    )
    UNION ALL
    SELECT
      substr(s, 1, ind - 1) || z || substr(s, ind + 1),
      instr(substr(s, 1, ind - 1) || z || substr(s, ind + 1), '.')
    FROM x, digits AS z
    WHERE ind > 0
      AND NOT EXISTS (
        SELECT 1
        FROM digits AS lp
        WHERE z.z = lp.z
        AND (
          -- Same Row
          substr(s, ((ind - 1) / 9) * 9 + 1, 9) LIKE '%' || lp.z || '%'
          OR
          -- Same Column
          substr(s, (ind - 1) % 9 + 1, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 9, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 18, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 27, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 36, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 45, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 54, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 63, 1) = lp.z
          OR substr(s, (ind - 1) % 9 + 1 + 72, 1) = lp.z
          -- Same Box (This is hard with string manipulation in SQL, let's try a different approach)
        )
      )
  )
SELECT s FROM x WHERE ind = 0;

-- The above string manipulation approach is very slow and complex in standard SQL.
-- A better approach for SQLite is using a recursive CTE with the board state string, 
-- but checking constraints is tricky.
-- Let's use a simpler solver if possible, or a very optimized CTE.

-- Actually, let's use the standard "Sudoku in SQL" approach found online, adapted for our input format.
-- But we need to read the file first.
-- The RunMe.sh will parse the matrix file and generate INSERT statements.

-- Optimized Recursive CTE
WITH RECURSIVE
  input_str(s) AS (
    SELECT group_concat(val, '') FROM (SELECT val FROM input ORDER BY row, col)
  ),
  solve(s, ind) AS (
    SELECT s, instr(s, '0') FROM input_str
    UNION ALL
    SELECT
      substr(s, 1, ind - 1) || z || substr(s, ind + 1),
      instr(substr(s, 1, ind - 1) || z || substr(s, ind + 1), '0')
    FROM solve, digits
    WHERE ind > 0
    AND NOT EXISTS (
        SELECT 1
        FROM digits AS lp
        WHERE z = lp.z
        AND (
            -- Row
            substr(s, ((ind - 1) / 9) * 9 + 1, 9) LIKE '%' || lp.z || '%'
            OR
            -- Col (check every 9th char)
            substr(s, (ind - 1) % 9 + 1, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 10, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 19, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 28, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 37, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 46, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 55, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 64, 1) = lp.z OR
            substr(s, (ind - 1) % 9 + 73, 1) = lp.z
            OR
            -- Box
            (
                SELECT count(*) > 0 FROM (
                    SELECT substr(s, r*9 + c + 1, 1) as v
                    FROM (SELECT 0 as r UNION SELECT 1 UNION SELECT 2) as rows,
                         (SELECT 0 as c UNION SELECT 1 UNION SELECT 2) as cols
                    WHERE 
                        ((ind-1)/9/3)*3 + rows.r = r
                        AND ((ind-1)%9/3)*3 + cols.c = c
                ) WHERE v = lp.z
            )
        )
    )
  ),
  digits(z) AS (VALUES('1'),('2'),('3'),('4'),('5'),('6'),('7'),('8'),('9'))
SELECT s FROM solve WHERE ind = 0;
