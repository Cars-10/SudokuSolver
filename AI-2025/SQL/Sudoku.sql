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
    -- Row Check
    AND instr(substr(s, ((ind - 1) / 9) * 9 + 1, 9), z) = 0
    -- Col Check
    AND instr(
        substr(s, (ind - 1) % 9 + 1, 1) ||
        substr(s, (ind - 1) % 9 + 10, 1) ||
        substr(s, (ind - 1) % 9 + 19, 1) ||
        substr(s, (ind - 1) % 9 + 28, 1) ||
        substr(s, (ind - 1) % 9 + 37, 1) ||
        substr(s, (ind - 1) % 9 + 46, 1) ||
        substr(s, (ind - 1) % 9 + 55, 1) ||
        substr(s, (ind - 1) % 9 + 64, 1) ||
        substr(s, (ind - 1) % 9 + 73, 1),
        z
    ) = 0
    -- Box Check
    AND instr(
        substr(s, (((ind-1)/9/3)*3 + 0)*9 + ((ind-1)%9/3)*3 + 1, 3) ||
        substr(s, (((ind-1)/9/3)*3 + 1)*9 + ((ind-1)%9/3)*3 + 1, 3) ||
        substr(s, (((ind-1)/9/3)*3 + 2)*9 + ((ind-1)%9/3)*3 + 1, 3),
        z
    ) = 0
  ),
  digits(z) AS (VALUES('1'),('2'),('3'),('4'),('5'),('6'),('7'),('8'),('9'))
SELECT s || char(10) || 'Solved in Iterations=' || ((SELECT count(*) FROM solve) * 9) FROM solve WHERE ind = 0;
