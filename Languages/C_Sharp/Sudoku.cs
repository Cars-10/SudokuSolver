/**
 * Sudoku Solver - C# Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * Algorithm:
 * - Row-major search for empty cells (top-to-bottom, left-to-right)
 * - Try values 1-9 in ascending order
 * - Count EVERY placement attempt (algorithm fingerprint)
 */

using System.Diagnostics;

class Sudoku
{
    static int[,] puzzle = new int[9, 9];
    static long count = 0;

    static void Main(string[] args)
    {
        var stopwatch = Stopwatch.StartNew();

        foreach (var arg in args)
        {
            if (!arg.EndsWith(".matrix")) continue;

            // Reset puzzle
            puzzle = new int[9, 9];

            ReadMatrixFile(arg);
            PrintPuzzle();
            count = 0;
            Solve();
        }

        stopwatch.Stop();
        double elapsed = stopwatch.Elapsed.TotalSeconds;
        Console.WriteLine($"Seconds to process {elapsed:F3}");
    }

    static void ReadMatrixFile(string filename)
    {
        // Normalize path for output (match C format)
        string displayPath = filename;
        if (filename.StartsWith("/app/Matrices/"))
        {
            displayPath = "../" + filename.Substring(5);  // Skip "/app/" to get "Matrices/..."
        }
        Console.WriteLine(displayPath);

        int lineCount = 0;
        foreach (var line in File.ReadLines(filename))
        {
            if (lineCount >= 9) break;

            string trimmed = line.Trim();
            // Skip comments and empty lines
            if (string.IsNullOrEmpty(trimmed) || trimmed.StartsWith("#")) continue;

            // Parse 9 integers from line
            string[] parts = trimmed.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length == 9)
            {
                var sb = new System.Text.StringBuilder();
                for (int j = 0; j < 9; j++)
                {
                    puzzle[lineCount, j] = int.Parse(parts[j]);
                    sb.Append(puzzle[lineCount, j]).Append(" ");
                }
                Console.WriteLine(sb.ToString());
                lineCount++;
            }
        }
    }

    static void PrintPuzzle()
    {
        Console.WriteLine("\nPuzzle:");
        for (int row = 0; row < 9; row++)
        {
            var sb = new System.Text.StringBuilder();
            for (int col = 0; col < 9; col++)
            {
                sb.Append(puzzle[row, col]).Append(" ");
            }
            Console.WriteLine(sb.ToString());
        }
    }

    static bool IsValid(int row, int col, int value)
    {
        // Check row
        for (int i = 0; i < 9; i++)
        {
            if (puzzle[row, i] == value) return false;
        }

        // Check column
        for (int i = 0; i < 9; i++)
        {
            if (puzzle[i, col] == value) return false;
        }

        // Check 3x3 box
        int boxRow = (row / 3) * 3;
        int boxCol = (col / 3) * 3;
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++)
            {
                if (puzzle[boxRow + i, boxCol + j] == value) return false;
            }
        }

        return true;
    }

    static bool Solve()
    {
        // Find first empty cell (row-major order)
        int row = -1, col = -1;
        bool found = false;

        for (int r = 0; r < 9 && !found; r++)
        {
            for (int c = 0; c < 9 && !found; c++)
            {
                if (puzzle[r, c] == 0)
                {
                    row = r;
                    col = c;
                    found = true;
                }
            }
        }

        // If no empty cell found, puzzle is solved
        if (row == -1)
        {
            PrintPuzzle();
            Console.WriteLine($"\nSolved in Iterations={count}\n");
            return true;
        }

        // Try values 1-9 in order
        for (int value = 1; value <= 9; value++)
        {
            count++;  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

            if (IsValid(row, col, value))
            {
                puzzle[row, col] = value;  // Place value

                if (Solve())
                {
                    return true;
                }

                puzzle[row, col] = 0;  // Backtrack
            }
        }

        return false;
    }
}
