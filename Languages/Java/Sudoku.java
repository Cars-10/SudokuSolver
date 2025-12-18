/**
 * Sudoku Solver - Java Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * Algorithm:
 * - Row-major search for empty cells (top-to-bottom, left-to-right)
 * - Try values 1-9 in ascending order
 * - Count EVERY placement attempt (algorithm fingerprint)
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Sudoku {
    private static int[][] puzzle = new int[9][9];
    private static long count = 0;

    public static void main(String[] args) {
        long startTime = System.nanoTime();

        for (String arg : args) {
            if (!arg.endsWith(".matrix")) continue;

            // Reset puzzle
            puzzle = new int[9][9];

            readMatrixFile(arg);
            printPuzzle();
            count = 0;
            solve();
        }

        double elapsed = (System.nanoTime() - startTime) / 1_000_000_000.0;
        System.out.printf("Seconds to process %.3f%n", elapsed);
    }

    private static void readMatrixFile(String filename) {
        // Normalize path for output (match C format)
        String displayPath = filename;
        if (filename.startsWith("/app/Matrices/")) {
            displayPath = "../" + filename.substring(5);  // Skip "/app/" to get "Matrices/..."
        }
        System.out.println(displayPath);

        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int lineCount = 0;

            while ((line = br.readLine()) != null && lineCount < 9) {
                line = line.trim();
                // Skip comments and empty lines
                if (line.isEmpty() || line.startsWith("#")) continue;

                // Parse 9 integers from line
                String[] parts = line.split("\\s+");
                if (parts.length == 9) {
                    StringBuilder sb = new StringBuilder();
                    for (int j = 0; j < 9; j++) {
                        puzzle[lineCount][j] = Integer.parseInt(parts[j]);
                        sb.append(puzzle[lineCount][j]).append(" ");
                    }
                    System.out.println(sb.toString());
                    lineCount++;
                }
            }
        } catch (IOException | NumberFormatException e) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
    }

    private static void printPuzzle() {
        System.out.println("\nPuzzle:");
        for (int row = 0; row < 9; row++) {
            StringBuilder sb = new StringBuilder();
            for (int col = 0; col < 9; col++) {
                sb.append(puzzle[row][col]).append(" ");
            }
            System.out.println(sb.toString());
        }
    }

    private static boolean isValid(int row, int col, int val) {
        // Check row
        for (int i = 0; i < 9; i++) {
            if (puzzle[row][i] == val) return false;
        }

        // Check column
        for (int i = 0; i < 9; i++) {
            if (puzzle[i][col] == val) return false;
        }

        // Check 3x3 box
        int boxRow = (row / 3) * 3;
        int boxCol = (col / 3) * 3;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (puzzle[boxRow + i][boxCol + j] == val) return false;
            }
        }

        return true;
    }

    private static boolean solve() {
        // Find first empty cell (row-major order)
        int row = -1, col = -1;
        outer:
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (puzzle[r][c] == 0) {
                    row = r;
                    col = c;
                    break outer;
                }
            }
        }

        // If no empty cell found, puzzle is solved
        if (row == -1) {
            printPuzzle();
            System.out.println("\nSolved in Iterations=" + count + "\n");
            return true;
        }

        // Try values 1-9 in order
        for (int val = 1; val <= 9; val++) {
            count++;  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

            if (isValid(row, col, val)) {
                puzzle[row][col] = val;  // Place value

                if (solve()) {
                    return true;
                }

                puzzle[row][col] = 0;  // Backtrack
            }
        }

        return false;
    }
}
