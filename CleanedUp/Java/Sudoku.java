import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Sudoku {
    private static int[][] puzzle = new int[9][9];
    private static long iterations = 0;

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: java Sudoku <file1> <file2> ...");
            System.exit(1);
        }

        for (String filename : args) {
            System.out.println("\nProcessing " + filename);
            if (readBoard(filename)) {
                printBoard();
                iterations = 0;
                if (solve(0, 0)) {
                    printBoard();
                    System.out.println("\nSolved in Iterations=" + iterations);
                } else {
                    System.out.println("No solution found");
                }
            }
        }
    }

    private static boolean readBoard(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int row = 0;
            while ((line = br.readLine()) != null && row < 9) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#")) {
                    continue;
                }
                String[] parts = line.split("\\s+");
                int col = 0;
                for (String part : parts) {
                    if (col < 9) {
                        puzzle[row][col] = Integer.parseInt(part);
                        col++;
                    }
                }
                row++;
            }
            return true;
        } catch (IOException | NumberFormatException e) {
            System.out.println("Error reading file " + filename + ": " + e.getMessage());
            return false;
        }
    }

    private static void printBoard() {
        System.out.println("Puzzle:");
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                System.out.print(puzzle[i][j] + " ");
            }
            System.out.println();
        }
    }

    private static boolean isPossible(int row, int col, int num) {
        for (int i = 0; i < 9; i++) {
            if (puzzle[row][i] == num || puzzle[i][col] == num) {
                return false;
            }
        }
        int startRow = (row / 3) * 3;
        int startCol = (col / 3) * 3;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (puzzle[startRow + i][startCol + j] == num) {
                    return false;
                }
            }
        }
        return true;
    }

    private static boolean solve(int row, int col) {
        if (row == 9) {
            return true;
        }
        int nextRow = row;
        int nextCol = col + 1;
        if (nextCol == 9) {
            nextRow = row + 1;
            nextCol = 0;
        }

        if (puzzle[row][col] != 0) {
            return solve(nextRow, nextCol);
        }

        for (int num = 1; num <= 9; num++) {
            iterations++;
            if (isPossible(row, col, num)) {
                puzzle[row][col] = num;
                if (solve(nextRow, nextCol)) {
                    return true;
                }
                puzzle[row][col] = 0;
            }
        }
        return false;
    }
}
