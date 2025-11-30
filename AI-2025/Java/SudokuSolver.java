import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class SudokuSolver {
    static long iterations = 0;

    // Step 1: Reads a 9x9 Sudoku matrix from a file and returns it as a 1D array
    public static int[] readBoardFromFile(String filename) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File(filename));
        int[] board = new int[81];
        int index = 0;

        while (scanner.hasNextLine()) {
            String line = scanner.nextLine().trim();
            // Ignore comments and empty lines
            if (!line.startsWith("#") && !line.isEmpty()) {
                String[] numbers = line.split("\\s+");
                for (String number : numbers) {
                    board[index++] = Integer.parseInt(number);
                }
            }
        }

        scanner.close();
        return board;
    }

    // Step 2: Calculates the complexity of a 9x9 Sudoku matrix
    public static int calculateComplexity(int[] board) {
        int complexity = 0;
        for (int cell : board) {
            if (cell == 0) {
                complexity++;
            }
        }
        return complexity;
    }

    // Step 3: Print the board in a 9x9 grid and complexity for the unsolved board
    public static void printBoard(int[] board, boolean calculateComplexity) {
        System.out.println("\nPuzzle:");
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                System.out.print(board[i * 9 + j] + " ");
            }
            System.out.println();
        }
    }

    // Step 4: Solves the Sudoku board using a backtracking algorithm
    public static boolean solveBoard(int[] board, int index) {
        if (index == 81) {
            return true; // Puzzle solved
        }

        if (board[index] != 0) { // Skip filled cells
            return solveBoard(board, index + 1);
        }

        for (int num = 1; num <= 9; num++) {
            iterations++;
            if (isSafe(board, index, num)) {
                board[index] = num;

                if (solveBoard(board, index + 1)) {
                    return true;
                }

                board[index] = 0; // Backtrack
            }
        }

        return false;
    }

    // Helper function to check if it's safe to place a number at the given index
    public static boolean isSafe(int[] board, int index, int num) {
        int row = index / 9;
        int col = index % 9;

        // Check row and column
        for (int i = 0; i < 9; i++) {
            if (board[row * 9 + i] == num || board[i * 9 + col] == num) {
                return false;
            }
        }

        // Check 3x3 square
        int startRow = row - row % 3;
        int startCol = col - col % 3;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (board[(startRow + i) * 9 + (startCol + j)] == num) {
                    return false;
                }
            }
        }

        return true;
    }

    // Main method
    public static void main(String[] args) {
        if (args.length > 0) {
            for (String arg : args) {
                System.out.println(arg);
                try {
                    int[] board = readBoardFromFile(arg);
                    long startTime = System.nanoTime();
                    iterations = 0;
                    printBoard(board, true); // Print unsolved board
                    if (solveBoard(board, 0)) {
                        printBoard(board, false); // Print solved board
                        System.out.println("\nSolved in Iterations=" + iterations);
                        System.out.println();
                    } else {
                        System.out.println("No solution exists.");
                    }
                    long endTime = System.nanoTime();
                    System.out.printf("Seconds to process %.3f\n", (endTime - startTime) / 1_000_000_000.0);
                } catch (FileNotFoundException e) {
                    System.out.println("File not found: " + e.getMessage());
                }
            }
        } else {
            System.out.println("Please provide the filename as a command-line argument.");
        }
    }
}
