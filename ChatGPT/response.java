import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class SudokuSolver {

    private static final int SIZE = 9;

    public static void main(String[] args) throws IOException {
        if (args.length == 0) {
            System.out.println("No file provided");
            return;
        }

        String path = args[0];
        int[] board = readBoardFromFile(path);
        printBoard(board);
        System.out.println("Complexity: " + calculateComplexity(board));

        int iterations = solveBoard(board, 0);

        if (iterations != -1) {
            printBoard(board);
            System.out.printf("Solved in %,d iterations.\n", iterations);
        } else {
            System.out.println("No solution found.");
        }
    }

    private static int[] readBoardFromFile(String path) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get(path));
        int[] board = new int[SIZE * SIZE];
        int idx = 0;

        for (String line : lines) {
            if (line.startsWith("#")) continue; // Ignore comments
            for (char c : line.toCharArray()) {
                if (Character.isDigit(c)) {
                    board[idx++] = c - '0';
                }
            }
        }
        return board;
    }

    private static int calculateComplexity(int[] board) {
        int emptyCells = 0;
        for (int num : board) {
            if (num == 0) emptyCells++;
        }
        return emptyCells;
    }

    private static void printBoard(int[] board) {
        for (int i = 0; i < board.length; i++) {
            System.out.print(board[i] + " ");
            if ((i + 1) % SIZE == 0) System.out.println();
        }
        System.out.println();
    }

    private static int solveBoard(int[] board, int index) {
        if (index == SIZE * SIZE) return 0; // Board solved

        if (board[index] != 0) return solveBoard(board, index + 1); // Skip filled cells

        for (int num = 1; num <= SIZE; num++) {
            if (isSafe(board, index, num)) {
                board[index] = num;
                int iterations = solveBoard(board, index + 1);
                if (iterations != -1) return iterations + 1;
                board[index] = 0; // Backtrack
            }
        }

        return -1; // No solution found
    }

    private static boolean isSafe(int[] board, int index, int num) {
        int row = index / SIZE;
        int col = index % SIZE;

        // Check row and column
        for (int i = 0; i < SIZE; i++) {
            if (board[row * SIZE + i] == num || board[i * SIZE + col] == num) return false;
        }

        // Check 3x3 square
        int startRow = row - row % 3;
        int startCol = col - col % 3;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (board[(startRow + i) * SIZE + (startCol + j)] == num) return false;
            }
        }
        return true;
    }

}