import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.NumberFormat;
import java.util.Locale;

public class SudokuSolver {
    private static final int SIZE = 9;
    private static int iterations = 0;

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Please provide the file path as a command line argument.");
            return;
        }

        try {
            int[] board = readBoardFromFile(args[0]);
            printBoard(board);
            System.out.println("Complexity: " + calculateComplexity(board));

            if (solveBoard(board)) {
                System.out.println("Solved Sudoku board:");
                printBoard(board);
                System.out.println("Iterations: " + NumberFormat.getNumberInstance(Locale.US).format(iterations));
            } else {
                System.out.println("No solution exists.");
            }
        } catch (IOException e) {
            System.out.println("Error reading the file: " + e.getMessage());
        }
    }

    private static int[] readBoardFromFile(String filePath) throws IOException {
        String content = new String(Files.readAllBytes(Paths.get(filePath)));
        int[] board = new int[SIZE * SIZE];
        int index = 0;

        for (String line : content.split("\n")) {
            if (!line.startsWith("#")) {
                for (char c : line.toCharArray()) {
                    if (Character.isDigit(c)) {
                        board[index++] = c - '0';
                    }
                }
            }
        }

        return board;
    }

    private static int calculateComplexity(int[] board) {
        int emptySpaces = 0;

        for (int cell : board) {
            if (cell == 0) {
                emptySpaces++;
            }
        }

        return emptySpaces;
    }

    private static boolean solveBoard(int[] board) {
        for (int i = 0; i < board.length; i++) {
            if (board[i] == 0) {
                for (int num = 1; num <= SIZE; num++) {
                    if (isSafe(board, i, num)) {
                        board[i] = num;
                        iterations++;

                        if (solveBoard(board)) {
                            return true;
                        } else {
                            board[i] = 0;
                        }
                    }
                }

                return false;
            }
        }

        return true;
    }

    private static boolean isSafe(int[] board, int index, int num) {
        int row = index / SIZE;
        int col = index % SIZE;

        for (int i = 0; i < SIZE; i++) {
            if (board[row * SIZE + i] == num || board[i * SIZE + col] == num) {
                return false;
            }
        }

        int boxRowStart = row - row % 3;
        int boxColStart = col - col % 3;

        for (int r = boxRowStart; r < boxRowStart + 3; r++) {
            for (int c = boxColStart; c < boxColStart + 3; c++) {
                if (board[r * SIZE + c] == num) {
                    return false;
                }
            }
        }

        return true;
    }

    private static void printBoard(int[] board) {
        for (int i = 0; i < board.length; i++) {
            System.out.print(board[i] + " ");
            if ((i + 1) % SIZE == 0) {
                System.out.println();
            }
        }
    }
}
