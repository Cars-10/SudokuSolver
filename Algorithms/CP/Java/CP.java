/**
 * Constraint Propagation (CP) Sudoku Solver - Java Implementation
 * Port of CP algorithm from C reference
 *
 * Algorithm: Backtracking with constraint propagation
 * - Uses bitsets to track candidate values per cell
 * - MRV (Minimum Remaining Values) heuristic for cell selection
 * - Propagates constraints after each assignment
 * - Singleton elimination and hidden singles
 * Expected iterations for Matrix 1: 67
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CP {
    // ========================================================================
    // DATA STRUCTURES
    // ========================================================================

    /**
     * CPGrid - represents Sudoku grid with values and candidates
     * values: 0 = empty, 1-9 = assigned
     * candidates: bitset where bit N represents whether digit N is possible
     */
    static class CPGrid {
        int[][] values = new int[9][9];           // Assigned values
        short[][] candidates = new short[9][9];   // Candidate bitsets (16-bit)

        CPGrid() {}

        // Deep copy constructor for backtracking
        CPGrid(CPGrid other) {
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    this.values[r][c] = other.values[r][c];
                    this.candidates[r][c] = other.candidates[r][c];
                }
            }
        }
    }

    // ========================================================================
    // BITSET OPERATIONS
    // ========================================================================

    private static boolean hasCandidate(short set, int digit) {
        return (set & (1 << digit)) != 0;
    }

    private static short removeCandidate(short set, int digit) {
        return (short) (set & ~(1 << digit));
    }

    private static int countCandidates(short set) {
        return Integer.bitCount(set & 0xFFFF);
    }

    private static int getFirstCandidate(short set) {
        for (int digit = 1; digit <= 9; digit++) {
            if (hasCandidate(set, digit)) {
                return digit;
            }
        }
        return 0;
    }

    // ========================================================================
    // GLOBAL STATE
    // ========================================================================

    private static int[][] puzzle = new int[9][9];
    private static long cpIterations = 0;

    // ========================================================================
    // INITIALIZATION
    // ========================================================================

    /**
     * Initialize grid from puzzle
     * Empty cells get all candidates (bits 1-9 set = 0x3FE)
     * Clue cells get single candidate bit
     */
    private static void initGrid(CPGrid grid, int[][] puzzle) {
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                if (puzzle[row][col] == 0) {
                    // Empty cell: all candidates 1-9
                    grid.values[row][col] = 0;
                    grid.candidates[row][col] = (short) 0x3FE;  // Binary: 0011 1111 1110
                } else {
                    // Given clue: single value
                    int digit = puzzle[row][col];
                    grid.values[row][col] = digit;
                    grid.candidates[row][col] = (short) (1 << digit);
                }
            }
        }
    }

    // ========================================================================
    // PEER MANAGEMENT
    // ========================================================================

    /**
     * Get all 20 peers for a cell (8 in row + 8 in col + 4 in box)
     */
    private static int[][] getPeers(int row, int col) {
        int[][] peers = new int[20][2];
        int idx = 0;

        // Same row (8 cells, excluding self)
        for (int c = 0; c < 9; c++) {
            if (c != col) {
                peers[idx][0] = row;
                peers[idx][1] = c;
                idx++;
            }
        }

        // Same column (8 cells, excluding self)
        for (int r = 0; r < 9; r++) {
            if (r != row) {
                peers[idx][0] = r;
                peers[idx][1] = col;
                idx++;
            }
        }

        // Same 3x3 box (4 cells, excluding self and already counted)
        int boxRow = (row / 3) * 3;
        int boxCol = (col / 3) * 3;
        for (int r = boxRow; r < boxRow + 3; r++) {
            for (int c = boxCol; c < boxCol + 3; c++) {
                if (r != row && c != col) {
                    peers[idx][0] = r;
                    peers[idx][1] = c;
                    idx++;
                }
            }
        }

        return peers;
    }

    // ========================================================================
    // CONSTRAINT PROPAGATION
    // ========================================================================

    /**
     * Eliminate a digit from a cell's candidates
     * If only one candidate remains, assign it (singleton elimination)
     * Returns false if contradiction detected
     */
    private static boolean eliminate(CPGrid grid, int row, int col, int digit) {
        // Check if digit is already eliminated
        if (!hasCandidate(grid.candidates[row][col], digit)) {
            return true;  // Already eliminated, no change
        }

        // Remove digit from candidates
        grid.candidates[row][col] = removeCandidate(grid.candidates[row][col], digit);

        // Check for contradiction (no candidates left)
        int remaining = countCandidates(grid.candidates[row][col]);
        if (remaining == 0) {
            return false;  // Contradiction
        }

        // If only one candidate left, assign it (singleton elimination)
        if (remaining == 1 && grid.values[row][col] == 0) {
            int lastDigit = getFirstCandidate(grid.candidates[row][col]);
            if (!assign(grid, row, col, lastDigit)) {
                return false;  // Assignment caused contradiction
            }
        }

        return true;
    }

    /**
     * Assign a digit to a cell
     * Eliminates digit from all peers
     * Increments iteration counter (benchmark metric)
     * Returns false if contradiction detected
     */
    private static boolean assign(CPGrid grid, int row, int col, int digit) {
        // Increment iteration counter (this is our benchmark metric)
        cpIterations++;

        // Set value
        grid.values[row][col] = digit;
        grid.candidates[row][col] = (short) (1 << digit);

        // Eliminate digit from all peers
        int[][] peers = getPeers(row, col);
        for (int i = 0; i < 20; i++) {
            int peerRow = peers[i][0];
            int peerCol = peers[i][1];

            if (!eliminate(grid, peerRow, peerCol, digit)) {
                return false;  // Contradiction in peer elimination
            }
        }

        return true;
    }

    /**
     * Propagate constraints until fixpoint
     * Applies singleton elimination and hidden singles
     * Returns false if contradiction detected
     */
    private static boolean propagate(CPGrid grid) {
        boolean changed = true;

        while (changed) {
            changed = false;

            // Strategy 1: Singleton elimination
            // If a cell has only one candidate, assign it
            for (int row = 0; row < 9; row++) {
                for (int col = 0; col < 9; col++) {
                    if (grid.values[row][col] == 0) {
                        int numCandidates = countCandidates(grid.candidates[row][col]);
                        if (numCandidates == 0) {
                            return false;  // Contradiction
                        }
                        if (numCandidates == 1) {
                            int digit = getFirstCandidate(grid.candidates[row][col]);
                            if (!assign(grid, row, col, digit)) {
                                return false;  // Assignment caused contradiction
                            }
                            changed = true;
                        }
                    }
                }
            }

            // Strategy 2: Hidden singles in rows
            for (int row = 0; row < 9; row++) {
                for (int digit = 1; digit <= 9; digit++) {
                    int count = 0;
                    int lastCol = -1;
                    boolean alreadyAssigned = false;

                    for (int col = 0; col < 9; col++) {
                        if (grid.values[row][col] == digit) {
                            alreadyAssigned = true;
                            break;
                        }
                        if (hasCandidate(grid.candidates[row][col], digit)) {
                            count++;
                            lastCol = col;
                        }
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!assign(grid, row, lastCol, digit)) {
                                return false;
                            }
                            changed = true;
                        } else if (count == 0) {
                            return false;  // Digit cannot be placed anywhere in row
                        }
                    }
                }
            }

            // Strategy 3: Hidden singles in columns
            for (int col = 0; col < 9; col++) {
                for (int digit = 1; digit <= 9; digit++) {
                    int count = 0;
                    int lastRow = -1;
                    boolean alreadyAssigned = false;

                    for (int row = 0; row < 9; row++) {
                        if (grid.values[row][col] == digit) {
                            alreadyAssigned = true;
                            break;
                        }
                        if (hasCandidate(grid.candidates[row][col], digit)) {
                            count++;
                            lastRow = row;
                        }
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!assign(grid, lastRow, col, digit)) {
                                return false;
                            }
                            changed = true;
                        } else if (count == 0) {
                            return false;  // Digit cannot be placed anywhere in column
                        }
                    }
                }
            }

            // Strategy 4: Hidden singles in boxes
            for (int box = 0; box < 9; box++) {
                int boxRow = (box / 3) * 3;
                int boxCol = (box % 3) * 3;

                for (int digit = 1; digit <= 9; digit++) {
                    int count = 0;
                    int lastR = -1, lastC = -1;
                    boolean alreadyAssigned = false;

                    outerBox:
                    for (int r = boxRow; r < boxRow + 3; r++) {
                        for (int c = boxCol; c < boxCol + 3; c++) {
                            if (grid.values[r][c] == digit) {
                                alreadyAssigned = true;
                                break outerBox;
                            }
                            if (hasCandidate(grid.candidates[r][c], digit)) {
                                count++;
                                lastR = r;
                                lastC = c;
                            }
                        }
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!assign(grid, lastR, lastC, digit)) {
                                return false;
                            }
                            changed = true;
                        } else if (count == 0) {
                            return false;  // Digit cannot be placed anywhere in box
                        }
                    }
                }
            }
        }

        return true;  // Success - reached fixpoint
    }

    // ========================================================================
    // SEARCH
    // ========================================================================

    /**
     * Find cell with Minimum Remaining Values (MRV heuristic)
     * Returns null if no empty cells (grid complete)
     */
    private static int[] findMrvCell(CPGrid grid) {
        int minCandidates = 10;  // More than 9
        int[] cell = null;

        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (grid.values[r][c] == 0) {
                    int numCandidates = countCandidates(grid.candidates[r][c]);
                    if (numCandidates < minCandidates) {
                        minCandidates = numCandidates;
                        cell = new int[]{r, c};
                    }
                }
            }
        }

        return cell;  // null if no empty cells
    }

    /**
     * CP Search - backtracking with constraint propagation
     * Returns solution array if found, null otherwise
     */
    private static int[] cpSearch(CPGrid grid) {
        // Base case: check if grid is complete
        int[] mrvCell = findMrvCell(grid);
        if (mrvCell == null) {
            // No empty cells - grid is complete, extract solution
            int[] solution = new int[81];
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    solution[r * 9 + c] = grid.values[r][c];
                }
            }
            return solution;
        }

        // Recursive case: try each candidate for the MRV cell
        int mrvRow = mrvCell[0];
        int mrvCol = mrvCell[1];
        short candidates = grid.candidates[mrvRow][mrvCol];

        for (int digit = 1; digit <= 9; digit++) {
            if (hasCandidate(candidates, digit)) {
                // Save grid state for backtracking
                CPGrid gridCopy = new CPGrid(grid);

                // Try assigning this digit
                if (assign(grid, mrvRow, mrvCol, digit)) {
                    // Assignment succeeded, propagate constraints
                    if (propagate(grid)) {
                        // Propagation succeeded, recurse
                        int[] result = cpSearch(grid);
                        if (result != null) {
                            return result;  // Found solution
                        }
                    }
                }

                // Failed - restore grid state and try next candidate
                grid.values = gridCopy.values;
                grid.candidates = gridCopy.candidates;
            }
        }

        // All candidates exhausted - dead end
        return null;
    }

    // ========================================================================
    // PUZZLE I/O
    // ========================================================================

    private static void printPuzzle(int[][] grid) {
        System.out.println("\nPuzzle:");
        for (int r = 0; r < 9; r++) {
            StringBuilder sb = new StringBuilder();
            for (int c = 0; c < 9; c++) {
                sb.append(grid[r][c]).append(" ");
            }
            System.out.println(sb.toString());
        }
    }

    private static void readMatrixFile(String filename) {
        // Normalize path for output (match C format)
        String displayPath = filename;
        if (filename.startsWith("/app/Matrices/")) {
            displayPath = "../" + filename.substring(5);
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

    // ========================================================================
    // MAIN ENTRYPOINT
    // ========================================================================

    public static void main(String[] args) {
        long startTime = System.nanoTime();

        if (args.length == 0) {
            System.err.println("Usage: java CP <matrix_file>");
            System.exit(1);
        }

        // Process matrix file
        String filename = args[0];
        if (!filename.endsWith(".matrix")) {
            System.err.println("Error: File must have .matrix extension");
            System.exit(1);
        }

        // Read puzzle from file
        readMatrixFile(filename);

        // Print initial puzzle
        printPuzzle(puzzle);

        // Initialize CP grid
        CPGrid grid = new CPGrid();
        initGrid(grid, puzzle);

        // Apply initial propagation
        if (!propagate(grid)) {
            System.out.println("\nNo solution found (contradiction during initial propagation)\n");
            double elapsed = (System.nanoTime() - startTime) / 1_000_000_000.0;
            System.out.printf("Seconds to process %.3f%n", elapsed);
            return;
        }

        // Run search
        cpIterations = 0;
        int[] solution = cpSearch(grid);

        if (solution != null) {
            // Convert solution array back to 2D for printing
            int[][] solutionGrid = new int[9][9];
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    solutionGrid[r][c] = solution[r * 9 + c];
                }
            }

            printPuzzle(solutionGrid);
            System.out.println("\nSolved in Iterations=" + cpIterations + "\n");
        } else {
            System.out.println("\nNo solution found\n");
        }

        double elapsed = (System.nanoTime() - startTime) / 1_000_000_000.0;
        System.out.printf("Seconds to process %.3f%n", elapsed);
    }
}
