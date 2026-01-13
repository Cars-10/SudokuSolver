using System;
using System.IO;

// Constraint Propagation (CP) Sudoku Solver
// Direct port of C implementation to maintain algorithmic compatibility

class CPGrid {
    public int[,] Values;           // Assigned values (0 = empty)
    public ushort[,] Candidates;    // Possible values per cell (bitset)

    public CPGrid() {
        Values = new int[9, 9];
        Candidates = new ushort[9, 9];
    }

    public CPGrid(CPGrid other) {
        Values = (int[,])other.Values.Clone();
        Candidates = (ushort[,])other.Candidates.Clone();
    }
}

class CPSolver {
    // Global iteration counter
    static long cpIterations = 0;

    // Candidate manipulation helpers
    static bool HasCandidate(ushort set, int digit) {
        return (set & (1 << digit)) != 0;
    }

    static void AddCandidate(ref ushort set, int digit) {
        set |= (ushort)(1 << digit);
    }

    static void RemoveCandidate(ref ushort set, int digit) {
        set &= (ushort)~(1 << digit);
    }

    static int CountCandidates(ushort set) {
        int count = 0;
        for (int i = 0; i < 16; i++) {
            if ((set & (1 << i)) != 0) count++;
        }
        return count;
    }

    static int GetFirstCandidate(ushort set) {
        for (int digit = 1; digit <= 9; digit++) {
            if (HasCandidate(set, digit)) {
                return digit;
            }
        }
        return 0;
    }

    // Get all 20 peers for a cell (row, col, box)
    static void GetPeers(int row, int col, int[,] peers) {
        int idx = 0;

        // Same row (9 cells minus self = 8)
        for (int c = 0; c < 9; c++) {
            if (c != col) {
                peers[idx, 0] = row;
                peers[idx, 1] = c;
                idx++;
            }
        }

        // Same column (9 cells minus self = 8)
        for (int r = 0; r < 9; r++) {
            if (r != row) {
                peers[idx, 0] = r;
                peers[idx, 1] = col;
                idx++;
            }
        }

        // Same 3x3 box (9 cells minus self minus already counted = 4)
        int boxRow = (row / 3) * 3;
        int boxCol = (col / 3) * 3;
        for (int r = boxRow; r < boxRow + 3; r++) {
            for (int c = boxCol; c < boxCol + 3; c++) {
                if (r != row && c != col) {
                    peers[idx, 0] = r;
                    peers[idx, 1] = c;
                    idx++;
                }
            }
        }
    }

    // Initialize grid with puzzle
    static void InitGrid(CPGrid grid, int[,] puzzle) {
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                if (puzzle[row, col] == 0) {
                    // Empty cell: set all candidates 1-9
                    grid.Values[row, col] = 0;
                    grid.Candidates[row, col] = 0x3FE;  // Binary: 0011 1111 1110 (bits 1-9)
                } else {
                    // Given clue: set single value
                    int digit = puzzle[row, col];
                    grid.Values[row, col] = digit;
                    grid.Candidates[row, col] = (ushort)(1 << digit);
                }
            }
        }
    }

    // Eliminate a digit from a cell
    static bool Eliminate(CPGrid grid, int row, int col, int digit) {
        // Check if digit is already eliminated
        if (!HasCandidate(grid.Candidates[row, col], digit)) {
            return true;  // Already eliminated, no change
        }

        // Remove digit from candidates
        RemoveCandidate(ref grid.Candidates[row, col], digit);

        // Check for contradiction (no candidates left)
        int remaining = CountCandidates(grid.Candidates[row, col]);
        if (remaining == 0) {
            return false;  // Contradiction
        }

        // If only one candidate left, assign it (singleton elimination)
        if (remaining == 1 && grid.Values[row, col] == 0) {
            int lastDigit = GetFirstCandidate(grid.Candidates[row, col]);
            if (!Assign(grid, row, col, lastDigit)) {
                return false;  // Assignment caused contradiction
            }
        }

        return true;
    }

    // Assign a digit to a cell and propagate constraints
    static bool Assign(CPGrid grid, int row, int col, int digit) {
        // Increment iteration counter
        cpIterations++;

        // Set value
        grid.Values[row, col] = digit;
        grid.Candidates[row, col] = (ushort)(1 << digit);

        // Eliminate digit from all peers
        int[,] peers = new int[20, 2];
        GetPeers(row, col, peers);

        for (int i = 0; i < 20; i++) {
            int peerRow = peers[i, 0];
            int peerCol = peers[i, 1];

            if (!Eliminate(grid, peerRow, peerCol, digit)) {
                return false;  // Contradiction in peer elimination
            }
        }

        return true;
    }

    // Propagate constraints until fixpoint
    static bool Propagate(CPGrid grid) {
        bool changed = true;

        while (changed) {
            changed = false;

            // Strategy 1: Singleton elimination
            for (int row = 0; row < 9; row++) {
                for (int col = 0; col < 9; col++) {
                    if (grid.Values[row, col] == 0) {
                        int numCandidates = CountCandidates(grid.Candidates[row, col]);
                        if (numCandidates == 0) {
                            return false;  // Contradiction
                        }
                        if (numCandidates == 1) {
                            int digit = GetFirstCandidate(grid.Candidates[row, col]);
                            if (!Assign(grid, row, col, digit)) {
                                return false;
                            }
                            changed = true;
                        }
                    }
                }
            }

            // Strategy 2: Hidden singles - Check rows
            for (int row = 0; row < 9; row++) {
                for (int digit = 1; digit <= 9; digit++) {
                    int count = 0;
                    int lastCol = -1;
                    bool alreadyAssigned = false;

                    for (int col = 0; col < 9; col++) {
                        if (grid.Values[row, col] == digit) {
                            alreadyAssigned = true;
                            break;
                        }
                        if (HasCandidate(grid.Candidates[row, col], digit)) {
                            count++;
                            lastCol = col;
                        }
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!Assign(grid, row, lastCol, digit)) {
                                return false;
                            }
                            changed = true;
                        } else if (count == 0) {
                            return false;  // Digit cannot be placed
                        }
                    }
                }
            }

            // Check columns
            for (int col = 0; col < 9; col++) {
                for (int digit = 1; digit <= 9; digit++) {
                    int count = 0;
                    int lastRow = -1;
                    bool alreadyAssigned = false;

                    for (int row = 0; row < 9; row++) {
                        if (grid.Values[row, col] == digit) {
                            alreadyAssigned = true;
                            break;
                        }
                        if (HasCandidate(grid.Candidates[row, col], digit)) {
                            count++;
                            lastRow = row;
                        }
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!Assign(grid, lastRow, col, digit)) {
                                return false;
                            }
                            changed = true;
                        } else if (count == 0) {
                            return false;
                        }
                    }
                }
            }

            // Check boxes
            for (int box = 0; box < 9; box++) {
                int boxRow = (box / 3) * 3;
                int boxCol = (box % 3) * 3;

                for (int digit = 1; digit <= 9; digit++) {
                    int count = 0;
                    int lastR = -1, lastC = -1;
                    bool alreadyAssigned = false;

                    for (int r = boxRow; r < boxRow + 3; r++) {
                        for (int c = boxCol; c < boxCol + 3; c++) {
                            if (grid.Values[r, c] == digit) {
                                alreadyAssigned = true;
                                goto nextBoxDigit;
                            }
                            if (HasCandidate(grid.Candidates[r, c], digit)) {
                                count++;
                                lastR = r;
                                lastC = c;
                            }
                        }
                    }

                    nextBoxDigit:
                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!Assign(grid, lastR, lastC, digit)) {
                                return false;
                            }
                            changed = true;
                        } else if (count == 0) {
                            return false;
                        }
                    }
                }
            }
        }

        return true;  // Success - reached fixpoint
    }

    // Find cell with minimum remaining values (MRV heuristic)
    static bool FindMrvCell(CPGrid grid, out int row, out int col) {
        int minCandidates = 10;
        bool found = false;
        row = -1;
        col = -1;

        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (grid.Values[r, c] == 0) {
                    int numCandidates = CountCandidates(grid.Candidates[r, c]);
                    if (numCandidates < minCandidates) {
                        minCandidates = numCandidates;
                        row = r;
                        col = c;
                        found = true;
                    }
                }
            }
        }

        return found;
    }

    // Search with constraint propagation
    static bool CpSearch(CPGrid grid, int[] solution) {
        // Base case: check if grid is complete
        if (!FindMrvCell(grid, out int mrvRow, out int mrvCol)) {
            // No empty cells - grid is complete
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    solution[r * 9 + c] = grid.Values[r, c];
                }
            }
            return true;
        }

        // Recursive case: try each candidate for the MRV cell
        ushort candidates = grid.Candidates[mrvRow, mrvCol];

        for (int digit = 1; digit <= 9; digit++) {
            if (HasCandidate(candidates, digit)) {
                // Save grid state for backtracking
                CPGrid gridCopy = new CPGrid(grid);

                // Try assigning this digit
                if (Assign(grid, mrvRow, mrvCol, digit)) {
                    // Assignment succeeded, propagate constraints
                    if (Propagate(grid)) {
                        // Propagation succeeded, recurse
                        if (CpSearch(grid, solution)) {
                            return true;  // Found solution
                        }
                    }
                }

                // Failed - restore grid state and try next candidate
                grid.Values = (int[,])gridCopy.Values.Clone();
                grid.Candidates = (ushort[,])gridCopy.Candidates.Clone();
            }
        }

        // All candidates exhausted - dead end
        return false;
    }

    // Print puzzle
    static void PrintPuzzle(int[,] grid) {
        Console.WriteLine();
        Console.WriteLine("Puzzle:");
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                Console.Write($"{grid[r, c]} ");
            }
            Console.WriteLine();
        }
    }

    // Read matrix file
    static bool ReadMatrixFile(string filename, int[,] puzzle) {
        if (!File.Exists(filename)) {
            Console.Error.WriteLine($"Error opening file '{filename}'");
            return false;
        }

        // Normalize path for output
        string displayPath = filename;
        if (filename.StartsWith("/app/Matrices/")) {
            displayPath = filename.Substring(5);
            Console.WriteLine($"../{displayPath}");
        } else {
            Console.WriteLine(filename);
        }

        int lineCount = 0;
        foreach (string line in File.ReadLines(filename)) {
            // Skip comments and empty lines
            if (string.IsNullOrWhiteSpace(line) || line.StartsWith("#")) {
                continue;
            }

            string[] parts = line.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length == 9 && lineCount < 9) {
                for (int i = 0; i < 9; i++) {
                    puzzle[lineCount, i] = int.Parse(parts[i]);
                    Console.Write($"{puzzle[lineCount, i]} ");
                }
                Console.WriteLine();
                lineCount++;
            }
        }

        return lineCount == 9;
    }

    static void Main(string[] args) {
        var startTime = DateTime.Now;

        if (args.Length != 1) {
            Console.Error.WriteLine("Usage: CP <matrix_file>");
            return;
        }

        // Read puzzle from file
        int[,] puzzle = new int[9, 9];
        if (!ReadMatrixFile(args[0], puzzle)) {
            Console.Error.WriteLine("Failed to read matrix file");
            return;
        }

        // Print initial puzzle
        PrintPuzzle(puzzle);

        // Initialize CP grid
        CPGrid grid = new CPGrid();
        InitGrid(grid, puzzle);

        // Apply initial propagation
        if (!Propagate(grid)) {
            Console.WriteLine("\nNo solution found (contradiction during initial propagation)");
            var elapsed = DateTime.Now - startTime;
            Console.WriteLine($"Seconds to process {elapsed.TotalSeconds:F3}");
            return;
        }

        // Run search
        int[] solution = new int[81];
        bool solved = CpSearch(grid, solution);

        if (solved) {
            // Convert solution array back to 2D for printing
            int[,] solutionGrid = new int[9, 9];
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    solutionGrid[r, c] = solution[r * 9 + c];
                }
            }

            PrintPuzzle(solutionGrid);
            Console.WriteLine($"\nSolved in Iterations={cpIterations}\n");
        } else {
            Console.WriteLine("\nNo solution found");
        }

        var totalElapsed = DateTime.Now - startTime;
        Console.WriteLine($"Seconds to process {totalElapsed.TotalSeconds:F3}");
    }
}
