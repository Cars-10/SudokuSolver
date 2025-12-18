import 'dart:io';

// Sudoku puzzle grid [row][col]
List<List<int>> puzzle = List.generate(9, (_) => List.filled(9, 0));
int count = 0; // Iteration counter

void printPuzzle() {
  print('\nPuzzle:');
  for (int j = 0; j < 9; j++) {
    for (int i = 0; i < 9; i++) {
      stdout.write('${puzzle[j][i]} ');
    }
    print('');
  }
}

int readMatrixFile(String filename) {
  File file = File(filename);
  if (!file.existsSync()) {
    stderr.writeln("Error opening file '$filename'");
    return 1;
  }

  // Normalize path for output (convert absolute to relative)
  if (filename.startsWith('/app/Matrices/')) {
    print('../${filename.substring(5)}');
  } else {
    print(filename);
  }

  int lineCount = 0;
  List<String> lines = file.readAsLinesSync();
  for (String line in lines) {
    String lineStr = line.trim();

    // Skip comments and empty lines
    if (lineStr.isEmpty || lineStr.startsWith('#')) {
      continue;
    }

    if (lineCount >= 9) break;

    // Parse 9 integers from line
    List<String> parts = lineStr.split(RegExp(r'\s+'));
    if (parts.length >= 9) {
      for (int i = 0; i < 9; i++) {
        puzzle[lineCount][i] = int.parse(parts[i]);
        stdout.write('${puzzle[lineCount][i]} ');
      }
      print('');
      lineCount++;
    }
  }

  return 0;
}

// Check if placing val at (row, col) is valid
bool isValid(int row, int col, int val) {
  // Check row
  for (int i = 0; i < 9; i++) {
    if (puzzle[row][i] == val) return false;
  }

  // Check column
  for (int i = 0; i < 9; i++) {
    if (puzzle[i][col] == val) return false;
  }

  // Check 3x3 box
  int boxRow = (row ~/ 3) * 3;
  int boxCol = (col ~/ 3) * 3;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      if (puzzle[boxRow + i][boxCol + j] == val) return false;
    }
  }

  return true;
}

// BRUTE-FORCE SOLVER
// Searches row-major order (top-to-bottom, left-to-right)
// Tries candidates 1-9 in ascending order
// Counts EVERY placement attempt (the algorithm fingerprint)
bool solve() {
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
    print('\nSolved in Iterations=$count\n');
    return true; // Success
  }

  // Try values 1-9 in order
  for (int val = 1; val <= 9; val++) {
    count++; // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

    if (isValid(row, col, val)) {
      puzzle[row][col] = val; // Place value

      if (solve()) {
        return true; // Solved
      }

      puzzle[row][col] = 0; // Backtrack
    }
  }

  return false; // No solution found
}

void main(List<String> args) {
  Stopwatch stopwatch = Stopwatch()..start();

  // Process each .matrix file from command line
  for (String filename in args) {
    if (filename.endsWith('.matrix')) {
      if (readMatrixFile(filename) != 0) {
        stderr.writeln('Error reading $filename');
        continue;
      }

      printPuzzle();
      count = 0;
      solve();
    }
  }

  stopwatch.stop();
  double elapsed = stopwatch.elapsedMicroseconds / 1000000.0;
  print('Seconds to process ${elapsed.toStringAsFixed(3)}');
}
