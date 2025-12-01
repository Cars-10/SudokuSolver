import 'dart:io';

List<List<int>> puzzle = List.generate(9, (_) => List.filled(9, 0));
int count = 0;

void printPuzzle() {
  print("\nPuzzle:");
  for (int j = 0; j < 9; j++) {
    for (int i = 0; i < 9; i++) {
      stdout.write("${puzzle[j][i]} ");
    }
    print("");
  }
}

void readMatrixFile(String filename) {
  print(filename);
  File file = File(filename);
  List<String> lines = file.readAsLinesSync();
  int row = 0;
  for (String line in lines) {
    if (line.startsWith("#") || line.trim().isEmpty) continue;
    List<String> parts = line.trim().split(RegExp(r'\s+'));
    if (parts.length == 9) {
      for (int col = 0; col < 9; col++) {
        puzzle[row][col] = int.parse(parts[col]);
      }
      row++;
      if (row == 9) break;
    }
  }
}

bool isPossible(int y, int x, int val) {
  for (int i = 0; i < 9; i++) {
    if (puzzle[i][x] == val) return false;
    if (puzzle[y][i] == val) return false;
  }

  int x0 = (x ~/ 3) * 3;
  int y0 = (y ~/ 3) * 3;

  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      if (puzzle[y0 + i][x0 + j] == val) return false;
    }
  }
  return true;
}

int solve() {
  for (int j = 0; j < 9; j++) {
    for (int i = 0; i < 9; i++) {
      if (puzzle[j][i] == 0) {
        for (int val = 1; val <= 9; val++) {
          count++;
          if (isPossible(j, i, val)) {
            puzzle[j][i] = val;
            if (solve() == 2) return 2;
            puzzle[j][i] = 0;
          }
        }
        return 0;
      }
    }
  }
  printPuzzle();
  print("\nSolved in Iterations=$count\n");
  return 2;
}

void main(List<String> args) {
  Stopwatch stopwatch = Stopwatch()..start();
  for (String arg in args) {
    if (arg.endsWith(".matrix")) {
      readMatrixFile(arg);
      printPuzzle();
      count = 0;
      solve();
    }
  }
  stopwatch.stop();
  print("Seconds to process ${(stopwatch.elapsedMilliseconds / 1000.0).toStringAsFixed(3)}");
}
