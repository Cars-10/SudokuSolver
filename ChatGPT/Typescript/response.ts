const fs = require('fs');

// Step 1: Read a 9x9 space-separated matrix from a file and return it as a 1D array.
function readSudokuBoardFromFile(filePath: string): number[] {
  const fileContent = fs.readFileSync(filePath, 'utf-8');
  let board: number[] = [];
  fileContent.split('\n').forEach((line) => {
    if (!line.startsWith('#')) {
      const row = line.split(' ').map(Number);
      board = [...board, ...row];
    }
  });
  return board;
}

// Step 2: Calculate the complexity of a 9x9 matrix.
function calculateComplexity(board: number[]): number {
  return board.reduce((acc, val) => acc + (val === 0 ? 1 : 0), 0);
}

// Step 3: Print the board in a 9x9 grid.
function printBoard(board: number[]): void {
  console.log('\nSudoku Board:');
  for (let i = 0; i < board.length; i++) {
    if (i % 9 === 0 && i !== 0) console.log('');
    process.stdout.write(`${board[i]} `);
  }
  console.log('\n');
}

// Step 4: Solve the Sudoku board using a backtracking algorithm.
function solveSudoku(board: number[]): boolean {
  const size = 9;
  const boxSize = 3;

  function findEmptyPosition(board: number[]): number[] | null {
    for (let i = 0; i < size * size; i++) {
      if (board[i] === 0) return [Math.floor(i / size), i % size];
    }
    return null;
  }

  function isValidPlacement(board: number[], num: number, pos: [number, number]): boolean {
    const [row, col] = pos;

    // Check rows and columns
    for (let i = 0; i < size; i++) {
      if (board[row * size + i] === num || board[i * size + col] === num) return false;
    }

    // Check box
    const boxRow = Math.floor(row / boxSize) * boxSize;
    const boxCol = Math.floor(col / boxSize) * boxSize;

    for (let r = 0; r < boxSize; r++) {
      for (let c = 0; c < boxSize; c++) {
        if (board[(boxRow + r) * size + (boxCol + c)] === num) return false;
      }
    }

    return true;
  }

  const emptyPos = findEmptyPosition(board);

  if (!emptyPos) return true;

  for (let num = 1; num <= size; num++) {
    if (isValidPlacement(board, num, emptyPos)) {
      const [row, col] = emptyPos;
      board[row * size + col] = num;
      iterations++;

      if (solveSudoku(board)) return true;

      board[row * size + col] = 0; // backtrack
    }
  }

  return false;
}

// Step 5: Print the final board and the number of iterations.
let iterations = 0;
function printSolution(board: number[]): void {
  printBoard(board);
  console.log(`Solved in ${iterations.toLocaleString()} iterations.`);
}

// Step 6: The matrices to read will be submitted on the command line.
const filePath = process.argv[2];
const sudokuBoard = readSudokuBoardFromFile(filePath);
printBoard(sudokuBoard);
console.log(`Complexity: ${calculateComplexity(sudokuBoard)}`);

if (solveSudoku(sudokuBoard)) {
  printSolution(sudokuBoard);
} else {
  console.log('No solution found.');
}
