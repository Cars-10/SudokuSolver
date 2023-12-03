const fs = require('fs');

// Step 1: Read a 9x9 space-separated matrix from a file, ignoring comments
function readBoardFromFile(filePath) {
  const fileContent = fs.readFileSync(filePath, 'utf8');
  const lines = fileContent.split('\n');
  const board = [];
  for (let line of lines) {
    if (line.startsWith('#') || line.trim() === '') continue;
    board.push(...line.trim().split(/\\(s+/).map(Number));
  }
  return board;
}

// Step 2: Calculate the complexity of a 9x9 matrix
function calculateComplexity(board) {
  return board.reduce((acc, val) => acc + (val === 0 ? 1 : 0), 0);
}

// Step 3: Print the board in a 9x9 grid
function printBoard(board) {
  for (let i = 0; i < 9; i++) {
    const row = board.slice(i * 9, i * 9 + 9).join(' ');
    console.log(row);
  }
}

// Step 4: Solve the Sudoku board using a backtracking algorithm
function solveBoard(board) {
  let iterations = 0;

  function isSafe(board, row, col, num) {
    for (let x = 0; x < 9; x++) {
      const squareRow = 3 * Math.floor(row / 3) + Math.floor(x / 3);
      const squareCol = 3 * Math.floor(col / 3) + x % 3;
      if (
        board[row * 9 + x] === num ||
        board[x * 9 + col] === num ||
        board[squareRow * 9 + squareCol] === num
      ) {
        return false;
      }
    }
    return true;
  }

  function backtrack(board) {
    iterations++;
    for (let i = 0; i < board.length; i++) {
      if (board[i] === 0) {
        for (let num = 1; num <= 9; num++) {
          if (isSafe(board, Math.floor(i / 9), i % 9, num)) {
            board[i] = num;
            if (backtrack(board)) {
              return true;
            }
            board[i] = 0;
          }
        }
        return false;
      }
    }
    return true;
  }

  backtrack(board);
  return { solvedBoard: board, iterations };
}

// Step 5: Print the solved board and the number of iterations with commas
function printSolution(board, iterations) {
  printBoard(board);
  console.log(`Iterations: ${iterations.toLocaleString()}`);
}

// Step 6: Read matrices from the command line and solve them
const filePath = process.argv[2];
if (filePath) {
  const board = readBoardFromFile(filePath);
  console.log('Unsolved Board:');
  printBoard(board);
  const complexity = calculateComplexity(board);
  console.log(`Complexity: ${complexity}`);
  const { solvedBoard, iterations } = solveBoard(board);
  console.log('\nSolved Board:');
  printSolution(solvedBoard, iterations);
} else {
  console.error('Please provide the file path as a command line argument.');
}
