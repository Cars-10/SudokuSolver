var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
var fs = require('fs');
// Step 1: Read a 9x9 space-separated matrix from a file and return it as a 1D array.
function readSudokuBoardFromFile(filePath) {
    var fileContent = fs.readFileSync(filePath, 'utf-8');
    var board = [];
    fileContent.split('\n').forEach(function (line) {
        if (!line.startsWith('#')) {
            var row = line.split(' ').map(Number);
            board = __spreadArrays(board, row);
        }
    });
    return board;
}
// Step 2: Calculate the complexity of a 9x9 matrix.
function calculateComplexity(board) {
    return board.reduce(function (acc, val) { return acc + (val === 0 ? 1 : 0); }, 0);
}
// Step 3: Print the board in a 9x9 grid.
function printBoard(board) {
    console.log('\nSudoku Board:');
    for (var i = 0; i < board.length; i++) {
        if (i % 9 === 0 && i !== 0)
            console.log('');
        process.stdout.write(board[i] + " ");
    }
    console.log('\n');
}
// Step 4: Solve the Sudoku board using a backtracking algorithm.
function solveSudoku(board) {
    var size = 9;
    var boxSize = 3;
    function findEmptyPosition(board) {
        for (var i = 0; i < size * size; i++) {
            if (board[i] === 0)
                return [Math.floor(i / size), i % size];
        }
        return null;
    }
    function isValidPlacement(board, num, pos) {
        var row = pos[0], col = pos[1];
        // Check rows and columns
        for (var i = 0; i < size; i++) {
            if (board[row * size + i] === num || board[i * size + col] === num)
                return false;
        }
        // Check box
        var boxRow = Math.floor(row / boxSize) * boxSize;
        var boxCol = Math.floor(col / boxSize) * boxSize;
        for (var r = 0; r < boxSize; r++) {
            for (var c = 0; c < boxSize; c++) {
                if (board[(boxRow + r) * size + (boxCol + c)] === num)
                    return false;
            }
        }
        return true;
    }
    var emptyPos = findEmptyPosition(board);
    if (!emptyPos)
        return true;
    for (var num = 1; num <= size; num++) {
        if (isValidPlacement(board, num, emptyPos)) {
            var row = emptyPos[0], col = emptyPos[1];
            board[row * size + col] = num;
            iterations++;
            if (solveSudoku(board))
                return true;
            board[row * size + col] = 0; // backtrack
        }
    }
    return false;
}
// Step 5: Print the final board and the number of iterations.
var iterations = 0;
function printSolution(board) {
    printBoard(board);
    console.log("Solved in " + iterations.toLocaleString() + " iterations.");
}
// Step 6: The matrices to read will be submitted on the command line.
var filePath = process.argv[2];
var sudokuBoard = readSudokuBoardFromFile(filePath);
printBoard(sudokuBoard);
console.log("Complexity: " + calculateComplexity(sudokuBoard));
if (solveSudoku(sudokuBoard)) {
    printSolution(sudokuBoard);
}
else {
    console.log('No solution found.');
}
