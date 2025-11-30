#!/bin/bash
cd "$(dirname "$0")"

# Install dependencies if needed
if [ ! -d "node_modules" ]; then
    npm install performance-now
fi

# Run
node Sudoku.js ../Matrices/*.matrix
