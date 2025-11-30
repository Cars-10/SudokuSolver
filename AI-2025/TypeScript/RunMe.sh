#!/bin/bash
cd "$(dirname "$0")"

# Install dependencies if needed
if [ ! -d "node_modules" ]; then
    npm install performance-now @types/node
fi

# Compile
# Assuming tsc is available
tsc Sudoku.ts --outDir out --esModuleInterop --moduleResolution node --target es2015

# Run
node out/Sudoku.js ../Matrices/*.matrix | tee run.txt
