#!/bin/bash
cd "$(dirname "$0")"

# Build release version
cd Sudoku
cargo build --release --quiet

# Run
./target/release/Sudoku ../../../Matrices/*.matrix
