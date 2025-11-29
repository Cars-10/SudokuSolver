#!/bin/bash
cd "$(dirname "$0")"
# Run
elixir Sudoku.exs ../Matrices/*.matrix | tee run.txt
