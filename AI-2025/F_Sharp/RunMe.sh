#!/bin/bash
cd Sudoku
dotnet run ../../../Matrices/*.matrix | tee ../run.txt
