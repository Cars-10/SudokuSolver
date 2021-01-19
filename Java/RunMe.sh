#!/bin/zsh
cd "$(dirname $0:A)/sudoku/target/classes"
time /Library/Java/JavaVirtualMachines/adoptopenjdk-15.jdk/Contents/Home/bin/java org.cars10.sudoku.Sudoku ../../../../Matrices/*.matrix  | tee run.txt
