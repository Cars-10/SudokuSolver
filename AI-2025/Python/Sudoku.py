#!/usr/bin/env python3
import numpy as np
import csv
import sys
import time

start = time.time()

def printPuzzleValues():
    global puzzle
    print("\nPuzzle:")
#    print(np.array_str(np.matrix(puzzle)).translate(str.maketrans("[]", "  ")))
    print(np.array_str(np.matrix(puzzle)).replace(" [","").replace("[", "").replace("]",""))

def isPossible(y,x,val):
    """ Find if a matching number (val) already exists
    in the same row (y) or column (x) or within its rectangle
    """
    global puzzle
    #print(f"Is possible y={y} x={x} val={val}" ); 
    for i in range(0,9):
        if puzzle[i][x] == val:
            return False
    for i in range(0,9):
        if puzzle[y][i] == val:
            return False
    
    # Search the Rectangle containing x & y
    # Find which 3x3 square we are in using the floor quotient
    x0=(x//3)*3
    y0=(y//3)*3
    #print(f"Is possible y={y} y0={y0}, x={x} x0={x0}, val={val}" );
    for i in range(0,3):
        for j in range(0,3):
            #print(f"y0+i={y0+i} i={i}, x0+j={x0+j} j={j} Puzzle[y0+i][x0+j]={puzzle[y0+i][x0+j]}, val={val}")
            if puzzle[y0+i][x0+j] == val:
                return False
    #print(f"YES Is possible {y}, {x}, {val}")
    return True

def solve():
    global puzzle, count

    for j in range(0,9):
        for i in range(0,9):
            if puzzle[j][i] == 0:
                for val in range(1,10):
                    count += 1
                    if isPossible(j,i,val):
                        puzzle[j][i] = val
                        if (solve()==2): return 2;
                        puzzle[j][i] = 0
                return
    printPuzzleValues()
    print(f"\nSolved in Iterations={count}\n")
    return 2;


##### Main Program Starts Here #####
# For each .matrix file supplied on the commandline run the solver
for datafile in sys.argv:
    if datafile.endswith("matrix"):
        print(datafile)
        # Use NumPy library to ready the array easily
        puzzle = np.genfromtxt(datafile, dtype='int', comments="#")
        printPuzzleValues()
        count = 0
        solve()

print(f"Seconds to process {(time.time()-start):.3f}")
