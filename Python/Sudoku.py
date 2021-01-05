#!/usr/bin/env python3
# coding: utf-8


import numpy as np
import csv
import sys

def printPuzzleValues():
    global puzzle
    print(np.matrix(puzzle))

def isPossible(y,x,val):
    """ Find if a matching number already exists
    in the same row or column
    """
    global puzzle
    
    for i in range(0,9):
        if puzzle[y][i] == val:
            return False
    for i in range(0,9):
        if puzzle[i][x] == val:
            return False
    x0=(x//3)*3
    y0=(y//3)*3
    for i in range(0,3):
        for j in range(0,3):
            if puzzle[y0+i][x0+j] == val:      
                return False
    #print("Is possible " ,x ,y ,val)
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
                        solve()
                        puzzle[j][i] = 0
                return
    print(np.matrix(puzzle))
    print("Iterations=" + str(count))
#    input("More?")

def decomment(csvfile):
    for row in csvfile:
        raw = row.split('#')[0].strip()
        if raw: yield raw

# For each .matrix file supplied on the commandline run the solver
for datafile in sys.argv:
    if datafile.endswith("matrix"):
        print(datafile)
        puzzle = np.genfromtxt(datafile, dtype='int', comments="#")
        print(np.matrix(puzzle))
        count = 0
        depth=1
        solve()
        printPuzzleValues()
