#!/usr/bin/env python
# coding: utf-8

# In[10]:



# Easy
puzzle= ([9, 2, 0, 0, 0, 0, 5, 8, 4],
[0, 0, 0, 5, 0, 0, 0, 0, 3],
[0, 8, 3, 0, 9, 2, 0, 0, 0],
[2, 6, 0, 8, 5, 4, 0, 0, 1],
[0, 0, 5, 3, 6, 1, 0, 9, 0],
[1, 0, 0, 0, 0, 9, 0, 0, 0],
[8, 5, 0, 2, 0, 3, 0, 1, 0],
[4, 1, 2, 9, 8, 0, 0, 3, 0],
[3, 9, 0, 0, 0, 6, 8, 0, 0])

#hard
puzzle= ([0,0,3,0,0,5,0,0,4],
[5,0,0,9,8,1,0,0,0],
[0,0,0,0,0,0,0,2,0],
[2,0,0,7,0,0,9,0,0],
[0,8,0,0,9,0,0,3,0],
[0,0,9,0,0,2,0,0,1],
[0,3,0,0,0,0,0,0,0],
[0,0,0,1,4,9,0,0,5],
[9,0,0,3,0,0,8,0,0])

# Extreme
puzzle= ([0,9,0,6,0,0,0,7,0],
[0,5,2,0,7,0,0,4,8],
[0,8,0,0,0,1,0,2,0],
[0,0,5,0,0,0,0,0,3],
[0,1,0,0,0,0,0,6,0],
[3,0,0,0,0,0,4,0,0],
[0,4,0,2,0,0,0,1,0],
[2,6,0,0,4,0,5,3,0],
[0,3,0,0,0,6,0,9,0])

count = 0

import numpy as np
print(np.matrix(puzzle))
depth=1
import sys



# In[6]:


def printPuzzleValues():
    global puzzle
    print(np.matrix(puzzle))


# In[7]:


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


# In[8]:


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
    input("More?")
                    


# In[11]:


solve()


# In[12]:


printPuzzleValues()

