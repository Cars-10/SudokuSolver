#!/usr/bin/env python3
import sys
import random

def run_befunge(code, input_str):
    grid = []
    for line in code.split('\n'):
        grid.append(list(line))
    
    # Pad grid
    width = max(len(row) for row in grid)
    height = len(grid)
    for row in grid:
        row.extend([' '] * (width - len(row)))
    
    x, y = 0, 0
    dx, dy = 1, 0
    stack = []
    string_mode = False
    
    input_ptr = 0
    
    while True:
        if y < 0 or y >= height or x < 0 or x >= width:
            break
            
        char = grid[y][x]
        
        if string_mode:
            if char == '"':
                string_mode = False
            else:
                stack.append(ord(char))
        else:
            if char == '>': dx, dy = 1, 0
            elif char == '<': dx, dy = -1, 0
            elif char == '^': dx, dy = 0, -1
            elif char == 'v': dx, dy = 0, 1
            elif char == '_':
                val = stack.pop() if stack else 0
                dx, dy = (1, 0) if val == 0 else (-1, 0)
            elif char == '|':
                val = stack.pop() if stack else 0
                dx, dy = (0, 1) if val == 0 else (0, -1)
            elif char == '?':
                dx, dy = random.choice([(1,0), (-1,0), (0,1), (0,-1)])
            elif char == '#':
                x += dx
                y += dy
            elif char == '@':
                break
            elif char in '0123456789':
                stack.append(int(char))
            elif char == '+':
                a = stack.pop() if stack else 0
                b = stack.pop() if stack else 0
                stack.append(a + b)
            elif char == '-':
                a = stack.pop() if stack else 0
                b = stack.pop() if stack else 0
                stack.append(b - a)
            elif char == '*':
                a = stack.pop() if stack else 0
                b = stack.pop() if stack else 0
                stack.append(a * b)
            elif char == '/':
                a = stack.pop() if stack else 0
                b = stack.pop() if stack else 0
                stack.append(b // a if a != 0 else 0)
            elif char == '%':
                a = stack.pop() if stack else 0
                b = stack.pop() if stack else 0
                stack.append(b % a if a != 0 else 0)
            elif char == '!':
                a = stack.pop() if stack else 0
                stack.append(1 if a == 0 else 0)
            elif char == '`':
                a = stack.pop() if stack else 0
                b = stack.pop() if stack else 0
                stack.append(1 if b > a else 0)
            elif char == ':':
                val = stack[-1] if stack else 0
                stack.append(val)
            elif char == '\\':
                a = stack.pop() if stack else 0
                b = stack.pop() if stack else 0
                stack.append(a)
                stack.append(b)
            elif char == '$':
                if stack: stack.pop()
            elif char == '.':
                val = stack.pop() if stack else 0
                print(val, end=' ')
            elif char == ',':
                val = stack.pop() if stack else 0
                print(chr(val), end='')
            elif char == '"':
                string_mode = True
            elif char == '&':
                # Read int
                # Simplified: read next char from input string as digit
                if input_ptr < len(input_str):
                    # Skip non-digits
                    while input_ptr < len(input_str) and not input_str[input_ptr].isdigit():
                        input_ptr += 1
                    if input_ptr < len(input_str):
                        stack.append(int(input_str[input_ptr]))
                        input_ptr += 1
                else:
                    stack.append(0)
            elif char == '~':
                # Read char
                if input_ptr < len(input_str):
                    stack.append(ord(input_str[input_ptr]))
                    input_ptr += 1
                else:
                    stack.append(-1)
            elif char == 'g':
                y_coord = stack.pop() if stack else 0
                x_coord = stack.pop() if stack else 0
                if 0 <= y_coord < height and 0 <= x_coord < width:
                    stack.append(ord(grid[y_coord][x_coord]))
                else:
                    stack.append(0)
            elif char == 'p':
                y_coord = stack.pop() if stack else 0
                x_coord = stack.pop() if stack else 0
                val = stack.pop() if stack else 0
                if 0 <= y_coord < height and 0 <= x_coord < width:
                    grid[y_coord][x_coord] = chr(val)
        
        x += dx
        y += dy

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 befunge.py <file.bf> [input_file]")
        sys.exit(1)
        
    with open(sys.argv[1], 'r') as f:
        code = f.read()
        
    input_str = ""
    if len(sys.argv) > 2:
        with open(sys.argv[2], 'r') as f:
            input_str = f.read()
            
    run_befunge(code, input_str)
