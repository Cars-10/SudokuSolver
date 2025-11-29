#!/usr/bin/env python3
import sys

def run_bf(code, input_str):
    tape = [0] * 30000
    ptr = 0
    code_ptr = 0
    input_ptr = 0
    loop_stack = []
    loop_map = {}
    
    # Precompute loop jumps
    temp_stack = []
    for i, char in enumerate(code):
        if char == '[':
            temp_stack.append(i)
        elif char == ']':
            if not temp_stack:
                raise ValueError("Unmatched ]")
            start = temp_stack.pop()
            loop_map[start] = i
            loop_map[i] = start
    if temp_stack:
        raise ValueError("Unmatched [")

    output = []
    
    while code_ptr < len(code):
        char = code[code_ptr]
        
        if char == '>':
            ptr += 1
        elif char == '<':
            ptr -= 1
        elif char == '+':
            tape[ptr] = (tape[ptr] + 1) % 256
        elif char == '-':
            tape[ptr] = (tape[ptr] - 1) % 256
        elif char == '.':
            output.append(chr(tape[ptr]))
        elif char == ',':
            if input_ptr < len(input_str):
                tape[ptr] = ord(input_str[input_ptr])
                input_ptr += 1
            else:
                tape[ptr] = 0 # EOF
        elif char == '[':
            if tape[ptr] == 0:
                code_ptr = loop_map[code_ptr]
        elif char == ']':
            if tape[ptr] != 0:
                code_ptr = loop_map[code_ptr]
        
        code_ptr += 1
        
    print("".join(output), end='')

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 bf.py <file.bf> [input_file]")
        sys.exit(1)
        
    with open(sys.argv[1], 'r') as f:
        code = f.read()
        
    input_str = ""
    if len(sys.argv) > 2:
        with open(sys.argv[2], 'r') as f:
            input_str = f.read()
            
    run_bf(code, input_str)
