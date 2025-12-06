
import re

file_path = "../Metrics/HTMLGenerator.ts"

with open(file_path, 'r') as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    # Check for backtick not preceded by backslash
    # We iterate chars to be safe
    for j, char in enumerate(line):
        if char == '`':
            if j > 0 and line[j-1] == '\\':
                continue
            # Found unescaped backtick
            # Print line number and context
            print(f"Line {i+1}: {line.strip()}")
