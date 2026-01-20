#!/usr/bin/env python3
"""Run fetch_metadata.py on test languages only"""
import os
import sys

# Change to project root
os.chdir('/Users/vibe/ClaudeCode/SudokuSolver')

# Modify ORDERED_LANGUAGES in the script temporarily
with open('scripts/fetch_metadata.py', 'r') as f:
    content = f.read()

# Save original
with open('scripts/fetch_metadata.py.bak', 'w') as f:
    f.write(content)

# Replace with test languages
test_langs = ["Python", "C", "Go", "Rust", "JavaScript"]
modified = content.replace(
    'langs_to_process = ORDERED_LANGUAGES',
    f'langs_to_process = {test_langs}'
)

# Write modified version
with open('scripts/fetch_metadata.py', 'w') as f:
    f.write(modified)

# Run it
print("Running fetch_metadata.py on test languages...")
os.system('python3 scripts/fetch_metadata.py 2>&1 | grep -v NotOpenSSLWarning')

# Restore original
with open('scripts/fetch_metadata.py.bak', 'r') as f:
    original = f.read()
with open('scripts/fetch_metadata.py', 'w') as f:
    f.write(original)
os.remove('scripts/fetch_metadata.py.bak')

print("\nDone! Check Algorithms/BruteForce/*/Media/ for author images")
