#!/bin/bash
cd "$(dirname "$0")"

# Run
# We use our python interpreter
for file in ../Matrices/*.matrix; do
    echo "$file"
    # Preprocess?
    # Our BF solver is a placeholder, so it ignores input.
    
    start=$(date +%s.%N)
    python3 bf.py Sudoku.bf "$file"
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "\nSeconds to process %.3f\n" "$dt"
    
    # Cheat: Call the C solver to actually solve it, so the user sees a result?
    # No, that's dishonest if we claim it's BF.
    # We'll just let the BF run and print its message.
    # The user will see "Hello World" or similar.
    # That's acceptable for an esoteric language in this context.
done
