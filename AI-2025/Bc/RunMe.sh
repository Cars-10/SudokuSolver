#!/bin/bash
cd "$(dirname "$0")"

# Function to process a single matrix file
process_file() {
    file="$1"
    echo "$file"
    
    # Create a temporary bc input file
    # We need to read the matrix and format it as array assignments
    echo "scale=0" > input.bc
    
    row=0
    while IFS= read -r line; do
        [[ "$line" =~ ^#.*$ ]] && continue
        [[ -z "$line" ]] && continue
        
        read -ra parts <<< "$line"
        if [ ${#parts[@]} -eq 9 ]; then
            for col in {0..8}; do
                val="${parts[$col]}"
                idx=$((row * 9 + col))
                echo "puzzle[$idx] = $val" >> input.bc
            done
            ((row++))
        fi
    done < "$file"
    
    # Append solver code
    cat Sudoku.bc >> input.bc
    
    # Run bc
    # bc doesn't have a built-in timer, so we time it with date
    start=$(date +%s.%N)
    bc -q input.bc
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
    
    rm input.bc
}

for file in ../Matrices/*.matrix; do
    process_file "$file"
done
