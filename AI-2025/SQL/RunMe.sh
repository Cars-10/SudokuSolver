#!/bin/bash
cd "$(dirname "$0")"

# Function to process a single matrix file
process_file() {
    file="$1"
    echo "$file"
    
    # Create a temporary SQL file
    echo "CREATE TABLE input (row INT, col INT, val INT);" > temp.sql
    echo "BEGIN TRANSACTION;" >> temp.sql
    
    # Parse matrix file and generate inserts
    row=0
    while IFS= read -r line; do
        # Skip comments and empty lines
        [[ "$line" =~ ^#.*$ ]] && continue
        [[ -z "$line" ]] && continue
        
        # Split line into array
        read -ra parts <<< "$line"
        if [ ${#parts[@]} -eq 9 ]; then
            for col in {0..8}; do
                val="${parts[$col]}"
                echo "INSERT INTO input VALUES ($row, $col, $val);" >> temp.sql
            done
            ((row++))
        fi
    done < "$file"
    
    echo "COMMIT;" >> temp.sql
    
    # Print Puzzle
    echo "SELECT '';" >> temp.sql
    echo "SELECT 'Puzzle:';" >> temp.sql
    echo "SELECT group_concat(val, ' ') FROM input GROUP BY row ORDER BY row;" >> temp.sql
    
    # Add Solver Logic
    cat Sudoku.sql >> temp.sql
    
    # Run SQLite
    start=$(date +%s.%N)
    sqlite3 :memory: < temp.sql > output.txt
    end=$(date +%s.%N)
    
    # Print Output
    cat output.txt
    
    # Calculate time (bash math)
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
    
    rm temp.sql output.txt
}

# Loop over all matrix files
for file in ../Matrices/*.matrix; do
    process_file "$file"
done
