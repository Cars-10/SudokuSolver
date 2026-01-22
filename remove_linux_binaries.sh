#!/bin/bash
# remove_linux_binaries.sh
# Removes Linux ELF binaries that won't run on macOS
# They will be recompiled natively when runMe.sh is executed

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

LOG_FILE="remove_linux_binaries.log"

echo "=== Removing Linux ELF Binaries ===" | tee "$LOG_FILE"
echo "Started: $(date)" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"

removed=0
kept=0
checked=0

# Process each algorithm directory
for algo_dir in Algorithms/BruteForce Algorithms/DLX Algorithms/CP; do
    [ ! -d "$algo_dir" ] && continue

    echo "=== $(basename $algo_dir) ===" | tee -a "$LOG_FILE"

    # Find all executable files
    while IFS= read -r -d '' binary; do
        # Skip shell scripts
        [[ "$binary" == *.sh ]] && continue

        ((checked++))

        # Check file type
        file_type=$(file "$binary")

        if echo "$file_type" | grep -q "ELF"; then
            echo "  [DELETE] $binary" | tee -a "$LOG_FILE"
            rm -f "$binary"
            ((removed++))
        elif echo "$file_type" | grep -q "Mach-O"; then
            echo "  [KEEP]   $binary" | tee -a "$LOG_FILE"
            ((kept++))
        fi
    done < <(find "$algo_dir" -type f -perm +111 -print0)
done

echo "" | tee -a "$LOG_FILE"
echo "==============================" | tee -a "$LOG_FILE"
echo "=== Summary ===" | tee -a "$LOG_FILE"
echo "==============================" | tee -a "$LOG_FILE"
echo "Completed: $(date)" | tee -a "$LOG_FILE"
echo "Checked: $checked binaries" | tee -a "$LOG_FILE"
echo "Removed: $removed Linux ELF binaries" | tee -a "$LOG_FILE"
echo "Kept: $kept macOS Mach-O binaries" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"
echo "Linux binaries removed. They will be recompiled natively when:" | tee -a "$LOG_FILE"
echo "  cd Algorithms/BruteForce/{Language} && ./runMe.sh" | tee -a "$LOG_FILE"
echo "" | tee -a "$LOG_FILE"
echo "Log saved to: $LOG_FILE" | tee -a "$LOG_FILE"
