#!/bin/bash
# Ralph Wiggum - Long-running AI agent loop (Gemini version)
# Usage: ./gemini_json_ralph.sh [max_iterations]

set -e

MAX_ITERATIONS=${1:-10}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PRD_FILE="$SCRIPT_DIR/prd.json"
PROGRESS_FILE="$SCRIPT_DIR/progress.txt"
ARCHIVE_DIR="$SCRIPT_DIR/archive"
LAST_BRANCH_FILE="$SCRIPT_DIR/.last-branch"
CODING_AGENT="gemini -y -o json"

# Archive previous run if branch changed
if [ -f "$PRD_FILE" ] && [ -f "$LAST_BRANCH_FILE" ]; then
  CURRENT_BRANCH=$(jq -r '.branchName // empty' "$PRD_FILE" 2>/dev/null || echo "")
  LAST_BRANCH=$(cat "$LAST_BRANCH_FILE" 2>/dev/null || echo "")
  
  if [ -n "$CURRENT_BRANCH" ] && [ -n "$LAST_BRANCH" ] && [ "$CURRENT_BRANCH" != "$LAST_BRANCH" ]; then
    # Archive the previous run
    # Format: YYYY-MM-DD-HHMMSS as per memory
    DATE=$(date +%Y-%m-%d-%H%M%S)
    # Strip "ralph/" prefix from branch name for folder
    FOLDER_NAME=$(echo "$LAST_BRANCH" | sed 's|^ralph/||')
    ARCHIVE_FOLDER="$ARCHIVE_DIR/$DATE-$FOLDER_NAME"
    
    echo "Archiving previous run: $LAST_BRANCH"
    mkdir -p "$ARCHIVE_FOLDER"
    [ -f "$PRD_FILE" ] && cp "$PRD_FILE" "$ARCHIVE_FOLDER/"
    [ -f "$PROGRESS_FILE" ] && cp "$PROGRESS_FILE" "$ARCHIVE_FOLDER/"
    echo "   Archived to: $ARCHIVE_FOLDER"
    
    # Reset progress file for new run
    echo "# Ralph Progress Log" > "$PROGRESS_FILE"
    echo "Started: $(date)" >> "$PROGRESS_FILE"
    echo "---" >> "$PROGRESS_FILE"
  fi
fi

# Track current branch
if [ -f "$PRD_FILE" ]; then
  CURRENT_BRANCH=$(jq -r '.branchName // empty' "$PRD_FILE" 2>/dev/null || echo "")
  if [ -n "$CURRENT_BRANCH" ]; then
    echo "$CURRENT_BRANCH" > "$LAST_BRANCH_FILE"
  fi
fi

# Initialize progress file if it doesn't exist
if [ ! -f "$PROGRESS_FILE" ]; then
  echo "# Ralph Progress Log" > "$PROGRESS_FILE"
  echo "Started: $(date)" >> "$PROGRESS_FILE"
  echo "---" >> "$PROGRESS_FILE"
fi

echo "Starting Ralph - Max iterations: $MAX_ITERATIONS"

for i in $(seq 1 $MAX_ITERATIONS); do
  echo ""
  echo "═══════════════════════════════════════════════════════"
  echo "  Ralph Iteration $i of $MAX_ITERATIONS"
  echo "═══════════════════════════════════════════════════════"
  
  # Run gemini with the ralph prompt
  # Capture JSON output from stdout. Allow stderr to pass through to console.
  JSON_OUTPUT=$(cat "$SCRIPT_DIR/prompt.md" | $CODING_AGENT) || true
  
  # Extract the text response
  # Use -r to get raw string (no quotes)
  OUTPUT_TEXT=$(echo "$JSON_OUTPUT" | jq -r '.response // empty')
  
  # Fallback if jq failed or response empty (e.g. error occurred)
  if [ -z "$OUTPUT_TEXT" ]; then
    echo "Warning: No valid JSON response or empty response field."
    echo "Raw Output: $JSON_OUTPUT"
    OUTPUT_TEXT="$JSON_OUTPUT"
  else
    # Print the clean text response for the user to see
    echo "$OUTPUT_TEXT"
  fi
  
  # Check for completion signal
  if echo "$OUTPUT_TEXT" | grep -q "<promise>COMPLETE</promise>"; then
    echo ""
    echo "Ralph completed all tasks!"
    echo "Completed at iteration $i of $MAX_ITERATIONS"

    # Auto-merge into main if on a ralph/* branch
    if [ -n "$CURRENT_BRANCH" ] && [[ "$CURRENT_BRANCH" == ralph/* ]]; then
      echo ""
      echo "═══════════════════════════════════════════════════════"
      echo "  Auto-merging $CURRENT_BRANCH into main"
      echo "═══════════════════════════════════════════════════════"

      cd "$(git rev-parse --show-toplevel)"
      git checkout main
      if git merge "$CURRENT_BRANCH" --no-edit; then
        echo "Merge successful!"
        git push origin main && echo "Pushed to origin/main"
      else
        echo "Merge failed - manual resolution required"
        git merge --abort 2>/dev/null || true
        git checkout "$CURRENT_BRANCH"
      fi
    fi

    exit 0
  fi
  
  echo "Iteration $i complete. Continuing..."
  sleep 2
done

echo ""
echo "Ralph reached max iterations ($MAX_ITERATIONS) without completing all tasks."
echo "Check $PROGRESS_FILE for status."
exit 1