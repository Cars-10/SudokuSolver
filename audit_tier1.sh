#!/bin/bash

# audit_tier1.sh
# Audits Tier 1 languages against Matrix 1 to verify algorithmic correctness.
# Expected output: "Solved in Iterations=656"

# List of Tier 1 Languages
LANGUAGES=(
  "Awk" "C" "C++" "Go" "Rust"
  "Python" "Ruby" "Perl" "JavaScript" "TypeScript"
  "Java" "Kotlin" "Swift" "Scala" "PHP"
  "C_Sharp" "D" "Nim" "Crystal" "Haskell"
  "OCaml" "Julia"
)

# Matrix Input
MATRIX_FILE="../../Matrices/1.matrix"
EXPECTED_ITERATIONS="Solved in Iterations=656"

# Output Formatting
printf "% -15s | % -15s | % -10s\n" "Language" "Iterations" "Status"
printf "%s\n" "----------------------------------------------"

RET_CODE=0

for lang in "${LANGUAGES[@]}"; do
  # Check if directory exists
  if [ ! -d "Languages/$lang" ]; then
    printf "% -15s | % -15s | % -10s\n" "$lang" "N/A" "ERROR: Dir missing"
    RET_CODE=1
    continue
  fi

  # Check if runMe.sh exists
  if [ ! -f "Languages/$lang/runMe.sh" ]; then
    printf "% -15s | % -15s | % -10s\n" "$lang" "N/A" "ERROR: No runMe.sh"
    RET_CODE=1
    continue
  fi

  # Run the benchmark with a timeout
  # We use a subshell to change directory and run
  # Remove old metrics to ensure fresh run
  rm -f "Languages/$lang/metrics.json"
  
  # Run the solver (capturing stdout/stderr to hide valid output, unless error)
  OUTPUT=$(cd "Languages/$lang" && timeout 60s ./runMe.sh "$MATRIX_FILE" > /dev/null 2>&1)
  EXIT_CODE=$?

  if [ $EXIT_CODE -eq 124 ]; then
    printf "% -15s | % -15s | % -10s\n" "$lang" "TIMEOUT" "FAIL"
    RET_CODE=1
    continue
  fi

  # Check if metrics.json was generated
  if [ ! -f "Languages/$lang/metrics.json" ]; then
    printf "% -15s | % -15s | % -10s\n" "$lang" "NONE" "ERROR: No metrics"
    RET_CODE=1
    continue
  fi

  # Extract iteration count using Python
  ITERATIONS=$(python3 -c "import json; data=json.load(open('Languages/$lang/metrics.json')); print(data[0]['results'][0]['iterations'])" 2>/dev/null)
  
  if [ -z "$ITERATIONS" ]; then
    printf "% -15s | % -15s | % -10s\n" "$lang" "NONE" "ERROR: Parse fail"
    RET_CODE=1
    continue
  fi

  # Validate
  if [ "$ITERATIONS" == "656" ]; then
    printf "% -15s | % -15s | % -10s\n" "$lang" "$ITERATIONS" "PASS"
  else
    printf "% -15s | % -15s | % -10s\n" "$lang" "$ITERATIONS" "FAIL"
    RET_CODE=1
  fi

done

exit $RET_CODE
