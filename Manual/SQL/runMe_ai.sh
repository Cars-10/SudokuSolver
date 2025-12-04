#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"
echo "[" > "$METRICS_FILE"

first=true
PATTERN="${1:-../../Matrices/*.matrix}"

for matrix in $PATTERN; do
    if [ "$first" = true ]; then
        first=false
    else
        echo "," >> "$METRICS_FILE"
    fi

    matrix_name=$(basename "$matrix")
    tmp_out=$(mktemp)
    tmp_err=$(mktemp)
    
    # Preprocess matrix to single string (replace . with 0, remove newlines/spaces)
    BOARD=$(grep -v '^#' "$matrix" | tr '\n' ' ' | sed 's/ //g' | sed 's/\./0/g')
    
    start_time=$(date +%s.%N)
    
    # Construct SQL
    # We use a recursive CTE.
    # We want to count total iterations (nodes visited).
    # To do this, we can't just SELECT the solution.
    # We need to run the CTE and count the rows?
    # But we also want the solution.
    # We can do: SELECT count(*) FROM x; AND SELECT s FROM x WHERE ind=81;
    # But that runs it twice?
    # Or we can have the CTE return a 'solution' flag?
    
    # Let's just solve it first.
    
    SQL="
WITH RECURSIVE
  digits(z) AS (VALUES ('1'),('2'),('3'),('4'),('5'),('6'),('7'),('8'),('9')),
  k(k) AS (VALUES (0),(1),(2),(3),(4),(5),(6),(7),(8)),
  
  x(s, ind) AS (
    SELECT '$BOARD', 0
    UNION ALL
    SELECT
      CASE 
        WHEN substr(s, ind+1, 1) = '0' THEN
          substr(s, 1, ind) || z || substr(s, ind+2)
        ELSE s
      END,
      ind + 1
    FROM x, digits
    WHERE ind < 81
      AND (
        (substr(s, ind+1, 1) != '0' AND z = substr(s, ind+1, 1))
        OR
        (substr(s, ind+1, 1) = '0' AND 
          NOT EXISTS (
            SELECT 1 FROM k WHERE 
              (substr(s, (ind/9)*9 + k + 1, 1) = z) OR
              (substr(s, (ind%9) + k*9 + 1, 1) = z) OR
              (substr(s, ((ind/27)*27 + ((ind%9)/3)*3) + (k/3)*9 + (k%3) + 1, 1) = z)
          )
        )
      )
  )
SELECT s, ind FROM x;
"
    
    # Run SQLite
    echo "$SQL" | sqlite3 > "$tmp_out" 2> "$tmp_err"
    
    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    
    # Count iterations (total rows generated)
    iterations=$(wc -l < "$tmp_out")
    
    # Find solution (row with ind=81)
    # SQLite output is "s|ind"
    SOLVED_BOARD=$(grep "|81$" "$tmp_out" | cut -d'|' -f1)
    
    if [ -n "$SOLVED_BOARD" ] && [ ${#SOLVED_BOARD} -eq 81 ]; then
        status="pass"
    else
        status="fail"
        echo "SQL Failed. Output:" >&2
        cat "$tmp_out" >&2
        echo "Error:" >&2
        cat "$tmp_err" >&2
    fi
    
    memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
    if [ -z "$memory" ]; then memory=0; fi
    
    cpu_user=$(grep "user" "$tmp_err" | awk '{print $3}')
    if [ -z "$cpu_user" ]; then cpu_user=0; fi
    
    cpu_sys=$(grep "sys" "$tmp_err" | awk '{print $5}')
    if [ -z "$cpu_sys" ]; then cpu_sys=0; fi
    
    echo "  {" >> "$METRICS_FILE"
    echo "    \"matrix\": \"$matrix_name\"," >> "$METRICS_FILE"
    echo "    \"time\": $duration," >> "$METRICS_FILE"
    echo "    \"iterations\": $iterations," >> "$METRICS_FILE"
    echo "    \"memory\": $memory," >> "$METRICS_FILE"
    echo "    \"cpu_user\": $cpu_user," >> "$METRICS_FILE"
    echo "    \"cpu_sys\": $cpu_sys," >> "$METRICS_FILE"
        echo "    \"status\": \"$status\"," >> "$METRICS_FILE"
    # Capture output safely
    json_output=$(cat "$tmp_out" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')
    echo "    \"output\": $json_output" >> "$METRICS_FILE"
    echo -n "  }" >> "$METRICS_FILE"
    
    rm "$tmp_out" "$tmp_err"
done

echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"

cat "$METRICS_FILE"
