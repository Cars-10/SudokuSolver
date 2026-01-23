#!/bin/bash
# Master benchmark runner that reads benchmark_config.json
# Usage: ./runBenchmarks.sh [language] [--all] [--pending]


cd "$(dirname "$0")"

CONFIG_FILE="benchmark_config.json"
ISSUES_FILE="benchmark_issues.json"
LANGUAGES_DIR="Algorithms"
TIMEOUT_DURATION="50m"
export TIMEOUT_SECONDS=3000 # 50 minutes in seconds for common.sh
MAX_JOBS=5
SLOW_LANGUAGES="PowerShell BASH DASH Fish Ksh M4 Make Awk Sed Make Vimscript Tcsh XSLT Zsh EmacsLisp"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if jq is available
if ! command -v jq &> /dev/null; then
    echo -e "${RED}Error: jq is required but not installed.${NC}"
    echo "Install with: brew install jq"
    exit 1
fi



# Get all available languages
get_available_languages() {
    local algo_filter="${ALGO_FILTER:-ALL}"
    
    local categories=""
    case "$algo_filter" in
        "BF"|"BruteForce") categories="BruteForce" ;;
        "CP"|"ConstraintPropagation") categories="CP" ;;
        "DLX"|"DancingLinks") categories="DLX" ;;
        "ALL") categories="BruteForce CP DLX" ;;
        *) echo "Error: Unknown algorithm filter $algo_filter" >&2; return 1 ;;
    esac

    for cat in $categories; do
        if [ -d "$LANGUAGES_DIR/$cat" ]; then
            ls "$LANGUAGES_DIR/$cat" 2>/dev/null | while read lang_name; do
                if [ -d "$LANGUAGES_DIR/$cat/$lang_name" ]; then
                    echo "$cat/$lang_name"
                fi
            done
        fi
    done | sort
}

# Get completed languages from config
get_completed_languages() {
    local all_completed=$(jq -r '.completed[]?' "$CONFIG_FILE" 2>/dev/null)
    local algo_filter="${ALGO_FILTER:-ALL}"
    
    if [ "$algo_filter" == "ALL" ]; then
        echo "$all_completed"
        return
    fi

    # Map filter to prefix
    local prefix=""
    case "$algo_filter" in
        "BF"|"BruteForce") prefix="BruteForce" ;;
        "CP"|"ConstraintPropagation") prefix="CP" ;;
        "DLX"|"DancingLinks") prefix="DLX" ;;
        *) echo "$all_completed"; return ;; 
    esac
    
    echo "$all_completed" | grep "^$prefix/"
}

# Get matrices for a language from config
get_matrices_for_language() {
    local lang="$1"
    local matrices=$(jq -r ".languages[\"$lang\"].matrices[]?" "$CONFIG_FILE" 2>/dev/null)
    if [ -z "$matrices" ]; then
        # Default to 1-5 if not specified
        echo "1 2 3 4 5"
    else
        echo "$matrices" | tr '\n' ' '
    fi
}

# Check if language is completed
is_completed() {
    local lang="$1"
    jq -e ".languages[\"$lang\"].status == \"complete\"" "$CONFIG_FILE" &>/dev/null
}

# Get pending languages (available but not completed)
get_pending_languages() {
    local available=$(get_available_languages)
    local completed=$(get_completed_languages)
    
    for lang in $available; do
        if ! echo "$completed" | grep -q "^$lang$"; then
            echo "$lang"
        fi
    done
}

get_slow_only_languages() {
    get_available_languages | while read lang; do
        local base_lang=$(basename "$lang")
        for slow in $SLOW_LANGUAGES; do
            if [ "$base_lang" == "$slow" ]; then
                echo "$lang"
                break
            fi
        done
    done
}

get_fast_languages() {
    local issues=""
    if [ -f "$ISSUES_FILE" ]; then
        issues=$(jq -r 'map("\(.algorithm)/\(.solver)") | .[]' "$ISSUES_FILE" 2>/dev/null)
    fi

    get_available_languages | while read lang; do
        local is_slow=0
        local base_name=$(basename "$lang")
        
        # Check if slow
        for s in $SLOW_LANGUAGES; do
            if [ "$base_name" == "$s" ]; then
                is_slow=1
                break
            fi
        done
        
        # Check if in issues
        local is_issue=0
        if [ -n "$issues" ]; then
            if echo "$issues" | grep -q "^$lang$"; then
                is_issue=1
            fi
        fi
        
        if [ $is_slow -eq 0 ] && [ $is_issue -eq 0 ]; then
            echo "$lang"
        fi
    done
}

# Run benchmark for a language
run_benchmark() {
    local lang="$1"
    local lang_dir=""
    
    # Try direct path first (e.g. BruteForce/C)
    if [ -d "$LANGUAGES_DIR/$lang" ]; then
        lang_dir="$LANGUAGES_DIR/$lang"
    else
        # If ALGO_FILTER is set (and not ALL), try that specific algorithm folder
        if [ "$ALGO_FILTER" != "ALL" ]; then
            local prefix=""
            case "$ALGO_FILTER" in
                "BF"|"BruteForce") prefix="BruteForce" ;;
                "CP"|"ConstraintPropagation") prefix="CP" ;;
                "DLX"|"DancingLinks") prefix="DLX" ;;
            esac
            
            if [ -n "$prefix" ] && [ -d "$LANGUAGES_DIR/$prefix/$lang" ]; then
                lang="$prefix/$lang"
                lang_dir="$LANGUAGES_DIR/$lang"
            fi
        # Default behavior: Try BruteForce if no specific path given and no strict filter
        elif [ -d "$LANGUAGES_DIR/BruteForce/$lang" ]; then
            # echo -e "${YELLOW}Assuming BruteForce variant for '$lang'...${NC}"
            lang="BruteForce/$lang"
            lang_dir="$LANGUAGES_DIR/$lang"
        fi
    fi

    if [ -z "$lang_dir" ]; then
        echo -e "${RED}Error: Language directory not found: $LANGUAGES_DIR/$lang${NC}"
        # Try to find it to be helpful
        local matches=$(find "$LANGUAGES_DIR" -maxdepth 2 -name "$(basename "$lang")" -type d 2>/dev/null)
        if [ -n "$matches" ]; then
             # Filter matches if ALGO_FILTER is set
            if [ "$ALGO_FILTER" != "ALL" ]; then
                 local prefix=""
                case "$ALGO_FILTER" in
                    "BF"|"BruteForce") prefix="BruteForce" ;;
                    "CP"|"ConstraintPropagation") prefix="CP" ;;
                    "DLX"|"DancingLinks") prefix="DLX" ;;
                esac
                matches=$(echo "$matches" | grep "$prefix/")
            fi
            
            if [ -n "$matches" ]; then
                echo -e "${YELLOW}Did you mean one of these?${NC}"
                echo "$matches" | sed "s|^$LANGUAGES_DIR/||"
            fi
        fi
        return 1
    fi
    
    if [ ! -f "$lang_dir/runMe.sh" ]; then
        echo -e "${RED}Error: No runMe.sh found for $lang in $lang_dir${NC}"
        return 1
    fi
    
    # Get matrices for this language
    local matrices=""
    if [ -n "$MATRIX_OVERRIDE" ]; then
        matrices="$MATRIX_OVERRIDE"
    else
        matrices=$(get_matrices_for_language "$lang")
    fi
    local matrix_args=""
    local matrix_dir="$(cd Matrices && pwd)"
    
    for m in $matrices; do
        matrix_args="$matrix_args $matrix_dir/${m}.matrix"
    done
    
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}Running $lang${NC} with matrices: ${YELLOW}$matrices${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    cd "$lang_dir"
    
    # Use timeout if available
    if command -v timeout &> /dev/null; then
        timeout "$TIMEOUT_DURATION" ./runMe.sh $matrix_args
    else
        ./runMe.sh $matrix_args
    fi
    local result=$?
    
    cd - > /dev/null
    
    if [ $result -eq 124 ]; then
        echo -e "${RED}✗ $lang timed out after $TIMEOUT_DURATION${NC}"
    elif [ $result -eq 0 ]; then
        echo -e "${GREEN}✓ $lang completed successfully${NC}"
        # Mark as complete in config
        mark_complete "$lang"
    else
        echo -e "${RED}✗ $lang failed${NC}"
    fi
    
    return $result
}

# Mark a language as complete in the config
mark_complete() {
    local lang="$1"
    
    # Update status to complete
    local tmp=$(mktemp)
    jq ".languages[\"$lang\"].status = \"complete\"" "$CONFIG_FILE" > "$tmp" && mv "$tmp" "$CONFIG_FILE"
    
    # Add to completed array if not present
    if ! jq -e ".completed | index(\"$lang\")" "$CONFIG_FILE" &>/dev/null; then
        tmp=$(mktemp)
        jq ".completed += [\"$lang\"]" "$CONFIG_FILE" > "$tmp" && mv "$tmp" "$CONFIG_FILE"
    fi
}

# Show status
show_status() {
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}BENCHMARK STATUS${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    local completed=$(get_completed_languages | wc -l | tr -d ' ')
    local available=$(get_available_languages | wc -l | tr -d ' ')
    local pending=$((available - completed))
    
    echo -e "Completed: ${GREEN}$completed${NC}"
    echo -e "Pending:   ${YELLOW}$pending${NC}"
    echo -e "Total:     $available"
    echo ""
    
    echo -e "${GREEN}Completed languages:${NC}"
    get_completed_languages | while read lang; do
        local matrices=$(get_matrices_for_language "$lang")
        echo -e "  ✓ $lang (matrices: $matrices)"
    done
    
    echo ""
    echo -e "${YELLOW}Pending languages:${NC}"
    get_pending_languages | head -20 | while read lang; do
        echo -e "  ○ $lang"
    done
    
    local more=$(get_pending_languages | tail -n +21 | wc -l | tr -d ' ')
    if [ "$more" -gt 0 ]; then
        echo -e "  ... and $more more"
    fi
}

# Main
# Parse arguments
MODE=""
TARGET=""
MATRIX_OVERRIDE=""
ALGO_FILTER="ALL"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --matrix)
            shift
            MATRIX_OVERRIDE="$1"
            shift
            ;;
        --recompile|--force)
            echo -e "${YELLOW}Forcing recompilation...${NC}"
            export FORCE_COMPILE=1
            shift
            ;;
        --status)
            MODE="status"
            shift
            ;;
        --report)
            MODE="report"
            shift
            ;;
        --pending)
            MODE="pending"
            shift
            ;;
        --all|-all)
            MODE="all"
            shift
            ;;
        --fast)
            MODE="fast"
            shift
            ;;
        --slow)
            MODE="slow"
            shift
            ;;
        --algo)
            shift
            # Normalize to canonical uppercase codes for consistency
            algo_upper=$(echo "$1" | tr '[:lower:]' '[:upper:]')
            case "$algo_upper" in
                "BF"|"BRUTEFORCE") ALGO_FILTER="BF" ;;
                "CP"|"CONSTRAINTPROPAGATION") ALGO_FILTER="CP" ;;
                "DLX"|"DANCINGLINKS") ALGO_FILTER="DLX" ;;
                "ALL") ALGO_FILTER="ALL" ;;
                *) 
                    echo -e "${RED}Error: Unknown algorithm filter '$1'. Use BF, CP, DLX, or ALL.${NC}"
                    exit 1
                    ;;
            esac
            shift
            ;;
        -*)
            echo -e "${RED}Error: Unknown option $1${NC}"
            exit 1
            ;;
        *)
            if [ -n "$TARGET" ]; then
                echo -e "${RED}Error: Multiple languages specified. Runs one at a time.${NC}"
                exit 1
            fi
            MODE="single"
            TARGET="$1"
            shift
            ;;
    esac
done

# Execute based on mode
case "$MODE" in
    status)
        show_status
        ;;
    report)
        echo -e "${BLUE}Generating benchmark report...${NC}"
        (cd Metrics && npx ts-node generate_report_only.ts)
        echo -e "${GREEN}✓ Report generated: index.html${NC}"
        ;;
    pending)
        echo -e "${YELLOW}Running all pending languages...${NC}"
        for lang in $(get_pending_languages); do
            run_benchmark "$lang"
        done
        ;;
    all)
        echo -e "${YELLOW}Running all available languages...${NC}"
        for lang in $(get_available_languages); do
            run_benchmark "$lang"
        done
        ;;
    fast)
        echo -e "${BLUE}Running FAST languages in parallel (Pool: $MAX_JOBS, Timeout: $TIMEOUT_DURATION)...${NC}"
        for lang in $(get_fast_languages); do
             # Job control loop for parallelism
            while [ $(jobs -r | wc -l) -ge $MAX_JOBS ]; do
                sleep 1
            done
            
            run_benchmark "$lang" &
        done
        wait
        ;;
    slow)
        echo -e "${YELLOW}Running SLOW languages in parallel (Pool: $MAX_JOBS, Timeout: $TIMEOUT_DURATION)...${NC}"
        for lang in $(get_slow_only_languages); do
             # Job control loop for parallelism
            while [ $(jobs -r | wc -l) -ge $MAX_JOBS ]; do
                sleep 1
            done
            
            run_benchmark "$lang" &
        done
        wait
        ;;
    single)
        # Check if TARGET is a direct match or needs expansion
        if [ -d "$LANGUAGES_DIR/$TARGET" ]; then
            run_benchmark "$TARGET"
        else
            # Try to find matches based on the available languages (which respects ALGO_FILTER)
            # We look for lines ending with "/$TARGET"
            MATCHES=$(get_available_languages | grep "/$TARGET$")
            
            if [ -n "$MATCHES" ]; then
                echo -e "${YELLOW}Running matches for '$TARGET' (Filter: $ALGO_FILTER):${NC}"
                for m in $MATCHES; do
                    run_benchmark "$m"
                done
            else
                # Fallback to original run_benchmark logic which handles error reporting/guessing
                run_benchmark "$TARGET"
            fi
        fi
        ;;
    *)
        show_status
        echo ""
        echo "Usage: $0 [--recompile] [language] [--status] [--pending] [--all] [--fast] [--slow] [--report]"
        echo "  --recompile - Force recompilation of binaries"
        echo "  language    - Run benchmark for specific language"
        echo "  --status    - Show completion status"
        echo "  --pending   - Run all pending (incomplete) languages"
        echo "  --all       - Run all languages"
        echo "  --fast      - Run fast languages (skip slow & known issues) in parallel"
        echo "  --slow      - Run only slow languages in parallel"
        echo "  --algo      - Filter by algorithm: BF, CP, DLX, ALL (default: ALL)"
        echo "  --matrix    - Override matrices to run (e.g. \"6\" or \"1 2\")"
        echo "  --report    - Generate benchmark report"
        ;;
esac
