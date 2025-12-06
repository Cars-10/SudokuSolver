#!/bin/bash
# Master benchmark runner that reads benchmark_config.json
# Usage: ./runBenchmarks.sh [language] [--all] [--pending]

cd "$(dirname "$0")"

CONFIG_FILE="benchmark_config.json"
LANGUAGES_DIR="Languages"

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
    ls -d "$LANGUAGES_DIR"/*/ 2>/dev/null | xargs -n1 basename
}

# Get completed languages from config
get_completed_languages() {
    jq -r '.completed[]?' "$CONFIG_FILE" 2>/dev/null
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

# Run benchmark for a language
run_benchmark() {
    local lang="$1"
    local lang_dir="$LANGUAGES_DIR/$lang"
    
    if [ ! -d "$lang_dir" ]; then
        echo -e "${RED}Error: Language directory not found: $lang_dir${NC}"
        return 1
    fi
    
    if [ ! -f "$lang_dir/setupAndRunMe.sh" ]; then
        echo -e "${RED}Error: No setupAndRunMe.sh found for $lang${NC}"
        return 1
    fi
    
    # Get matrices for this language
    local matrices=$(get_matrices_for_language "$lang")
    local matrix_args=""
    
    for m in $matrices; do
        matrix_args="$matrix_args ../../../Matrices/${m}.matrix"
    done
    
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}Running $lang${NC} with matrices: ${YELLOW}$matrices${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    cd "$lang_dir"
    ./setupAndRunMe.sh $matrix_args
    local result=$?
    cd - > /dev/null
    
    if [ $result -eq 0 ]; then
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
case "$1" in
    --status)
        show_status
        ;;
    --report)
        echo -e "${BLUE}Generating benchmark report...${NC}"
        cd ..
        npx ts-node Metrics/generate_report_only.ts
        echo -e "${GREEN}✓ Report generated: CleanedUp/benchmark_report.html${NC}"
        ;;
    --pending)
        echo -e "${YELLOW}Running all pending languages...${NC}"
        for lang in $(get_pending_languages); do
            run_benchmark "$lang"
        done
        ;;
    --all)
        echo -e "${YELLOW}Running all available languages...${NC}"
        for lang in $(get_available_languages); do
            run_benchmark "$lang"
        done
        ;;
    "")
        show_status
        echo ""
        echo "Usage: $0 [language] [--status] [--pending] [--all] [--report]"
        echo "  language   - Run benchmark for specific language"
        echo "  --status   - Show completion status"
        echo "  --pending  - Run all pending (incomplete) languages"
        echo "  --all      - Run all languages"
        echo "  --report   - Generate benchmark report"
        ;;
    *)
        run_benchmark "$1"
        ;;
esac
