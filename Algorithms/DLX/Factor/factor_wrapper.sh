#!/bin/bash
# Wrapper for Factor to convert relative matrix paths to absolute paths
FACTOR_BIN="/Applications/factor/factor"
SCRIPT="$1"
shift

# Convert relative paths to absolute paths
ARGS=()
for arg in "$@"; do
    if [[ "$arg" == *.matrix ]]; then
        # Convert to absolute path - handle if file exists
        if [ -e "$arg" ]; then
            ARGS+=("$(cd "$(dirname "$arg")" && pwd)/$(basename "$arg")")
        else
            # File doesn't exist, just pass as-is
            ARGS+=("$arg")
        fi
    else
        ARGS+=("$arg")
    fi
done

exec "$FACTOR_BIN" "$SCRIPT" "${ARGS[@]}"
