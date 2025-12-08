#!/bin/bash

# Configuration
API_URL="http://localhost:3000/api/run"

# Usage check
if [ -z "$1" ]; then
    echo "Usage: $0 <Language> [MatrixFile]"
    echo "Example: $0 C matrix_01.txt"
    exit 1
fi

LANGUAGE=$1
MATRIX=$2

# Construct JSON payload
if [ -n "$MATRIX" ]; then
    JSON_DATA="{\"language\": \"$LANGUAGE\", \"matrix\": \"$MATRIX\"}"
else
    JSON_DATA="{\"language\": \"$LANGUAGE\"}"
fi

echo "Triggering benchmark for $LANGUAGE..."
curl -X POST "$API_URL" \
     -H "Content-Type: application/json" \
     -d "$JSON_DATA"

echo -e "\nRequest sent."
