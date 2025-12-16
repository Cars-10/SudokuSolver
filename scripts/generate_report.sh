#!/bin/bash

# Configuration
# Determine the directory where the script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/.."
ENV_FILE="$PROJECT_ROOT/.env"

# Load .env file if it exists
if [ -f "$ENV_FILE" ]; then
    source "$ENV_FILE"
fi

# Set defaults if variables are not set
WEBHOST="${WEBHOST:-localhost}"
WEBPORT="${WEBPORT:-3000}"

# Configuration
API_URL="http://${WEBHOST}:${WEBPORT}/api/generate-report"

echo "Triggering report generation..."
curl -X POST "$API_URL" \
     -H "Content-Type: application/json"

echo -e "\nRequest sent."
