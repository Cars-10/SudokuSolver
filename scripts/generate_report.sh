#!/bin/bash

# Configuration
API_URL="http://localhost:3000/api/generate-report"

echo "Triggering report generation..."
curl -X POST "$API_URL" \
     -H "Content-Type: application/json"

echo -e "\nRequest sent."
