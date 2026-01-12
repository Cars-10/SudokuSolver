#!/bin/bash
# Run the Benchmark Report Generation Script
cd "$(dirname "$0")/Metrics" && npx ts-node generate_report_only.ts 2>&1
