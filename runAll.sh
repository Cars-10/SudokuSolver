#!/bin/zsh
cd "$(dirname $0:A)"
for file in AI-2025/*/RunMe.sh; do 
    echo "Running $file..."
    $file 
done
echo "\nResults:"
./results.sh 