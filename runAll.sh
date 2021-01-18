#!/bin/zsh
cd "$(dirname $0:A)"
for file in $(ls */RunMe.sh); do $file ; done
echo "\nResults:"
./results.sh 