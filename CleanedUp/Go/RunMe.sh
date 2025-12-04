#!/bin/zsh
cd "$(dirname $0:A)"
time go run . ../Matrices/*.matrix | tee run.txt
