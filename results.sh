#!/bin/zsh
cd "$(dirname $0:A)"
a=$(grep process  */run.txt | tr ":" "\t")
a=${a// to process/}
echo ${a//\/run.txt/}