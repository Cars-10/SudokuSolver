#!/bin/zsh
cd "$(dirname $0:A)"
a=$(grep process  */run.txt | tr ":" "\t")
echo ${a//\/run.txt/}