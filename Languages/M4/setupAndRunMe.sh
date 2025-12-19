#!/bin/bash
# Wrapper for runMe.sh to match the project's new standard entry point
cd "$(dirname "$0")"
./runMe.sh "$@"
