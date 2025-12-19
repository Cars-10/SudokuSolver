#!/bin/bash
# Wrapper for runMe.sh to match the projects new standard entry point
cd "$(dirname "$0")"
./runMe.sh "$@"
