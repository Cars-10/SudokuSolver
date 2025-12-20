#!/bin/bash
# Wrapper to call the modular runMe.sh
cd "$(dirname "$0")"
./runMe.sh "$@"
