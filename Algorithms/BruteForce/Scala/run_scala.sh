#!/bin/bash
# Find Scala library path
if [ -d /root/.sdkman/candidates/scala/current/lib ]; then
    SCALA_LIB="/root/.sdkman/candidates/scala/current/lib"
elif [ -d /opt/homebrew/opt/scala/libexec/lib ]; then
    SCALA_LIB="/opt/homebrew/opt/scala/libexec/lib"
else
    SCALA_LIB="$(dirname $(which scalac))/../lib"
fi
java -cp ".:$SCALA_LIB/*" Sudoku "$@"
