#!/bin/bash
# Wrapper script to run the Sudoku Benchmark Suite on CleanedUp solvers
# Usage: ./runCleanedUpSuite.sh

export METRICS_FILE="CleanedUp_Metrics.json"

# Set the matrices to run (e.g., 1,1-3,6,"" for all matrices)
MATRICES="1,2,3,4,5"

# List of languages to benchmark
# Comment out languages you want to skip with #
LANGUAGES=(
#    "Ada"
#    "AppleScript"
#    "Assembly"
    "Awk"
#    "BASH"
#    "BASIC"
#    "Bc"
#    "C"
#    "C++"
#    "C_Sharp"
#    "Clojure"
#    "Cobol"
#    "CoffeeScript"
#    "CommonLisp"
#    "Crystal"
#    "D"
#    "Dart"
#    "Dash"
#    "Dc"
#    "Elixir"
#    "EmacsLisp"
#    "Erlang"
#    "Expect"
#    "F_Sharp"
#    "Fish"
#    "Forth"
#    "Fortran"
#    "Gnuplot"
#    "Go"
#    "Groovy"
#    "Haskell"
#    "Haxe"
#    "Java"
#    "JavaScript"
#    "Jq"
#    "Julia"
#    "Jupyter"
#    "Kotlin"
#    "Ksh"
#    "Lua"
#    "M4"
#    "Make"
#    "Nim"
#    "OCaml"
#    "Objective-C"
#    "Octave"
#    "PHP"
#    "Pascal"
#    "Perl"
#    "PostScript"
#    "PowerShell"
#    "Prolog"
#    "Python"
#    "R"
#    "Racket"
#    "Raku"
#    "Rexx"
#    "Ruby"
#    "Rust"
#    "SQL"
#    "SQLite"
#    "Scala"
#    "Scheme"
#    "Sed"
#    "Smalltalk"
#    "Swift"
#    "Tcl"
#    "Tcsh"
#    "TypeScript"
#    "V"
#    "Vala"
#    "Verilog"
#    "Vimscript"
#    "VisualBasic"
#    "XSLT"
#    "Zig"
#    "Zsh"
)

# Join array with commas
IFS=','
LANGUAGE_LIST="${LANGUAGES[*]}"
unset IFS

if [ -z "$LANGUAGE_LIST" ]; then
    echo "No languages selected. Please uncomment at least one language in the script."
    exit 1
fi

echo "Running CleanedUp Suite for languages: $LANGUAGE_LIST"
echo "Matrices: $MATRICES"
echo "Metrics will be saved to: $METRICS_FILE"

export OUTPUT_DIR="$(pwd)"
echo "Output Directory: $OUTPUT_DIR"

# Use ts-node to run the suite directly
# Assuming ts-node is in the path or installed globally/locally
if command -v ts-node &> /dev/null; then
    ts-node ../Metrics/run_suite.ts "$LANGUAGE_LIST" "$MATRICES"
else
    # Fallback to npx if ts-node is not in PATH
    npx ts-node ../Metrics/run_suite.ts "$LANGUAGE_LIST" "$MATRICES"
fi
