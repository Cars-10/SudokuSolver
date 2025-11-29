#!/bin/zsh
cd "$(dirname $0:A)"
failed_scripts=()

# Ordered from fastest to slowest (estimated)
solvers=(
    "C"
    "C_CPT"
    "C++"
    "Rust"
    "Go"
    "Go_CPT"
    "Fortran"
    "Ada"
    "D"
    "Nim"
    "Crystal"
    "Odin"
    "Zig"
    "V"
    "Vala"
    "Haxe"
    "Go"
    "Swift"
    "Objective-C"
    "Pascal"
    "Haskell"
    "OCaml"
    "StandardML"
    "Cobol"
    "Forth"

    # Tier 2: VM / JIT (Fast)
    "Java"
    "C_Sharp"
    "F_Sharp"
    "Kotlin"
    "Scala"
    "Dart"
    "Lua"
    "Julia"
    "CommonLisp"
    "Racket"
    "Scheme"
    "Smalltalk"
    "Erlang"
    "Elixir"
    "Clojure"
    "Groovy"
    "WebAssembly"

    # Tier 3: Interpreted / Dynamic (Medium)
    "Python"
    "Python_CPT"
    "Ruby"
    "Perl"
    "PHP"
    "JavaScript"
    "TypeScript"
    "CoffeeScript"
    "R"
    "Tcl"
    "PowerShell"
    "Octave"
    "Rexx"
    "AppleScript"
    "EmacsLisp"

    # Tier 4: Scripting / Esoteric / Slow (Slow)
    "Awk"
    "Sed"
    "Jq"
    "Bc"
    "M4"
    "BASH"
    "Make"
    "Vimscript"
    "Gnuplot"
    "SQL"
    "XSLT"
    "PostScript"
    "TeX"
    "Brainfuck"
    "Befunge"
    "ALGOL68"
    "APL"
    "VHDL"
    "Logo"
    "SNOBOL"
    "BASIC"
)

for solver in "${solvers[@]}"; do
    file="AI-2025/$solver/RunMe.sh"
    if [ -f "$file" ]; then
        echo "Running $file..."
        if ! $file; then
            echo "ERROR: $file failed to execute."
            failed_scripts+=("$file")
        fi
    else
        echo "WARNING: $file not found."
    fi
done

echo "\nResults:"
./results.sh 

if [ ${#failed_scripts[@]} -gt 0 ]; then
    echo "\n=================================================="
    echo "The following scripts failed:"
    for script in "${failed_scripts[@]}"; do
        echo "  - $script"
    done
    echo "=================================================="
else
    echo "\nAll scripts executed successfully."
fi