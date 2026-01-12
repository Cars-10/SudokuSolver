 COMPILED LANGUAGES (brew uninstall):
      Ada         - brew uninstall gnat
      C/C++       - Keep (needed for many tools)
      Cobol       - brew uninstall gnucobol
      D           - brew uninstall dmd
      Dart        - brew uninstall dart
      Fortran     - brew uninstall gcc (provides gfortran)
      Go          - brew uninstall go
      Haskell     - brew uninstall ghc
      Haxe        - brew uninstall haxe
      Java        - brew uninstall openjdk
      Kotlin      - brew uninstall kotlin
      Nim         - brew uninstall nim
      Objective-C - Keep (part of Xcode)
      OCaml       - brew uninstall ocaml
      Pascal      - brew uninstall fpc
      Rust        - rustup self uninstall
      Scala       - brew uninstall scala
      V           - brew uninstall vlang
      Vala        - brew uninstall vala
      Zig         - brew uninstall zig

    INTERPRETED LANGUAGES (brew uninstall):
      Clojure     - brew uninstall clojure
      Elixir      - brew uninstall elixir
      Erlang      - brew uninstall erlang
      Groovy      - brew uninstall groovy
      Julia       - brew uninstall julia
      Lua         - brew uninstall lua
      Node.js     - brew uninstall node (for JS/TS/CoffeeScript)
      Perl        - Keep (system perl)
      PHP         - brew uninstall php
      Python      - Keep (system python)
      R           - brew uninstall r
      Racket      - brew uninstall racket
      Raku        - brew uninstall rakudo
      Ruby        - Keep (system ruby) or brew uninstall ruby
      Scheme      - brew uninstall mit-scheme (or guile/chicken)

    SHELL/SCRIPTING:
      Fish        - brew uninstall fish
      Ksh         - brew uninstall ksh
      Tcsh        - brew uninstall tcsh
      Zsh         - Keep (default macOS shell)

    ESOTERIC/SPECIALIZED:
      Awk         - Keep (system awk)
      Bc          - Keep (system bc)
      CommonLisp  - brew uninstall sbcl
      Forth       - brew uninstall gforth
      Gnuplot     - brew uninstall gnuplot
      Jq          - brew uninstall jq
      M4          - Keep (system m4)
      Make        - Keep (system make)
      Octave      - brew uninstall octave
      PostScript  - brew uninstall ghostscript
      Prolog      - brew uninstall swi-prolog
      Rexx        - brew uninstall regina-rexx
      SQLite      - Keep (system sqlite)
      Tcl         - brew uninstall tcl-tk
      XSLT        - Keep (system xsltproc)

    macOS-ONLY (keep for local runs):
      AppleScript - Cannot run in Docker (macOS only)

    To uninstall all at once:
      brew uninstall gnat gnucobol dmd dart gcc go ghc haxe openjdk kotlin nim ocaml fpc scala vlang vala zig clojure elixir
     erlang groovy julia lua node php r racket rakudo mit-scheme fish ksh tcsh sbcl gforth gnuplot jq octave ghostscript
    swi-prolog regina-rexx tcl-tk 2>/dev/null