const fs = require('fs');
const path = require('path');

const metadataPath = path.join(__dirname, '../Languages/metadata.json');
let metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));

const updates = {
    "Ada": {
        history: "1980. Designed for safety-critical systems.",
        paradigm: "Multi-paradigm, structured, object-oriented",
        typeSystem: "Static, Strong, Safe"
    },
    "AppleScript": {
        history: "1993. Automates Mac applications naturally.",
        paradigm: "Scripting, object-oriented, natural language",
        typeSystem: "Dynamic"
    },
    "Assembly": {
        history: "1947. The closest you can get to the metal.",
        paradigm: "Imperative, unstructured",
        typeSystem: "Untyped"
    },
    "Awk": {
        history: "1977. The original data extraction tool.",
        paradigm: "Scripting, procedural, data-driven",
        typeSystem: "Dynamic, Weak"
    },
    "BASH": {
        history: "1989. The default shell for Linux and macOS.",
        description: "The Bourne Again SHell. The default shell for most Linux distributions and macOS. It unified scripting and interactive use on Unix-like systems.",
        paradigm: "Scripting, command line interface",
        typeSystem: "Dynamic, Stringly-typed"
    },
    "BASIC": {
        history: "1964. Brought programming to the masses.",
        paradigm: "Procedural, imperative",
        typeSystem: "Static (mostly), Weak"
    },
    "Bc": {
        history: "1975. Arbitrary-precision calculator language.",
        paradigm: "Procedural, scripting",
        typeSystem: "Dynamic, Numeric"
    },
    "Brainfuck": {
        history: "1993. Minimalist esoteric language.",
        paradigm: "Esoteric, imperative",
        typeSystem: "Untyped"
    },
    "C": {
        history: "1972. The foundation of modern computing.",
        paradigm: "Imperative, procedural, structured",
        typeSystem: "Static, Weak, Unsafe"
    },
    "C_Sharp": {
        history: "2000. Microsoft's modern enterprise flagship.",
        paradigm: "Multi-paradigm, object-oriented, structured",
        typeSystem: "Static, Strong, Safe"
    },
    "C++": {
        history: "1985. C with Classes, evolved.",
        paradigm: "Multi-paradigm, procedural, object-oriented",
        typeSystem: "Static, Strong, Unsafe"
    },
    "Clojure": {
        history: "2007. Lisp on the JVM.",
        paradigm: "Functional, dynamic, concurrent",
        typeSystem: "Dynamic, Strong"
    },
    "Cobol": {
        history: "1959. Powering global finance for decades.",
        paradigm: "Imperative, procedural, object-oriented",
        typeSystem: "Static, Weak"
    },
    "CoffeeScript": {
        history: "2009. It's just JavaScript.",
        paradigm: "Multi-paradigm, scripting",
        typeSystem: "Dynamic, Weak"
    },
    "CommonLisp": {
        history: "1984. The industrial-strength Lisp standard.",
        paradigm: "Multi-paradigm, functional, procedural",
        typeSystem: "Dynamic, Strong"
    },
    "Crystal": {
        history: "2014. Fast as C, slick as Ruby.",
        paradigm: "Object-oriented, functional, imperative",
        typeSystem: "Static, Strong, Inferred"
    },
    "D": {
        history: "2001. A better C++ that never quite took off.",
        description: "A general-purpose programming language with static typing, systems-level access, and C-like syntax.",
        paradigm: "Multi-paradigm, object-oriented, imperative",
        typeSystem: "Static, Strong"
    },
    "Dart": {
        history: "2011. Client-optimized for fast apps (Flutter).",
        paradigm: "Object-oriented, class-based, structured",
        typeSystem: "Static, Strong, Sound"
    },
    "Dash": {
        history: "1997. A faster, smaller POSIX shell.",
        paradigm: "Scripting, command line",
        typeSystem: "Dynamic, Stringly-typed"
    },
    "Dc": {
        history: "1970. The oldest Unix utility still in use.",
        paradigm: "Stack-based, reverse polish notation",
        typeSystem: "Dynamic, Numeric"
    },
    "Elixir": {
        history: "2011. Scalable, fault-tolerant apps on Erlang VM.",
        paradigm: "Functional, concurrent, distributed",
        typeSystem: "Dynamic, Strong"
    },
    "EmacsLisp": {
        history: "1985. The programmable text editor.",
        description: "A dialect of the Lisp programming language used as a scripting language by Emacs.",
        paradigm: "Functional, procedural, reflective",
        typeSystem: "Dynamic, Strong"
    },
    "Erlang": {
        history: "1986. Built for telecom systems that never fail.",
        paradigm: "Functional, concurrent, distributed",
        typeSystem: "Dynamic, Strong"
    },
    "F_Sharp": {
        history: "2005. Functional-first programming on .NET.",
        paradigm: "Functional, imperative, object-oriented",
        typeSystem: "Static, Strong, Inferred"
    },
    "Fennel": {
        history: "2016. Lisp that compiles to Lua.",
        paradigm: "Functional, Lisp-dialect",
        typeSystem: "Dynamic"
    },
    "Fish": {
        history: "2005. The user-friendly command line shell.",
        paradigm: "Scripting, command line",
        typeSystem: "Dynamic"
    },
    "Forth": {
        history: "1970. Concatenative, stack-based efficiency.",
        paradigm: "Concatenative, stack-oriented",
        typeSystem: "Untyped"
    },
    "Fortran": {
        history: "1957. The first high-level language.",
        paradigm: "Imperative, procedural, structured",
        typeSystem: "Static, Strong"
    },
    "Gnuplot": {
        history: "1986. Portable command-line graphing utility.",
        paradigm: "Scripting, declarative",
        typeSystem: "Dynamic"
    },
    "Go": {
        history: "2009. Google's answer to complexity.",
        description: "A statically typed, compiled programming language designed at Google. It is syntactically similar to C, but with memory safety, garbage collection, structural typing, and CSP-style concurrency.",
        paradigm: "Multi-paradigm, procedural, concurrent",
        typeSystem: "Static, Strong, Structural"
    },
    "Groovy": {
        history: "2003. Dynamic power for the JVM.",
        paradigm: "Object-oriented, scripting",
        typeSystem: "Dynamic (optional static)"
    },
    "Haskell": {
        history: "1990. Purely functional research language.",
        description: "A statically typed, purely functional programming language with type inference and lazy evaluation.",
        paradigm: "Purely functional, lazy",
        typeSystem: "Static, Strong, Inferred"
    },
    "Haxe": {
        history: "2005. One language, all platforms.",
        paradigm: "Multi-paradigm, object-oriented",
        typeSystem: "Static, Strong"
    },
    "Icon": {
        history: "1977. Goal-directed execution strings.",
        paradigm: "Imperative, goal-directed",
        typeSystem: "Dynamic"
    },
    "Janet": {
        history: "2017. Modern, embeddable Lisp.",
        paradigm: "Functional, imperative",
        typeSystem: "Dynamic, Strong"
    },
    "Java": {
        history: "1995. Write once, run anywhere.",
        description: "A high-level, class-based, object-oriented programming language that is designed to have as few implementation dependencies as possible.",
        paradigm: "Multi-paradigm, object-oriented, structured",
        typeSystem: "Static, Strong, Safe"
    },
    "JavaScript": {
        history: "1995. The language of the web.",
        description: "A lightweight, interpreted, or just-in-time compiled programming language with first-class functions.",
        paradigm: "Multi-paradigm, event-driven, functional",
        typeSystem: "Dynamic, Weak"
    },
    "Jq": {
        history: "2012. Sed for JSON data.",
        paradigm: "Functional, filter-based",
        typeSystem: "Dynamic"
    },
    "Julia": {
        history: "2012. Looks like Python, runs like C.",
        paradigm: "Multi-paradigm, functional, imperative",
        typeSystem: "Dynamic, Parametric"
    },
    "Jupyter": {
        history: "2014. Interactive computing notebooks.",
        paradigm: "Interactive, literate programming",
        typeSystem: "Dynamic"
    },
    "Kotlin": {
        history: "2011. Concise, safe JVM language.",
        description: "A cross-platform, statically typed, general-purpose programming language with type inference. Kotlin is designed to interoperate fully with Java.",
        paradigm: "Multi-paradigm, object-oriented, functional",
        typeSystem: "Static, Strong, Inferred"
    },
    "Ksh": {
        history: "1983. The foundation of modern shells.",
        paradigm: "Scripting, command line",
        typeSystem: "Dynamic"
    },
    "Lua": {
        history: "1993. Fast, lightweight, embeddable scripting.",
        description: "A lightweight, high-level, multi-paradigm programming language designed primarily for embedded use in applications.",
        paradigm: "Multi-paradigm, scripting, procedural",
        typeSystem: "Dynamic, Weak"
    },
    "M4": {
        history: "1977. The universal macro processor.",
        paradigm: "Macro, text-replacement",
        typeSystem: "Untyped"
    },
    "Make": {
        history: "1976. Dependency-tracking build tool.",
        paradigm: "Declarative, automation",
        typeSystem: "String-based"
    }
};

Object.keys(updates).forEach(lang => {
    if (metadata.languageMetadata[lang]) {
        Object.assign(metadata.languageMetadata[lang], updates[lang]);
    } else {
        console.warn(`Language ${lang} not found in metadata`);
    }
});

fs.writeFileSync(metadataPath, JSON.stringify(metadata, null, 4));
console.log("Metadata updated for batch A-M.");
