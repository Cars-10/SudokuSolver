// LanguagesMetadata.ts - Language-specific metadata only
// Persona-related data (personalities, labels, intros) has been moved to PersonaMetadata.ts

// Re-export persona data for backward compatibility
export {
    methodologyTexts,
    mismatchLabels,
    iterationLabels,
    timeLabels,
    memoryLabels,
    scoreLabels,
    narratorIntros,
    personalities
} from './PersonaMetadata.js';

// --- Master Language List (Ordered by Performance/Tier) ---
export const orderedLanguages = [
    "C", "C++", "Rust", "Zig", "Ada", "Fortran", "Pascal", "D", "Nim", "Crystal", "V", "Vala", "Go",
    "Java", "C_Sharp", "F_Sharp", "Scala", "Kotlin", "Swift", "Dart", "Julia", "R", "Haxe",
    "JavaScript", "TypeScript", "CoffeeScript", "Lua", "Python", "Ruby", "PHP", "Perl", "Raku", "Groovy", "Wren", "Red",
    "Erlang", "Elixir", "Haskell", "OCaml", "CommonLisp", "Scheme", "Racket", "Clojure", "EmacsLisp", "Vimscript",
    "Smalltalk", "Objective-C", "VisualBasic", "Cobol", "Prolog", "Rexx", "Tcl",
    "Bash", "Zsh", "Fish", "Ksh", "Tcsh", "Dash", "PowerShell", "AppleScript", "Make", "M4",
    "Awk", "Sed", "Bc", "Dc", "Jq",
    "SQLite", "XSLT", "Gnuplot", "PostScript",
    "Assembly", "Verilog", "BASIC", "Forth", "Jupyter", "Octave"
];

// --- Language Quotes (short descriptions per language) ---
export const quotes: Record<string, string> = {
    "C": "Old iron. Fast, dangerous, and doesn't care if you bleed.",
    "C++": "Chrome-plated complexity. Powerful, but the weight of the past drags it down.",
    "Rust": "The new flesh. Memory safe, fearless concurrency. It's the future, if you can survive the borrow checker.",
    "Go": "Google's brutalist architecture. Simple, efficient, no soul. It gets the job done.",
    "Python": "Slick, interpreted glue. It holds the sprawl together, but don't ask it to run a marathon.",
    "JavaScript": "The universal solvent. It runs everywhere, even where it shouldn't. Chaos incarnate.",
    "TypeScript": "JavaScript with a suit and tie. It pretends to be civilized, but the chaos is still underneath.",
    "Java": "The corporate monolith. Verbose, heavy, reliable. It runs the banking systems of the old world.",
    "C_Sharp": "Microsoft's answer to the monolith. Sleek, refined, but locked in the garden.",
    "PHP": "The cockroach of the web. It survives everything. Ugly, but it works.",
    "Ruby": "A programmer's best friend. Elegant, expressive, slow as molasses.",
    "Swift": "Apple's walled garden fruit. Fast, safe, but you play by their rules.",
    "Kotlin": "Java's cooler younger brother. Less boilerplate, more fun.",
    "Scala": "Academic purity meets JVM reality. Functional, powerful, confusing.",
    "Haskell": "Pure math. Side effects are forbidden. It solves Sudoku in a parallel universe.",
    "Ocaml": "The French connection. Functional, fast, obscure.",
    "Lisp": "Lost in a sea of parentheses. The AI language of the golden age.",
    "Clojure": "Lisp on the JVM. Data is code, code is data. The parentheses are eternal.",
    "Erlang": "Built for the telephone network. It handles failure like a stoic. Let it crash.",
    "Elixir": "Erlang for hipsters. Ruby syntax, telecom reliability.",
        "XSLT": "XML transformation. A forgotten art, transforming data like alchemy.",
    "Sed": "Stream editor. Ancient magic. Modifying the flow of data byte by byte.",
    "Awk": "Pattern scanning and processing. The scalpel of the command line.",
    "Perl": "Write once, read never. The Swiss Army chainsaw of text processing.",
    "Bash": "The shell. It binds the system together. Ugly, dangerous, essential.",
    "Assembly": "The machine's native tongue. Raw power. No safety net.",
    "Fortran": "Number crunching from the dawn of time. It still calculates the stars.",
    "Cobol": "The undead. It runs the financial systems. It will outlive us all.",
    "Pascal": "Structured programming. A teaching tool that grew up.",
    "Ada": "Designed by committee for the military. Safe, robust, bureaucratic.",
    "Zig": "The new C. Manual memory management, but with better tools.",
    "Nim": "Python syntax, C speed. A hidden gem.",
    "Crystal": "Ruby syntax, C speed. Another contender.",
    "D": "C++ done right, but too late.",
    "Julia": "Python's ease, C's speed. Built for science.",
    "R": "Statistical computing. Great for data, terrible for Sudoku.",
    "Lua": "Embedded scripting. Light, fast, ubiquitous in games.",
    "Tcl": "Tool Command Language. Everything is a string.",
    "Prolog": "Logic programming. You describe the problem, it finds the solution.",
    "Smalltalk": "Everything is an object. The grandfather of OOP.",
    "Verilog": "Hardware description. You're not writing code, you're designing circuits.",
    "VHDL": "Hardware description. Verbose, strict, powerful.",
    "WebAssembly": "The binary instruction format for a stack-based virtual machine. The web's assembly.",
    "Befunge": "Esoteric stack-based. Code is a 2D grid. Madness.",
    "Brainfuck": "Minimalist esoteric. 8 commands. Pure pain.",
    "M4": "Macro processor. It expands text. A preprocessor for everything.",
    "Make": "Build automation. It knows dependencies. It runs the world.",
    "Gnuplot": "Plotting utility. It draws graphs. Not meant for Sudoku.",
    "Octave": "Matlab clone. Numerical computing.",
    "PostScript": "Page description. It prints documents. It's also a language.",
    "TeX": "Typesetting. It formats text. It's Turing complete.",
    "Vimscript": "The editor's language. It automates text editing.",
    "AppleScript": "Automation for Mac. English-like syntax. Weird.",
    "PowerShell": "Windows automation. Object-oriented shell.",
    "Rexx": "Mainframe scripting. Easy to learn, hard to kill.",
    "Red": "Rebol's successor. Symbolic, functional, and full-stack. A hidden powerhouse.",
    "SNOBOL": "String manipulation. Ancient and powerful.",
    "Vala": "GObject type system. C# syntax for C.",
    "V": "Simple, fast, safe. A new contender.",
    "Odin": "Data-oriented. Built for games.",
    "F_Sharp": "Functional on .NET.",
    "Forth": "Stack-based. Postfix notation. You define the language.",
    "Jq": "JSON processor. It slices and dices JSON.",
    "Bc": "Arbitrary precision calculator. It does math.",
    "Basic": "Beginner's All-purpose Symbolic Instruction Code. Where we all started.",
    "Algol68": "The ancestor. It defined the block structure.",
    "APL": "A Programming Language. cryptic symbols. Array processing.",
    "Jupyter": "Interactive notebooks. Data science playground.",
    "Wren": "Small, fast, class-based. Like Lua, but with more feathers.",
    "Matrices": "The input data. The puzzle itself.",
    "Matrices_Backup": "The backup.",
    "Matrices_Filtered": "The filter."
};

// --- Language Histories (historical context per language) ---
export const languageHistories: Record<string, string> = {
    "Assembly": "1949. Born from the earliest machines, assembly languages provide a symbolic layer over raw machine code. Programmers using assembly have fine-grained control over registers, memory layout and instruction scheduling — essential for firmware and tight performance hotspots.",
    "Awk": "1977. Created by Aho, Weinberger and Kernighan at Bell Labs as a concise domain language for text processing. Awk remains ideal for quick data extraction and on-the-fly reporting in shell pipelines.",
    "Bash": "1989. Brian Fox's Bourne Again SHell unified scripting and interactive use on Unix-like systems. Beyond interactive shells, Bash scripts glue together tools, automate builds and manage system tasks.",
    "Basic": "1964. Beginner's All-purpose Symbolic Instruction Code (BASIC) was created by John Kemeny and Thomas Kurtz at Dartmouth College to give students easy access to computing. Early implementations ran on the Dartmouth Time-Sharing System and emphasized simple, interactive use (PRINT, LET, GOTO, line numbers). BASIC exploded in popularity on microcomputers in the 1970s and 1980s via many dialects (and influential ports such as Altair BASIC), helping introduce programming to hobbyists and a generation of developers.",
    "Befunge": "1993. An esoteric two-dimensional language by Chris Pressey where instructions move on a grid and programs can self-modify. Befunge is mostly a thought experiment used for puzzles and language-design challenges.",
    "Brainfuck": "1993. Urban Müller's minimalist language reduces computation to eight commands and an instruction pointer. Primarily an academic curiosity, it highlights what is required for Turing-completeness.",
    "C": "1972. Dennis Ritchie's C balanced low-level access with structured programming and portability, shaping decades of systems software. Its influence is evident in modern compilers, runtimes and standards.",
    "C++": "1985. Bjarne Stroustrup extended C with abstractions like classes and templates to enable both low-level control and high-level design. C++ powers performance-critical applications, from game engines to embedded systems.",
    "C_Sharp": "2000. Designed by Anders Hejlsberg, C# blends modern language features with the .NET runtime to support enterprise, desktop and web applications. It emphasizes tooling, libraries and developer productivity.",
    "Clojure": "2007. Rich Hickey's Clojure brings Lisp's code-as-data and immutable data structures to the JVM, focusing on simplicity, concurrency and functional programming. It's used for building robust, composable systems.",
    "Cobol": "1959. Created for business data processing, COBOL's verbose, English-like syntax made it accessible to non-academic programmers and it still runs critical financial systems today.",
    "CoffeeScript": "2009. A syntactic layer over JavaScript that introduced concise idioms and inspired later JS syntax improvements. CoffeeScript smoothed the migration to more expressive JavaScript patterns.",
    "CommonLisp": "1984. A standardized Lisp dialect with powerful macros and dynamic runtime features, Common Lisp supports rapid prototyping and domain-specific language creation.",
    "Crystal": "2014. Crystal aims to deliver Ruby-like syntax with static typing and native performance, targeting developers who want expressive code without sacrificing speed.",
    "D": "2001. Walter Bright's D modernizes systems programming by adding safety and productivity features while keeping C-like performance. It targets high-performance, maintainable code.",
    "Dart": "2011. Created for structured client-side development, Dart powers Flutter for cross-platform UIs and compiles to efficient native or JS code. Its toolchain focuses on developer productivity.",
    "Elixir": "2011. Built on the Erlang VM by José Valim, Elixir blends fault-tolerant concurrency with elegant syntax and tooling, favored for scalable distributed services.",
    "EmacsLisp": "1985. The extensible scripting language that turns Emacs into a programmable environment for editing, tooling and experimentation. Emacs Lisp lets users customize and extend editor behaviour deeply.",
    "Erlang": "1986. Designed for telecoms, Erlang emphasizes lightweight processes, message-passing concurrency and robust fault-recovery. It's a foundation for resilient distributed systems.",
    "F_Sharp": "2005. F# brings functional-first programming to .NET with strong typing, succinct syntax and excellent interop, used in finance, analytics and domain modelling.",
    "Fortran": "1957. One of the first high-level languages, Fortran was built for numerical computation and scientific programming; optimized compilers and legacy code keep it relevant in HPC.",
    "Go": "2009. Designed at Google for simplicity, fast compilation and pragmatic concurrency, Go is a popular choice for cloud services, networking and developer tools.",
    "Groovy": "2003. A dynamic JVM language that blends scripting ergonomics with Java interoperability; Groovy is used for build scripts, DSLs and rapid prototyping.",
    "Haskell": "1990. A purely functional language stressing strong static types and lazy evaluation; Haskell is prized for expressiveness and correctness in research and some production systems.",
    "Java": "1995. Java's portable bytecode and extensive libraries made it the backbone of enterprise applications and large-scale distributed systems for decades.",
    "JavaScript": "1995. Created for the browser, JavaScript evolved into a universal platform for web and server-side code; its flexibility enabled an enormous ecosystem.",
    "Julia": "2012. Built for numerical and scientific computing, Julia combines easy syntax with high-performance JIT-compiled code, reducing the need for separate prototyping and production languages.",
    "Kotlin": "2011. JetBrains developed Kotlin to modernize JVM development with concise syntax, null-safety and great Java interop, now a primary language for Android.",
    "Logo": "1967. Created to teach programming concepts through turtle graphics, Logo introduced learners to procedural thinking with immediate visual feedback.",
    "Lua": "1993. Lightweight, embeddable and fast, Lua is ubiquitous in game scripting and embedded contexts thanks to a tiny runtime and simple C API.",
    "M4": "1977. A general-purpose macro processor used for text generation and build-time code expansion; M4 powers many classic build tools and preprocessors.",
    "Nim": "2008. Nim offers Python-like syntax, powerful metaprogramming and C-level performance, aiming for expressive yet efficient system-level code.",
    "Objective-C": "1984. Combining C with Smalltalk-style messaging, Objective-C powered classic Apple development with a dynamic runtime and flexible object model.",
    "OCaml": "1996. A pragmatic functional language with strong typing and efficient native code generation, OCaml is used in compilers, tooling and domain-specific systems.",
    "Octave": "1988. An open numerical computation environment compatible with MATLAB, Octave is convenient for algorithm prototyping and academic work.",
    "Pascal": "1970. Niklaus Wirth designed Pascal to teach structured programming and data structuring; it influenced many later languages and educational curricula.",
    "Perl": "1987. Larry Wall's practical text-processing language excels at regex-driven scripting and rapid data munging; Perl was the web glue for many early projects.",
    "PHP": "1995. Initially built for web pages, PHP scaled into server-side frameworks and CMS platforms, powering a significant fraction of the web.",
    "PostScript": "1982. A page-description language that's also Turing-complete; PostScript shaped printing, vector graphics and document rendering workflows.",
    "PowerShell": "2006. Microsoft's object-oriented shell for task automation and configuration management, combining system tooling with structured pipeline objects.",
    "Prolog": "1972. A logic-programming paradigm where code expresses facts and rules; Prolog is well suited for symbolic reasoning, constraint solving and AI research.",
    "Python": "1991. Guido van Rossum designed Python for readability and productivity; with an enormous ecosystem it excels in scripting, data science, automation and web services.",
    "Red": "2011. A next-generation functional and symbolic language inspired by REBOL. It's designed to be a full-stack language, capable of everything from low-level systems programming to high-level GUI application development.",
    "R": "1993. A language and environment for statistical computing and visualization, R offers domain-specific tools for data analysis and reproducible research.",
    "Racket": "1995. A descendant of Scheme created for language-oriented programming, education and building new DSLs with powerful macro systems.",
    "Rexx": "1979. A readable scripting language historically used on mainframes for automation and text processing; Rexx emphasizes clarity and maintainability.",
    "Ruby": "1995. Designed for programmer happiness, Ruby's elegant syntax and metaprogramming made it the language behind rapid web development frameworks like Rails.",
    "Rust": "2010. Rust targets safe, concurrent systems programming with compile-time guarantees that prevent many classes of runtime errors while delivering native performance.",
    "Scala": "2004. Scala fuses object-oriented and functional programming on the JVM, enabling concise, type-safe code for large systems and data pipelines.",
    "Scheme": "1975. A minimalist Lisp dialect focusing on clean semantics and first-class procedures; Scheme is central in programming language education.",
    "Sed": "1974. A stream editor ideal for scripted, line-oriented text transformations; sed remains a compact tool in shell-based text processing.",
    "Smalltalk": "1972. Pioneering a pure object model and live programming environment, Smalltalk influenced GUIs, IDEs and modern object-oriented language design.",
    "SNOBOL": "1962. Early string-oriented languages for pattern matching, SNOBOL introduced expressive text processing concepts before modern regex engines.",
        "Swift": "2014. Apple's modern language focusing on safety, performance and developer ergonomics; Swift has largely superseded Objective-C for Apple platform development.",
    "Tcl": "1988. A simple, embeddable scripting language often paired with Tk for GUI applications; Tcl is valued for its ease of extension.",
    "TypeScript": "2012. Adds optional static typing and tooling to JavaScript to improve maintainability and catch errors early while compiling to standard JS.",
    "Vala": "2006. Offers modern language conveniences while compiling to C and targeting GObject, simplifying GNOME application development.",
    "Verilog": "1984. An HDL for modeling and simulating digital circuits; Verilog is essential in hardware design and synthesis flows.",
    "VHDL": "1980. A strongly-typed hardware description language used for rigorous modelling and verification of digital systems in industry.",
    "Vimscript": "1991. The scripting language of Vim enabling powerful editor automation, macros and plugin development for efficient text editing.",
    "VisualBasic": "1991. Made event-driven Windows application development accessible with RAD tools and a beginner-friendly syntax.",
    "WebAssembly": "2017. A compact binary format that runs in browsers and other hosts, enabling near-native performance for multiple languages.",
    "Wren": "2013. A small, fast, class-based concurrent scripting language. Designed by Bob Nystrom, Wren aims to be a modern, improved version of Lua, featuring a small footprint and high performance.",
    "Zig": "2016. A modern systems language focused on simplicity, explicit control and predictable performance, positioning itself as a pragmatic C alternative."
};

// --- Language Metadata (creator info, descriptions, etc.) ---
export interface LanguageMeta {
    creator: string;
    date: string;
    description: string;

    location?: string;
    benefits?: string;
    related?: string;
    history?: string;
    paradigm?: string;
    typeSystem?: string;
}

export const languageMetadata: Record<string, LanguageMeta> = {
    "Ada": {
        "creator": "Jean Ichbiah",
        "date": "1980",
        "description": "A statically typed, structured, imperative, and object-oriented high-level language, designed for safety-critical and real-time systems.",

        "location": "CII Honeywell Bull, France",
        "benefits": "Strong typing, explicit concurrency, and high reliability.",
        "related": "Pascal, Modula-2, VHDL, PL/SQL"
    },
    "Assembly": {
        "creator": "Kathleen Booth",
        "date": "1947",
        "description": "Low-level symbolic representation of machine code. As close to the metal as you can get. Programmers using assembly have fine-grained control over registers, memory layout and instruction scheduling — essential for firmware and tight performance hotspots.",

        "location": "Birkbeck College, London",
        "benefits": "Direct hardware control, maximum performance, zero overhead.",
        "related": "Machine Code, C, Forth"
    ,
        "history": "1949. Born from the earliest machines, assembly languages provide a symbolic layer over raw machine code. Programmers using assembly have fine-grained control over registers, memory layout and instruction scheduling — essential for firmware and tight performance hotspots."
    },
    "Awk": {
        "creator": "Aho, Weinberger, Kernighan",
        "date": "1977",
        "description": "A concise domain-specific language designed for text processing and typically used as a data extraction and reporting tool. Awk remains ideal for quick data extraction and on-the-fly reporting in shell pipelines.",

        "location": "Bell Labs, USA",
        "benefits": "Excellent for text processing, one-liners, and data extraction.",

        "related": "C, SNOBOL, Shell, Perl, Lua"
    ,
        "history": "1977. Created by Aho, Weinberger and Kernighan at Bell Labs as a concise domain language for text processing. Awk remains ideal for quick data extraction and on-the-fly reporting in shell pipelines."
    },
    "Bash": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "The Bourne Again SHell. The default shell for most Linux distributions and macOS. It unified scripting and interactive use on Unix-like systems. Beyond interactive shells, Bash scripts glue together tools, automate builds and manage system tasks.",

        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction.",
        "related": "Bourne Shell, C Shell, KornShell, Zsh"
    ,
        "history": "1989. Brian Fox's Bourne Again SHell unified scripting and interactive use on Unix-like systems. Beyond interactive shells, Bash scripts glue together tools, automate builds and manage system tasks."
    },
    "Basic": {
        "creator": "John G. Kemeny, Thomas E. Kurtz",
        "date": "1964",
        "description": "Beginner's All-purpose Symbolic Instruction Code. Designed to emphasize ease of use. Early implementations ran on the Dartmouth Time-Sharing System and emphasized simple, interactive use (PRINT, LET, GOTO, line numbers). BASIC exploded in popularity on microcomputers in the 1970s and 1980s via many dialects.",

        "location": "Dartmouth College, USA",
        "benefits": "Easy to learn, interactive, historical significance.",
        "related": "Fortran, JOSS, Visual Basic, COMAL"
    ,
        "history": "1964. Beginner's All-purpose Symbolic Instruction Code (BASIC) was created by John Kemeny and Thomas Kurtz at Dartmouth College to give students easy access to computing. Early implementations ran on the Dartmouth Time-Sharing System and emphasized simple, interactive use (PRINT, LET, GOTO, line numbers). BASIC exploded in popularity on microcomputers in the 1970s and 1980s via many dialects (and influential ports such as Altair BASIC), helping introduce programming to hobbyists and a generation of developers."
    },
    "C": {
        "creator": "Dennis Ritchie",
        "date": "1972",
        "description": "General-purpose systems programming language. Excels in operating systems, embedded systems, and performance-critical applications where direct hardware access is essential. Its influence is evident in modern compilers, runtimes and standards.",

        "location": "Bell Labs, USA",
        "benefits": "High performance, portability, low-level access, massive legacy.",
        "related": "B, BCPL, Algol 68, C++, C#, Java, Objective-C, Go, Rust, Zig",
        "paradigm": "Imperative, Structured",
        "typeSystem": "Static, Weak",
        "history": "1972. Dennis Ritchie's C balanced low-level access with structured programming and portability, shaping decades of systems software. Its influence is evident in modern compilers, runtimes and standards."
    },
    "C++": {
        "creator": "Bjarne Stroustrup",
        "date": "1985",
        "description": "Multi-paradigm language combining procedural and object-oriented programming. It extended C with abstractions like classes and templates to enable both low-level control and high-level design. C++ powers performance-critical applications, from game engines to embedded systems.",

        "location": "Bell Labs, USA",
        "benefits": "Performance, object-oriented, rich ecosystem, hardware control.",
        "related": "C, Simula, Ada, Rust, D, Java, C#"
    ,
        "history": "1985. Bjarne Stroustrup extended C with abstractions like classes and templates to enable both low-level control and high-level design. C++ powers performance-critical applications, from game engines to embedded systems."
    },
    "C_Sharp": {
        "creator": "Anders Hejlsberg",
        "date": "2000",
        "description": "A modern, object-oriented, and type-safe programming language derived from C and C++. Designed to support enterprise, desktop and web applications on the .NET platform. It emphasizes tooling, libraries and developer productivity.",

        "location": "Microsoft, USA",
        "benefits": "Strong typing, rich .NET ecosystem, modern features, tooling.",
        "related": "C++, Java, Delphi, Modula-3, F#, Visual Basic .NET"
    ,
        "history": "2000. Designed by Anders Hejlsberg, C# blends modern language features with the .NET runtime to support enterprise, desktop and web applications. It emphasizes tooling, libraries and developer productivity."
    },
    "Clojure": {
        "creator": "Rich Hickey",
        "date": "2007",
        "description": "A dynamic, general-purpose programming language, combining the approachability and interactive development of a scripting language with an efficient and robust infrastructure for multithreaded programming. It brings Lisp's code-as-data and immutable data structures to the JVM.",

        "location": "USA",
        "benefits": "Immutability, functional programming, JVM interoperability, simplicity.",
        "related": "Lisp, Java, Haskell, Scheme, Racket"
    ,
        "history": "2007. Rich Hickey's Clojure brings Lisp's code-as-data and immutable data structures to the JVM, focusing on simplicity, concurrency and functional programming. It's used for building robust, composable systems."
    },
    "Cobol": {
        "creator": "CODASYL Committee (Grace Hopper)",
        "date": "1959",
        "description": "Common Business-Oriented Language. Designed for business use. COBOL's verbose, English-like syntax made it accessible to non-academic programmers and it still runs critical financial systems today.",

        "location": "USA",
        "benefits": "Business data processing, stability, massive legacy codebases.",
        "related": "FLOW-MATIC, COMTRAN, PL/I"
    ,
        "history": "1959. Created for business data processing, COBOL's verbose, English-like syntax made it accessible to non-academic programmers and it still runs critical financial systems today."
    },
    "CoffeeScript": {
        "creator": "Jeremy Ashkenas",
        "date": "2009",
        "description": "A programming language that compiles to JavaScript. It adds syntactic sugar inspired by Ruby, Python and Haskell. CoffeeScript smoothed the migration to more expressive JavaScript patterns.",

        "location": "USA",
        "benefits": "Concise syntax, readability, compiles to clean JavaScript.",
        "related": "JavaScript, Ruby, Python, Haskell, TypeScript"
    ,
        "history": "2009. A syntactic layer over JavaScript that introduced concise idioms and inspired later JS syntax improvements. CoffeeScript smoothed the migration to more expressive JavaScript patterns."
    },
    "CommonLisp": {
        "creator": "Committee (Guy L. Steele Jr. et al.)",
        "date": "1984",
        "description": "A standardized, general-purpose Lisp dialect with powerful macros and dynamic runtime features. Common Lisp supports rapid prototyping and domain-specific language creation.",

        "location": "USA",
        "benefits": "Macros, dynamic typing, interactive development, flexibility.",
        "related": "Lisp, Scheme, MacLisp, Interlisp, Clojure"
    ,
        "history": "1984. A standardized Lisp dialect with powerful macros and dynamic runtime features, Common Lisp supports rapid prototyping and domain-specific language creation."
    },
    "Crystal": {
        "creator": "Ary Borenszweig",
        "date": "2014",
        "description": "A general-purpose, object-oriented programming language, designed and implemented by Ary Borenszweig, Juan Wajnerman, Brian Cardiff and more than 300 contributors. Crystal aims to deliver Ruby-like syntax with static typing and native performance.",

        "location": "Argentina",
        "benefits": "Ruby-like syntax, C-like performance, static typing.",
        "related": "Ruby, C, Go, Rust, C#"
    ,
        "history": "2014. Crystal aims to deliver Ruby-like syntax with static typing and native performance, targeting developers who want expressive code without sacrificing speed."
    },
    "D": {
        "creator": "Walter Bright",
        "date": "2001",
        "description": "A general-purpose system programming language with a C-like syntax that compiles to native code. D modernizes systems programming by adding safety and productivity features while keeping C-like performance.",

        "location": "Digital Mars, USA",
        "benefits": "System programming, performance, safety, metaprogramming.",
        "related": "C, C++, Java, C#, Python, Ruby"
    ,
        "history": "2001. Walter Bright's D modernizes systems programming by adding safety and productivity features while keeping C-like performance. It targets high-performance, maintainable code."
    },
    "Dart": {
        "creator": "Lars Bak, Kasper Lund",
        "date": "2011",
        "description": "A client-optimized language for fast apps on any platform. Created for structured client-side development, Dart powers Flutter for cross-platform UIs and compiles to efficient native or JS code.",

        "location": "Google, Denmark/USA",
        "benefits": "UI optimized, fast compilation, cross-platform (Flutter).",
        "related": "Java, C#, JavaScript, TypeScript, Smalltalk"
    ,
        "history": "2011. Created for structured client-side development, Dart powers Flutter for cross-platform UIs and compiles to efficient native or JS code. Its toolchain focuses on developer productivity."
    },
    "Elixir": {
        "creator": "José Valim",
        "date": "2011",
        "description": "A dynamic, functional language designed for building scalable and maintainable applications. Built on the Erlang VM, Elixir blends fault-tolerant concurrency with elegant syntax and tooling.",

        "location": "Plataformatec, Brazil",
        "benefits": "Concurrency, fault tolerance, functional, Ruby-like syntax.",
        "related": "Erlang, Ruby, Clojure, LFE"
    ,
        "history": "2011. Built on the Erlang VM by José Valim, Elixir blends fault-tolerant concurrency with elegant syntax and tooling, favored for scalable distributed services."
    },
    "Erlang": {
        "creator": "Joe Armstrong",
        "date": "1986",
        "description": "A general-purpose, concurrent, functional programming language, and a garbage-collected runtime system. Designed for telecoms, it emphasizes lightweight processes, message-passing concurrency and robust fault-recovery.",

        "location": "Ericsson, Sweden",
        "benefits": "Massive concurrency, fault tolerance, hot code swapping.",
        "related": "Prolog, Smalltalk, Elixir, LFE, Rust"
    ,
        "history": "1986. Designed for telecoms, Erlang emphasizes lightweight processes, message-passing concurrency and robust fault-recovery. It's a foundation for resilient distributed systems."
    },
    "F_Sharp": {
        "creator": "Don Syme",
        "date": "2005",
        "description": "A functional-first programming language that encompasses functional, imperative, and object-oriented programming methods. F# brings functional-first programming to .NET with strong typing, succinct syntax and excellent interop.",

        "location": "Microsoft Research, UK",
        "benefits": "Functional-first, .NET integration, type inference, concise.",
        "related": "OCaml, C#, Haskell, Scala, Python"
    ,
        "history": "2005. F# brings functional-first programming to .NET with strong typing, succinct syntax and excellent interop, used in finance, analytics and domain modelling."
    },
    "Fortran": {
        "creator": "John Backus",
        "date": "1957",
        "description": "The first high-level programming language, designed for numeric computation and scientific computing. It is a third-generation, compiled, imperative programming language, still used today for scientific and mathematical applications.",

        "location": "IBM, USA",
        "benefits": "Numerical computation, scientific computing, performance.",
        "related": "ALGOL, BASIC, PL/I, C, Julia, MATLAB"
    ,
        "history": "1957. One of the first high-level languages, Fortran was built for numerical computation and scientific programming; optimized compilers and legacy code keep it relevant in HPC."
    },
    "Go": {
        "creator": "Robert Griesemer, Rob Pike, Ken Thompson",
        "date": "2009",
        "description": "Compiled, statically-typed language with built-in concurrency. Designed at Google for simplicity, fast compilation and pragmatic concurrency, Go is a popular choice for cloud services, networking and developer tools.",

        "location": "Google, USA",
        "benefits": "Simplicity, concurrency, fast compilation, static typing.",
        "related": "C, Pascal, Oberon, Limbo, CSP, Python"
    ,
        "history": "2009. Designed at Google for simplicity, fast compilation and pragmatic concurrency, Go is a popular choice for cloud services, networking and developer tools."
    },
    "Groovy": {
        "creator": "James Strachan",
        "date": "2003",
        "description": "A Java-syntax-compatible object-oriented programming language for the Java platform. It is a dynamic language with features similar to Python, Ruby, and Smalltalk, used for build scripts, DSLs and rapid prototyping.",

        "location": "USA",
        "benefits": "Java compatibility, scripting, dynamic features, DSLs.",
        "related": "Java, Python, Ruby, Smalltalk"
    ,
        "history": "2003. A dynamic JVM language that blends scripting ergonomics with Java interoperability; Groovy is used for build scripts, DSLs and rapid prototyping."
    },
    "Haskell": {
        "creator": "Committee (Simon Peyton Jones et al.)",
        "date": "1990",
        "description": "A standardized, general-purpose, purely functional programming language with non-strict semantics and strong static typing. Haskell is prized for expressiveness and correctness in research and some production systems.",

        "location": "Global",
        "benefits": "Pure functional, type safety, lazy evaluation, concurrency.",
        "related": "Miranda, ML, Clean, Agda, Purescript, Elm, Rust"
    ,
        "history": "1990. A purely functional language stressing strong static types and lazy evaluation; Haskell is prized for expressiveness and correctness in research and some production systems."
    },
    "Java": {
        "creator": "James Gosling",
        "date": "1995",
        "description": "Platform-independent, object-oriented language. Its portable bytecode and extensive libraries made it the backbone of enterprise applications and large-scale distributed systems for decades.",

        "location": "Sun Microsystems, USA",
        "benefits": "Platform independence, vast ecosystem, enterprise-grade.",
        "related": "C++, Smalltalk, Objective-C, C#, Kotlin, Scala",
        "paradigm": "Object-oriented, Class-based, Imperative",
        "typeSystem": "Static, Strong",
        "history": "1995. Java's portable bytecode and extensive libraries made it the backbone of enterprise applications and large-scale distributed systems for decades."
    },
    "JavaScript": {
        "creator": "Brendan Eich",
        "date": "1995",
        "description": "High-level, dynamic, untyped, interpreted runtime. Created for the browser, JavaScript evolved into a universal platform for web and server-side code; its flexibility enabled an enormous ecosystem.",

        "location": "Netscape, USA",
        "benefits": "Ubiquitous, versatile, huge ecosystem, async capabilities.",
        "related": "Self, Scheme, Java, ECMAScript, TypeScript",
        "paradigm": "Multi-paradigm, Event-driven, Functional, Imperative, Prototype-based",
        "typeSystem": "Dynamic, Weak",
        "history": "1995. Created for the browser, JavaScript evolved into a universal platform for web and server-side code; its flexibility enabled an enormous ecosystem."
    },
    "Julia": {
        "creator": "Jeff Bezanson, Stefan Karpinski, Viral B. Shah, Alan Edelman",
        "date": "2012",
        "description": "A high-level, high-performance dynamic programming language for technical computing. It combines easy syntax with high-performance JIT-compiled code, reducing the need for separate prototyping and production languages.",

        "location": "MIT, USA",
        "benefits": "Speed of C, ease of Python, great for scientific computing.",
        "related": "MATLAB, Python, R, Lisp, Fortran"
    ,
        "history": "2012. Built for numerical and scientific computing, Julia combines easy syntax with high-performance JIT-compiled code, reducing the need for separate prototyping and production languages."
    },
    "Kotlin": {
        "creator": "JetBrains",
        "date": "2011",
        "description": "A cross-platform, statically typed, general-purpose programming language with type inference. JetBrains developed Kotlin to modernize JVM development with concise syntax, null-safety and great Java interop.",

        "location": "JetBrains, Czech Republic",
        "benefits": "Concise, null-safe, great Java interop, Android official.",
        "related": "Java, Scala, C#, Groovy, Swift"
    ,
        "history": "2011. JetBrains developed Kotlin to modernize JVM development with concise syntax, null-safety and great Java interop, now a primary language for Android."
    },
    "Lua": {
        "creator": "Roberto Ierusalimschy, Luiz Henrique de Figueiredo, Waldemar Celes",
        "date": "1993",
        "description": "A powerful, efficient, lightweight, embeddable scripting language. It is designed for embedded use in applications and is ubiquitous in game engines.",

        "location": "PUC-Rio, Brazil",
        "benefits": "Lightweight, fast, embeddable, simple syntax.",
        "related": "Scheme, SNOBOL, C, Python, JavaScript"
    ,
        "history": "1993. Lightweight, embeddable and fast, Lua is ubiquitous in game scripting and embedded contexts thanks to a tiny runtime and simple C API."
    },
    "Nim": {
        "creator": "Andreas Rumpf",
        "date": "2008",
        "description": "A statically typed compiled systems programming language. It combines successful concepts from mature languages like Python, Ada and Modula. Nim offers Python-like syntax, powerful metaprogramming and C-level performance.",

        "location": "Germany",
        "benefits": "Python-like syntax, C-like performance, metaprogramming.",
        "related": "Python, Ada, Modula-3, Lisp, C"
    ,
        "history": "2008. Nim offers Python-like syntax, powerful metaprogramming and C-level performance, aiming for expressive yet efficient system-level code."
    },
    "OCaml": {
        "creator": "Xavier Leroy et al.",
        "date": "1996",
        "description": "A general-purpose, multi-paradigm programming language which extends the Caml dialect of ML with object-oriented features. OCaml is used in compilers, tooling and domain-specific systems.",

        "location": "INRIA, France",
        "benefits": "Strong typing, functional, efficient native code.",
        "related": "ML, Standard ML, Haskell, F#, Rust"
    ,
        "history": "1996. A pragmatic functional language with strong typing and efficient native code generation, OCaml is used in compilers, tooling and domain-specific systems."
    },
    "Octave": {
        "creator": "John W. Eaton",
        "date": "1988",
        "description": "A high-level language, primarily intended for numerical computations. An open numerical computation environment compatible with MATLAB, Octave is convenient for algorithm prototyping and academic work.",

        "location": "University of Wisconsin-Madison, USA",
        "benefits": "MATLAB compatible, free, numerical computing.",
        "related": "MATLAB, Julia, Python (NumPy), R"
    ,
        "history": "1988. An open numerical computation environment compatible with MATLAB, Octave is convenient for algorithm prototyping and academic work."
    },
    "Pascal": {
        "creator": "Niklaus Wirth",
        "date": "1970",
        "description": "An imperative and procedural programming language, designed as a small, efficient language intended to encourage good programming practices. Niklaus Wirth designed Pascal to teach structured programming and data structuring.",

        "location": "ETH Zurich, Switzerland",
        "benefits": "Structured programming, teaching, strong typing.",
        "related": "ALGOL, Modula-2, Oberon, Ada, Delphi"
    ,
        "history": "1970. Niklaus Wirth designed Pascal to teach structured programming and data structuring; it influenced many later languages and educational curricula."
    },
    "Perl": {
        "creator": "Larry Wall",
        "date": "1987",
        "description": "A high-level, general-purpose, interpreted, dynamic programming language. It excels at regex-driven scripting and rapid data munging; Perl was the web glue for many early projects.",

        "location": "Unisys, USA",
        "benefits": "Text processing, regex, CPAN, practical.",
        "related": "AWK, Sed, C, Shell, Raku, Python, Ruby"
    ,
        "history": "1987. Larry Wall's practical text-processing language excels at regex-driven scripting and rapid data munging; Perl was the web glue for many early projects."
    },
    "PHP": {
        "creator": "Rasmus Lerdorf",
        "date": "1995",
        "description": "A general-purpose scripting language geared towards web development. Initially built for web pages, PHP scaled into server-side frameworks and CMS platforms, powering a significant fraction of the web.",

        "location": "Denmark/Canada",
        "benefits": "Web development, vast ecosystem (WordPress, Laravel).",
        "related": "Perl, C, Java, JavaScript, Python"
    ,
        "history": "1995. Initially built for web pages, PHP scaled into server-side frameworks and CMS platforms, powering a significant fraction of the web."
    },
    "Prolog": {
        "creator": "Alain Colmerauer, Robert Kowalski",
        "date": "1972",
        "description": "A logic programming language associated with artificial intelligence and computational linguistics. It is well suited for symbolic reasoning, constraint solving and AI research.",

        "location": "University of Aix-Marseille and University of Edinburgh",
        "benefits": "Logic programming, AI, pattern matching, declarative.",
        "related": "Lisp, SQL, Datalog, Mercury, Erlang"
    ,
        "history": "1972. A logic-programming paradigm where code expresses facts and rules; Prolog is well suited for symbolic reasoning, constraint solving and AI research."
    },
    "Python": {
        "creator": "Guido van Rossum",
        "date": "1991",
        "description": "A high-level, interpreted, general-purpose programming language emphasizing readability. With an enormous ecosystem it excels in scripting, data science, automation and web services.",

        "location": "CWI, Netherlands",
        "benefits": "Readability, versatility, massive ecosystem, beginner-friendly.",
        "related": "ABC, C, Haskell, Lisp, Perl, Ruby, Java",
        "paradigm": "Multi-paradigm, Functional, Imperative, Object-oriented, Structured",
        "typeSystem": "Dynamic, Strong",
        "history": "1991. Guido van Rossum designed Python for readability and productivity; with an enormous ecosystem it excels in scripting, data science, automation and web services."
    },
    "R": {
        "creator": "Ross Ihaka, Robert Gentleman",
        "date": "1993",
        "description": "A language and environment for statistical computing and graphics. R offers domain-specific tools for data analysis and reproducible research.",

        "location": "University of Auckland, New Zealand",
        "benefits": "Statistical analysis, data visualization, CRAN packages.",
        "related": "S, Scheme, Lisp, Python (Pandas), Julia"
    ,
        "history": "1993. A language and environment for statistical computing and visualization, R offers domain-specific tools for data analysis and reproducible research."
    },
    "Racket": {
        "creator": "PLT Inc. (Matthew Flatt et al.)",
        "date": "1995",
        "description": "A general-purpose, multi-paradigm programming language based on the Scheme branch of Lisp. A descendant of Scheme created for language-oriented programming, education and building new DSLs.",

        "location": "Northeastern University, USA",
        "benefits": "Language-oriented programming, education, DSLs, macros.",
        "related": "Scheme, Lisp, Clojure, Haskell"
    ,
        "history": "1995. A descendant of Scheme created for language-oriented programming, education and building new DSLs with powerful macro systems."
    },
    "Ruby": {
        "creator": "Yukihiro Matsumoto",
        "date": "1995",
        "description": "A dynamic, open source programming language with a focus on simplicity and productivity. Designed for programmer happiness, Ruby's elegant syntax and metaprogramming made it the language behind rapid web development frameworks like Rails.",

        "location": "Japan",
        "benefits": "Developer happiness, metaprogramming, Rails ecosystem.",
        "related": "Perl, Smalltalk, Eiffel, Ada, Lisp, Python"
    ,
        "history": "1995. Designed for programmer happiness, Ruby's elegant syntax and metaprogramming made it the language behind rapid web development frameworks like Rails."
    },
    "Rust": {
        "creator": "Graydon Hoare",
        "date": "2010",
        "description": "A multi-paradigm, general-purpose programming language designed for safety and performance. It targets safe, concurrent systems programming with compile-time guarantees that prevent many classes of runtime errors.",

        "location": "Mozilla, USA",
        "benefits": "Memory safety, concurrency, performance, no GC.",
        "related": "C++, ML, Haskell, Erlang, Swift, C",
        "paradigm": "Multi-paradigm, Concurrent, Functional, Imperative, Structured",
        "typeSystem": "Static, Strong, Safe",
        "history": "2010. Rust targets safe, concurrent systems programming with compile-time guarantees that prevent many classes of runtime errors while delivering native performance."
    },
    "Scala": {
        "creator": "Martin Odersky",
        "date": "2004",
        "description": "A strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming. Scala fuses object-oriented and functional programming on the JVM.",

        "location": "EPFL, Switzerland",
        "benefits": "Functional + OOP, JVM, type safety, concurrency (Akka).",
        "related": "Java, ML, Haskell, Smalltalk, Erlang, Kotlin"
    ,
        "history": "2004. Scala fuses object-oriented and functional programming on the JVM, enabling concise, type-safe code for large systems and data pipelines."
    },
    "Scheme": {
        "creator": "Guy L. Steele Jr., Gerald Jay Sussman",
        "date": "1975",
        "description": "A minimalist dialect of the Lisp family of programming languages. Scheme is central in programming language education, focusing on clean semantics and first-class procedures.",

        "location": "MIT AI Lab, USA",
        "benefits": "Minimalist, powerful macros, education, standards (R5RS, R7RS).",
        "related": "Lisp, Common Lisp, Racket, Clojure, JavaScript"
    ,
        "history": "1975. A minimalist Lisp dialect focusing on clean semantics and first-class procedures; Scheme is central in programming language education."
    },
    "Swift": {
        "creator": "Chris Lattner et al.",
        "date": "2014",
        "description": "A general-purpose, multi-paradigm, compiled programming language developed by Apple. Swift has largely superseded Objective-C for Apple platform development, focusing on safety, performance and developer ergonomics.",

        "location": "Apple Inc., USA",
        "benefits": "Safety, performance, modern syntax, Apple ecosystem.",
        "related": "Objective-C, Rust, C#, Python, Ruby"
    ,
        "history": "2014. Apple's modern language focusing on safety, performance and developer ergonomics; Swift has largely superseded Objective-C for Apple platform development."
    },
    "Tcl": {
        "creator": "John Ousterhout",
        "date": "1988",
        "description": "A high-level, general-purpose, interpreted, dynamic programming language. A simple, embeddable scripting language often paired with Tk for GUI applications; Tcl is valued for its ease of extension.",

        "location": "UC Berkeley, USA",
        "benefits": "Scripting, GUI (Tk), embedding, simplicity.",
        "related": "Lisp, Shell, Python, Perl"
    ,
        "history": "1988. A simple, embeddable scripting language often paired with Tk for GUI applications; Tcl is valued for its ease of extension."
    },
    "TypeScript": {
        "creator": "Anders Hejlsberg (Microsoft)",
        "date": "2012",
        "description": "A strict syntactical superset of JavaScript that adds optional static typing. It improves maintainability and catches errors early while compiling to standard JS.",

        "location": "Microsoft, USA",
        "benefits": "Type safety, IDE support, scales to large codebases.",
        "related": "JavaScript, C#, Java, CoffeeScript"
    ,
        "history": "2012. Adds optional static typing and tooling to JavaScript to improve maintainability and catch errors early while compiling to standard JS."
    },
    "Vala": {
        "creator": "Jürg Billeter",
        "date": "2006",
        "description": "An object-oriented programming language with a self-hosting compiler that generates C code and uses the GObject system. Offers modern language conveniences while compiling to C, simplifying GNOME application development.",

        "location": "GNOME Project",
        "benefits": "GObject integration, C performance, modern syntax.",
        "related": "C, C#, Java, GObject"
    ,
        "history": "2006. Offers modern language conveniences while compiling to C and targeting GObject, simplifying GNOME application development."
    },
    "VisualBasic": {
        "creator": "Microsoft (Alan Cooper)",
        "date": "1991",
        "description": "A third-generation event-driven programming language from Microsoft for its Component Object Model programming model. Made event-driven Windows application development accessible with RAD tools.",

        "location": "Microsoft, USA",
        "benefits": "RAD, ease of use, Windows integration, legacy support.",
        "related": "BASIC, QuickBASIC, VBA, C#"
    ,
        "history": "1991. Made event-driven Windows application development accessible with RAD tools and a beginner-friendly syntax."
    },
    "Zig": {
        "creator": "Andrew Kelley",
        "date": "2016",
        "description": "A general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software. It aims to improve upon C with features like comptime and manual memory management without hidden control flow.",

        "location": "USA",
        "benefits": "No hidden control flow, manual memory management, comptime.",
        "related": "C, C++, Rust, Go, Jai"
    ,
        "history": "2016. A modern systems language focused on simplicity, explicit control and predictable performance, positioning itself as a pragmatic C alternative."
    },
    "BASH": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "The Bourne Again SHell. The default shell for most Linux distributions and macOS.",

        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction.",
        "related": "Bourne Shell, Zsh, Ksh"
    },
    "BASIC": {
        "creator": "John G. Kemeny, Thomas E. Kurtz",
        "date": "1964",
        "description": "Beginner's All-purpose Symbolic Instruction Code. Designed to emphasize ease of use.",

        "location": "Dartmouth College, USA",
        "benefits": "Easy to learn, interactive, historical significance.",
        "related": "Fortran, Visual Basic"
    },
    "Wren": {
        "creator": "Bob Nystrom",
        "date": "2013",
        "description": "A small, fast, class-based concurrent scripting language. Designed for embedding in applications, it is object-oriented and comparable in speed to Lua.",

        "location": "USA",
        "benefits": "High performance, small footprint, clean syntax.",
        "related": "Lua, Smalltalk, Dart, JavaScript"
    ,
        "history": "2013. A small, fast, class-based concurrent scripting language. Designed by Bob Nystrom, Wren aims to be a modern, improved version of Lua, featuring a small footprint and high performance."
    },
    "Red": {
        "creator": "Nenad Rakocevic",
        "date": "2011",
        "description": "A functional and symbolic language inspired by REBOL. It is a full-stack language, capable of both high-level scripting and low-level system programming.",

        "location": "Global",
        "benefits": "Full-stack, symbolic, cross-platform.",
        "related": "Rebol, Lisp, Lua"
    ,
        "history": "2011. A next-generation functional and symbolic language inspired by REBOL. It's designed to be a full-stack language, capable of everything from low-level systems programming to high-level GUI application development."
    },
    "Pike": {
        "creator": "Fredrik Hübinette, Per Hedbor",
        "date": "1994",
        "description": "A general-purpose, high-level, cross-platform, dynamic programming language with a C-like syntax. It features garbage collection, advanced data types, and first-class anonymous functions.",

        "location": "Linköping, Sweden",
        "benefits": "C-like syntax, dynamic typing, concurrency.",
        "related": "LPC, C, C++, Python"
    },
    "Fennel": {
        "creator": "Calvin Rose (Project Lead)",
        "date": "2016",
        "description": "A programming language that brings Lisp syntax and macros to the Lua architecture. It compiles to Lua and runs with zero overhead, offering full Lua compatibility.",

        "location": "Global",
        "benefits": "Lisp syntax, Lua speed, zero overhead, embeddable.",
        "related": "Lua, Lisp, Clojure"
    },
    "Janet": {
        "creator": "Calvin Rose",
        "date": "2017",
        "description": "A functional and imperative programming language designed for system scripting, expressive automation, and embedding. It features a built-in PEG engine and Lisp-like macros.",

        "location": "USA",
        "benefits": "Embeddable, PEG parsing, structural editing.",
        "related": "Lua, Clojure, Lisp, C"
    },
    "Io": {
        "creator": "Steve Dekorte",
        "date": "2002",
        "description": "A pure object-oriented, prototype-based programming language. In Io, everything is an object, and it uses a message-passing model similar to Smalltalk but with a prototype-based object model like Self.",

        "location": "USA",
        "benefits": "Simplicity, flexibility, concurrency (coroutines).",
        "related": "Smalltalk, Self, Lisp, Lua"
    },
    "Factor": {
        "creator": "Slava Pestov",
        "date": "2003",
        "description": "A stack-oriented, concatenative, multi-paradigm programming language. It combines the flexibility of dynamic typing with the performance of native compilation.",

        "location": "USA",
        "benefits": "Concatenative, metaprogramming, performance.",
        "related": "Forth, Joy, Lisp, Smalltalk"
    },
    "Icon": {
        "creator": "Ralph Griswold",
        "date": "1977",
        "description": "A very high-level programming language based on goal-directed execution and string scanning. It makes extensive use of generators and backtracking.",

        "location": "University of Arizona, USA",
        "benefits": "String processing, goal-directed execution, generators.",
        "related": "SNOBOL, Python, Unicon"
    },
    "Objective-C": {
        "creator": "Brad Cox, Tom Love",
        "date": "1984",
        "description": "A general-purpose, object-oriented programming language that adds Smalltalk-style messaging to C. It was the standard language for developing macOS and iOS applications until Swift's introduction.",

        "location": "Stepstone, USA",
        "benefits": "Dynamic runtime, C compatibility, Apple legacy.",
        "related": "C, Smalltalk, Swift, C++"
    ,
        "history": "1984. Combining C with Smalltalk-style messaging, Objective-C powered classic Apple development with a dynamic runtime and flexible object model."
    },
    "Raku": {
        "creator": "Larry Wall",
        "date": "2015",
        "description": "A member of the Perl family. Expressive, multi-paradigm, and versatile. A dynamic, high-level, general-purpose language with a strong emphasis on human-readable syntax and advanced text processing.",

        "location": "Global",
        "benefits": "Expressiveness, grammars, concurrency.",
        "related": "Perl, Haskell, Python, Ruby"
    },
    "V": {
        "creator": "Alexander Medvednikov",
        "date": "2019",
        "description": "A statically typed, compiled language designed for maintainability and speed. It aims to be simple, fast, and safe, compiling to C.",

        "location": "Global",
        "benefits": "Fast compilation, simplicity, safety.",
        "related": "Go, Oberon, Rust, Swift, C"
    }
};
