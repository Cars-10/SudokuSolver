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

    // Structured author information (array of author objects)
    authors?: Array<{ name: string; image?: string; website?: string }>;
    // Primary language logo/image
    image?: string;
    // Official language website
    website?: string;
}

export const languageMetadata: Record<string, LanguageMeta> = {
    "Ada": {
        "creator": "Jean Ichbiah",
        "date": "1980",
        "description": "Ada is a structured, statically typed, imperative, and object-oriented high-level programming language, inspired by Pascal and other languages. It has built-in language support for design by contract (DbC), extremely strong typing, explicit concurrency, tasks, synchronous message passing, protected objects, and non-determinism. Ada improves code safety and maintainability by using the compiler to find errors in favor of runtime errors.",
        "location": "CII Honeywell Bull, France",
        "benefits": "Strong typing, explicit concurrency, and high reliability.",
        "related": "Pascal, Modula-2, VHDL, PL/SQL",
        "authors": [
            { "name": "Jean Ichbiah" }
        ],
        "website": "https://www.adaic.org/"
    },
    "Assembly": {
        "creator": "Kathleen Booth",
        "date": "1947",
        "description": "In computing, assembly language (alternatively assembler language or symbolic machine code), often referred to simply as assembly and commonly abbreviated as ASM or asm, is any low-level programming language with a very strong correspondence between the instructions in the language and the architecture's machine code instructions. Assembly language usually has one statement per machine code instruction (1:1), but  constants, comments, assembler directives, symbolic labels of, e.g., memory locations, registers, and macros are generally also supported.\nThe first assembly code in which a language is used to represent machine code instructions is found in Kathleen and Andrew Donald Booth's 1947 work, Coding for A.R.C..",
        "location": "Birkbeck College, London",
        "benefits": "Direct hardware control, maximum performance, zero overhead.",
        "related": "Machine Code, C, Forth",
        "history": "1949. Born from the earliest machines, assembly languages provide a symbolic layer over raw machine code. Programmers using assembly have fine-grained control over registers, memory layout and instruction scheduling — essential for firmware and tight performance hotspots.",
        "authors": [
            { "name": "Kathleen Booth" }
        ]
    },
    "Awk": {
        "creator": "Aho, Weinberger, Kernighan",
        "date": "1977",
        "description": "AWK () is a scripting language designed for text processing and typically used as a data extraction and reporting tool. Like sed and grep, it is a filter, and it is a standard feature of most Unix-like operating systems. The shell command that runs the AWK processor is named awk.",
        "location": "Bell Labs, USA",
        "benefits": "Excellent for text processing, one-liners, and data extraction.",
        "related": "C, SNOBOL, Shell, Perl, Lua",
        "history": "1977. Created by Aho, Weinberger and Kernighan at Bell Labs as a concise domain language for text processing. Awk remains ideal for quick data extraction and on-the-fly reporting in shell pipelines.",
        "authors": [
            { "name": "Alfred Aho" },
            { "name": "Peter Weinberger" },
            { "name": "Brian Kernighan" }
        ]
    },
    "Bash": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "Bash (short for \"Bourne Again SHell\") is an interactive command interpreter and scripting language developed for Unix-like operating systems. Created in 1989 by Brian Fox for the GNU Project, it is designed as a completely free software alternative for the Bourne shell, sh, and other proprietary Unix shells, supported by the Free Software Foundation. Having gained widespread adoption, Bash is commonly used as the default login shell for numerous Linux distributions.",
        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction.",
        "related": "Bourne Shell, C Shell, KornShell, Zsh",
        "history": "1989. Brian Fox's Bourne Again SHell unified scripting and interactive use on Unix-like systems. Beyond interactive shells, Bash scripts glue together tools, automate builds and manage system tasks.",
        "authors": [
            { "name": "Brian Fox" }
        ],
        "website": "https://www.gnu.org/software/bash/"
    },
    "C": {
        "creator": "Dennis Ritchie",
        "date": "1972",
        "description": "C is a general-purpose programming language. It was created in the 1970s by Dennis Ritchie and remains widely used and influential. By design, C gives the programmer relatively direct access to the features of the typical CPU architecture, customized for the target instruction set.",
        "location": "Bell Labs, USA",
        "benefits": "High performance, portability, low-level access, massive legacy.",
        "related": "B, BCPL, Algol 68, C++, C#, Java, Objective-C, Go, Rust, Zig",
        "paradigm": "Imperative, Structured",
        "typeSystem": "Static, Weak",
        "history": "1972. Dennis Ritchie's C balanced low-level access with structured programming and portability, shaping decades of systems software. Its influence is evident in modern compilers, runtimes and standards.",
        "authors": [
            { "name": "Dennis Ritchie" }
        ],
        "website": "https://www.iso.org/standard/74528.html"
    },
    "C++": {
        "creator": "Bjarne Stroustrup",
        "date": "1985",
        "description": "C++ is a high-level, general-purpose programming language created by Danish computer scientist Bjarne Stroustrup. First released in 1985 as an extension of the C programming language, adding object-oriented (OOP) features, it has since expanded significantly over time adding more OOP and other features; as of 1997/C++98 standardization, C++ has added functional features, in addition to facilities for low-level memory manipulation for systems like microcomputers or to make operating systems like Linux or Windows, and even later came features like generic programming (through the use of  templates). C++ is usually implemented as a compiled language, and many vendors provide C++ compilers, including the Free Software Foundation, LLVM, Microsoft, Intel, Embarcadero, Oracle, and IBM.\nC++ was designed with systems programming and embedded, resource-constrained software and large systems in mind, with performance, efficiency, and flexibility of use as its design highlights.",
        "location": "Bell Labs, USA",
        "benefits": "Performance, object-oriented, rich ecosystem, hardware control.",
        "related": "C, Simula, Ada, Rust, D, Java, C#",
        "history": "1985. Bjarne Stroustrup extended C with abstractions like classes and templates to enable both low-level control and high-level design. C++ powers performance-critical applications, from game engines to embedded systems.",
        "authors": [
            { "name": "Bjarne Stroustrup" }
        ],
        "website": "https://isocpp.org/"
    },
    "C_Sharp": {
        "creator": "Anders Hejlsberg",
        "date": "2000",
        "description": "C# ( see SHARP) is a general-purpose high-level programming language supporting multiple paradigms. C# encompasses static typing, strong typing, lexically scoped, imperative, declarative, functional, generic, object-oriented (class-based), and component-oriented programming disciplines.\nThe principal designers of the C# programming language were Anders Hejlsberg, Scott Wiltamuth, and Peter Golde from Microsoft.",

        "location": "Microsoft, USA",
        "benefits": "Strong typing, rich .NET ecosystem, modern features, tooling.",
        "related": "C++, Java, Delphi, Modula-3, F#, Visual Basic .NET",
        "authors": [
            { "name": "Anders Hejlsberg" }
        ],
        "website": "https://docs.microsoft.com/en-us/dotnet/csharp/",
        "history": "2000. Designed by Anders Hejlsberg, C# blends modern language features with the .NET runtime to support enterprise, desktop and web applications. It emphasizes tooling, libraries and developer productivity.",
    },    "Clojure": {
        "creator": "Rich Hickey",
        "date": "2007",
        "description": "Clojure (, like closure) is a dynamic and functional dialect of the programming language Lisp on the Java platform.\nLike most other Lisps, Clojure's syntax is built on S-expressions that are first parsed into data structures by a Lisp reader before being compiled. Clojure's reader supports literal syntax for maps, sets, and vectors along with lists, and these are compiled to the mentioned structures directly.",

        "location": "USA",
        "benefits": "Immutability, functional programming, JVM interoperability, simplicity.",
        "related": "Lisp, Java, Haskell, Scheme, Racket",
        "authors": [
            { "name": "Rich Hickey" }
        ],
        "website": "https://clojure.org/",
        "history": "2007. Rich Hickey's Clojure brings Lisp's code-as-data and immutable data structures to the JVM, focusing on simplicity, concurrency and functional programming. It's used for building robust, composable systems.",
    },    "Cobol": {
        "creator": "CODASYL Committee (Grace Hopper)",
        "date": "1959",
        "description": "COBOL (Common Business-Oriented Language; ) is a compiled English-like computer programming language designed for business use. It is an imperative, procedural, and, since 2002, object-oriented language. COBOL is primarily used in business, finance, and administrative systems for companies and governments.",

        "location": "USA",
        "benefits": "Business data processing, stability, massive legacy codebases.",
        "related": "FLOW-MATIC, COMTRAN, PL/I",
        "authors": [
            { "name": "Grace Hopper" }
        ],
        "history": "1959. Created for business data processing, COBOL's verbose, English-like syntax made it accessible to non-academic programmers and it still runs critical financial systems today.",
    },    "CoffeeScript": {
        "creator": "Jeremy Ashkenas",
        "date": "2009",
        "description": "CoffeeScript is a programming language that compiles to JavaScript. It adds syntactic sugar inspired by Ruby, Python, and Haskell in an effort to enhance JavaScript's brevity and readability. Some added features include list comprehension and destructuring assignment.",

        "location": "USA",
        "benefits": "Concise syntax, readability, compiles to clean JavaScript.",
        "related": "JavaScript, Ruby, Python, Haskell, TypeScript",
        "authors": [
            { "name": "Jeremy Ashkenas" }
        ],
        "website": "https://coffeescript.org/",
        "history": "2009. A syntactic layer over JavaScript that introduced concise idioms and inspired later JS syntax improvements. CoffeeScript smoothed the migration to more expressive JavaScript patterns.",
    },    "CommonLisp": {
        "creator": "Committee (Guy L. Steele Jr. et al.)",
        "date": "1984",
        "description": "Common Lisp (CL) is a dialect of the Lisp programming language, published in American National Standards Institute (ANSI) standard document ANSI INCITS 226-1994 (S2018) (formerly X3.226-1994 (R1999)). The Common Lisp HyperSpec, a hyperlinked HTML version, has been derived from the ANSI Common Lisp standard.\nThe Common Lisp language was developed as a standardized and improved successor of Maclisp.",

        "location": "USA",
        "benefits": "Macros, dynamic typing, interactive development, flexibility.",
        "related": "Lisp, Scheme, MacLisp, Interlisp, Clojure",
        "history": "1984. A standardized Lisp dialect with powerful macros and dynamic runtime features, Common Lisp supports rapid prototyping and domain-specific language creation.",
        "authors": [
            { "name": "ANSI X3J13 committee" }, 
        ]
    },
    "Crystal": {
        "creator": "Ary Borenszweig",
        "date": "2014",
        "description": "Crystal is a high-level general-purpose, object-oriented programming language, designed and developed by Ary Borenszweig, Juan Wajnerman, Brian Cardiff and more than 530 contributors. With syntax inspired by the language Ruby, it is a compiled language with static type-checking, but specifying the types of variables or method arguments is generally unneeded. Types are resolved by an advanced global type inference algorithm.",

        "location": "Argentina",
        "benefits": "Ruby-like syntax, C-like performance, static typing.",
        "related": "Ruby, C, Go, Rust, C#",
        "authors": [
            { "name": "Ary Borenszweig" },            { "name": "Juan Wajnerman" },            { "name": "Brian Cardiff" }
        ],
        "website": "https://crystal-lang.org/",
        "history": "2014. Crystal aims to deliver Ruby-like syntax with static typing and native performance, targeting developers who want expressive code without sacrificing speed.",
    },    "D": {
        "creator": "Walter Bright",
        "date": "2001",
        "description": "D, also known as dlang, is a multi-paradigm system programming language created by Walter Bright at Digital Mars and released in 2001. Andrei Alexandrescu joined the design and development effort in 2007. Though it originated as a re-engineering of C++, D is now a very different language.",

        "location": "Digital Mars, USA",
        "benefits": "System programming, performance, safety, metaprogramming.",
        "related": "C, C++, Java, C#, Python, Ruby",
        "authors": [
            { "name": "Walter Bright" }
        ],
        "website": "https://dlang.org/",
        "history": "2001. Walter Bright's D modernizes systems programming by adding safety and productivity features while keeping C-like performance. It targets high-performance, maintainable code.",
    },    "Dart": {
        "creator": "Lars Bak, Kasper Lund",
        "date": "2011",
        "description": "Dart is a programming language designed by Lars Bak and Kasper Lund and developed by Google. It can be used to develop web and mobile apps as well as server and desktop applications.\nDart is an object-oriented, class-based, garbage-collected language with C-style syntax.",

        "location": "Google, Denmark/USA",
        "benefits": "UI optimized, fast compilation, cross-platform (Flutter).",
        "related": "Java, C#, JavaScript, TypeScript, Smalltalk",
        "authors": [
            { "name": "Lars Bak" },            { "name": "Kasper Lund" }
        ],
        "website": "https://dart.dev/",
        "history": "2011. Created for structured client-side development, Dart powers Flutter for cross-platform UIs and compiles to efficient native or JS code. Its toolchain focuses on developer productivity.",
    },    "Elixir": {
        "creator": "José Valim",
        "date": "2011",
        "description": "Elixir is a functional, concurrent, high-level general-purpose programming language that runs on the BEAM virtual machine, which is also used to implement the Erlang programming language. Elixir builds on top of Erlang and shares the same abstractions for building distributed, fault-tolerant applications. Elixir also provides tooling and an extensible design.",

        "location": "Plataformatec, Brazil",
        "benefits": "Concurrency, fault tolerance, functional, Ruby-like syntax.",
        "related": "Erlang, Ruby, Clojure, LFE",
        "authors": [
            { "name": "José Valim" }
        ],
        "website": "https://elixir-lang.org/",
        "history": "2011. Built on the Erlang VM by José Valim, Elixir blends fault-tolerant concurrency with elegant syntax and tooling, favored for scalable distributed services.",
    },    "Erlang": {
        "creator": "Joe Armstrong",
        "date": "1986",
        "description": "Erlang ( UR-lang) is a general-purpose, concurrent, functional high-level programming language, and a garbage-collected runtime system. The term Erlang is used interchangeably with Erlang/OTP, or Open Telecom Platform (OTP), which consists of the Erlang runtime system, several ready-to-use components (OTP) mainly written in Erlang, and a set of design principles for Erlang programs.\nThe Erlang runtime system is designed for systems with these traits:\n\nDistributed\nFault-tolerant\nSoft real-time\nHighly available, non-stop applications\nHot swapping, where code can be changed without stopping a system.",

        "location": "Ericsson, Sweden",
        "benefits": "Massive concurrency, fault tolerance, hot code swapping.",
        "related": "Prolog, Smalltalk, Elixir, LFE, Rust",
        "authors": [
            { "name": "Joe Armstrong" }
        ],
        "website": "https://www.erlang.org/",
        "history": "1986. Designed for telecoms, Erlang emphasizes lightweight processes, message-passing concurrency and robust fault-recovery. It's a foundation for resilient distributed systems.",
    },    "F_Sharp": {
        "creator": "Don Syme",
        "date": "2005",
        "description": "F# (pronounced F sharp) is a general-purpose, high-level, strongly typed, multi-paradigm programming language that encompasses functional, imperative, and object-oriented programming methods. It is most often used as a cross-platform Common Language Infrastructure (CLI) language on .NET, but can also generate JavaScript and graphics processing unit (GPU) code.\nF# is developed by the F# Software Foundation, Microsoft and open contributors.",

        "location": "Microsoft Research, UK",
        "benefits": "Functional-first, .NET integration, type inference, concise.",
        "related": "OCaml, C#, Haskell, Scala, Python",
        "authors": [
            { "name": "Don Syme" }
        ],
        "website": "https://fsharp.org/",
        "history": "2005. F# brings functional-first programming to .NET with strong typing, succinct syntax and excellent interop, used in finance, analytics and domain modelling.",
    },    "Fortran": {
        "creator": "John Backus",
        "date": "1957",
        "description": "Fortran (; formerly FORTRAN) is a third-generation, compiled, imperative programming language designed for numeric computation and scientific computing.\nFortran was originally developed by IBM with a reference manual being released in 1956; however, the first compilers only began to produce accurate code two years later. Fortran computer programs have been written to support scientific and engineering applications, such as numerical weather prediction, finite element analysis, computational fluid dynamics, plasma physics, geophysics, computational physics, crystallography and computational chemistry.",

        "location": "IBM, USA",
        "benefits": "Numerical computation, scientific computing, performance.",
        "related": "ALGOL, BASIC, PL/I, C, Julia, MATLAB",
        "authors": [
            { "name": "John Backus" }
        ],
        "history": "1957. One of the first high-level languages, Fortran was built for numerical computation and scientific programming; optimized compilers and legacy code keep it relevant in HPC.",
    },    "Go": {
        "creator": "Robert Griesemer, Rob Pike, Ken Thompson",
        "date": "2009",
        "description": "Go is a high-level, general-purpose programming language that is statically-typed and compiled. It is known for the simplicity of its syntax and the efficiency of development that it enables through the inclusion of a large standard library supplying many needs for common projects. It was designed at Google in 2007 by Robert Griesemer, Rob Pike, and Ken Thompson, and publicly announced in November 2009.",

        "location": "Google, USA",
        "benefits": "Simplicity, concurrency, fast compilation, static typing.",
        "related": "C, Pascal, Oberon, Limbo, CSP, Python",
        "authors": [
            { "name": "Robert Griesemer" },            { "name": "Rob Pike" },            { "name": "Ken Thompson" }
        ],
        "website": "https://go.dev/",
        "history": "2009. Designed at Google for simplicity, fast compilation and pragmatic concurrency, Go is a popular choice for cloud services, networking and developer tools.",
    },    "Groovy": {
        "creator": "James Strachan",
        "date": "2003",
        "description": "Apache Groovy is a Java-syntax-compatible object-oriented programming language for the Java platform. It is both a static and dynamic language with features similar to those of Python, Ruby, and Smalltalk. It can be used as both a programming language and a scripting language for the Java Platform, is compiled to Java virtual machine (JVM) bytecode, and interoperates seamlessly with other Java code and libraries.",

        "location": "USA",
        "benefits": "Java compatibility, scripting, dynamic features, DSLs.",
        "related": "Java, Python, Ruby, Smalltalk",
        "history": "2003. A dynamic JVM language that blends scripting ergonomics with Java interoperability; Groovy is used for build scripts, DSLs and rapid prototyping.",
        "authors": [
        ]
    },
    "Haskell": {
        "creator": "Committee (Simon Peyton Jones et al.)",
        "date": "1990",
        "description": "Haskell () is a general-purpose, statically typed, purely functional programming language with type inference and lazy evaluation. Haskell pioneered several programming language features including type classes for type-safe operator overloading and monadic input/output (IO). It is named after logician Haskell Curry.",

        "location": "Global",
        "benefits": "Pure functional, type safety, lazy evaluation, concurrency.",
        "related": "Miranda, ML, Clean, Agda, Purescript, Elm, Rust",
        "authors": [
            { "name": "Simon Peyton Jones" }
        ],
        "website": "https://www.haskell.org/",
        "history": "1990. A purely functional language stressing strong static types and lazy evaluation; Haskell is prized for expressiveness and correctness in research and some production systems.",
    },    "Java": {
        "creator": "James Gosling",
        "date": "1995",
        "description": "Java is a high-level, general-purpose, memory-safe, object-oriented programming language. It is intended to let programmers write once, run anywhere (WORA), meaning that compiled Java code can run on all platforms that support Java without the need to recompile. Java applications are typically compiled to bytecode that can run on any Java virtual machine (JVM) regardless of the underlying computer architecture.",

        "location": "Sun Microsystems, USA",
        "benefits": "Platform independence, vast ecosystem, enterprise-grade.",
        "related": "C++, Smalltalk, Objective-C, C#, Kotlin, Scala",
        "paradigm": "Object-oriented, Class-based, Imperative",
        "typeSystem": "Static, Strong",
        "authors": [
            { "name": "James Gosling" }
        ],
        "website": "https://www.oracle.com/java/",
        "history": "1995. Java's portable bytecode and extensive libraries made it the backbone of enterprise applications and large-scale distributed systems for decades.",
    },    "JavaScript": {
        "creator": "Brendan Eich",
        "date": "1995",
        "description": "JavaScript (JS) is a programming language and core technology of the Web, alongside HTML and CSS. It was created by Brendan Eich in 1995. As of 2025, the overwhelming majority of websites (98.9%) uses JavaScript for client side webpage behavior.\nWeb browsers have a dedicated JavaScript engine that executes the client code.",

        "location": "Netscape, USA",
        "benefits": "Ubiquitous, versatile, huge ecosystem, async capabilities.",
        "related": "Self, Scheme, Java, ECMAScript, TypeScript",
        "paradigm": "Multi-paradigm, Event-driven, Functional, Imperative, Prototype-based",
        "typeSystem": "Dynamic, Weak",
        "authors": [
            { "name": "Brendan Eich" }
        ],
        "website": "https://developer.mozilla.org/en-US/docs/Web/JavaScript",
        "history": "1995. Created for the browser, JavaScript evolved into a universal platform for web and server-side code; its flexibility enabled an enormous ecosystem.",
    },    "Julia": {
        "creator": "Jeff Bezanson, Stefan Karpinski, Viral B. Shah, Alan Edelman",
        "date": "2012",
        "description": "Julia is a dynamic general-purpose programming language. As a high-level language, distinctive aspects of Julia's design include a type system with parametric polymorphism, the use of multiple dispatch as a core programming paradigm, just-in-time compilation and a parallel garbage collection implementation. Notably, Julia does not support classes with encapsulated methods but instead relies on the types of all of a function's arguments to determine which method will be called.",

        "location": "MIT, USA",
        "benefits": "Speed of C, ease of Python, great for scientific computing.",
        "related": "MATLAB, Python, R, Lisp, Fortran",
        "authors": [
            { "name": "Jeff Bezanson" },            { "name": "Stefan Karpinski" },            { "name": "Viral B. Shah" },            { "name": "Alan Edelman" }
        ],
        "website": "https://julialang.org/",
        "history": "2012. Built for numerical and scientific computing, Julia combines easy syntax with high-performance JIT-compiled code, reducing the need for separate prototyping and production languages.",
    },    "Kotlin": {
        "creator": "JetBrains",
        "date": "2011",
        "description": "Kotlin () is a cross-platform, statically typed, general-purpose high-level programming language with type inference. Kotlin is designed to interoperate fully with Java, and the JVM version of Kotlin's standard library depends on the Java Class Library,\nbut type inference allows its syntax to be more concise. Kotlin mainly targets the JVM, but also compiles to JavaScript (e.g.",

        "location": "JetBrains, Czech Republic",
        "benefits": "Concise, null-safe, great Java interop, Android official.",
        "related": "Java, Scala, C#, Groovy, Swift",
        "authors": [
            { "name": "JetBrains Team" }
        ],
        "website": "https://kotlinlang.org/",
        "history": "2011. JetBrains developed Kotlin to modernize JVM development with concise syntax, null-safety and great Java interop, now a primary language for Android.",
    },    "Lua": {
        "creator": "Roberto Ierusalimschy, Luiz Henrique de Figueiredo, Waldemar Celes",
        "date": "1993",
        "description": "Lua  is a lightweight, high-level, multi-paradigm programming language designed mainly for embedded use in applications. Lua is cross-platform software, since the interpreter of compiled bytecode is written in ANSI C, and Lua has a relatively simple C application programming interface (API) to embed it into applications.\nLua originated in 1993 as a language for extending software applications to meet the increasing demand for customization at the time.",

        "location": "PUC-Rio, Brazil",
        "benefits": "Lightweight, fast, embeddable, simple syntax.",
        "related": "Scheme, SNOBOL, C, Python, JavaScript",
        "authors": [
            { "name": "Roberto Ierusalimschy" },            { "name": "Luiz Henrique de Figueiredo" },            { "name": "Waldemar Celes" }
        ],
        "website": "https://www.lua.org/",
        "history": "1993. Lightweight, embeddable and fast, Lua is ubiquitous in game scripting and embedded contexts thanks to a tiny runtime and simple C API.",
    },    "Nim": {
        "creator": "Andreas Rumpf",
        "date": "2008",
        "description": "Nim is a general-purpose programming language, multi-paradigm, statically typed, compiled, high-level system programming language. It was designed and developed by a team led by Andreas Rumpf. Nim aims to be \"efficient, expressive, and elegant\", and supports metaprogramming, functional, message passing, procedural, and object-oriented programming paradigms.",

        "location": "Germany",
        "benefits": "Python-like syntax, C-like performance, metaprogramming.",
        "related": "Python, Ada, Modula-3, Lisp, C",
        "authors": [
            { "name": "Andreas Rumpf" }
        ],
        "website": "https://nim-lang.org/",
        "history": "2008. Nim offers Python-like syntax, powerful metaprogramming and C-level performance, aiming for expressive yet efficient system-level code.",
    },    "OCaml": {
        "creator": "Xavier Leroy et al.",
        "date": "1996",
        "description": "OCaml ( oh-KAM-\u0259l, formerly Objective Caml) is a general-purpose, high-level, multi-paradigm programming language which extends the Caml dialect of ML with object-oriented features. OCaml was created in 1996 by Xavier Leroy, J\u00e9r\u00f4me Vouillon, Damien Doligez, Didier R\u00e9my, Asc\u00e1nder Su\u00e1rez, and others.\nThe OCaml toolchain includes an interactive top-level interpreter, a bytecode compiler, an optimizing native code compiler, a reversible debugger, and a package manager (OPAM) together with a composable build system for OCaml (Dune).",

        "location": "INRIA, France",
        "benefits": "Strong typing, functional, efficient native code.",
        "related": "ML, Standard ML, Haskell, F#, Rust",
        "history": "1996. A pragmatic functional language with strong typing and efficient native code generation, OCaml is used in compilers, tooling and domain-specific systems.",
        "authors": [
            { "name": "Inria" }, 
        ]
    },
    "Octave": {
        "creator": "John W. Eaton",
        "date": "1988",
        "description": "GNU Octave is a scientific programming language for scientific computing and numerical computation. Octave helps in solving linear and nonlinear problems numerically, and for performing other numerical experiments using a language that is mostly compatible with MATLAB. It may also be used as a batch-oriented language. As part of the GNU Project, it is free software under the terms of the GNU General Public License.",

        "location": "University of Wisconsin-Madison, USA",
        "benefits": "MATLAB compatible, free, numerical computing.",
        "related": "MATLAB, Julia, Python (NumPy), R",
        "history": "1988. An open numerical computation environment compatible with MATLAB, Octave is convenient for algorithm prototyping and academic work.",
        "authors": [
            { "name": "John W. Eaton and many others" }, 
        ]
    },
    "Pascal": {
        "creator": "Niklaus Wirth",
        "date": "1970",
        "description": "Pascal is an imperative and procedural programming language, designed by Niklaus Wirth as a small, efficient language intended to encourage good programming practices using structured programming and data structuring. It is named after French mathematician, philosopher and physicist Blaise Pascal.\nPascal was developed on the pattern of the ALGOL 60 language.",

        "location": "ETH Zurich, Switzerland",
        "benefits": "Structured programming, teaching, strong typing.",
        "related": "ALGOL, Modula-2, Oberon, Ada, Delphi",
        "authors": [
            { "name": "Niklaus Wirth" }
        ],
        "history": "1970. Niklaus Wirth designed Pascal to teach structured programming and data structuring; it influenced many later languages and educational curricula.",
    },    "Perl": {
        "creator": "Larry Wall",
        "date": "1987",
        "description": "Perl is a high-level, general-purpose, interpreted, dynamic programming language. Though Perl is not officially an acronym, there are various backronyms in use, including \"Practical Extraction and Reporting Language\".\nPerl was developed by Larry Wall in 1987 as a general-purpose Unix scripting language to make report processing easier.",

        "location": "Unisys, USA",
        "benefits": "Text processing, regex, CPAN, practical.",
        "related": "AWK, Sed, C, Shell, Raku, Python, Ruby",
        "authors": [
            { "name": "Larry Wall" }
        ],
        "website": "https://www.perl.org/",
        "history": "1987. Larry Wall's practical text-processing language excels at regex-driven scripting and rapid data munging; Perl was the web glue for many early projects.",
    },    "PHP": {
        "creator": "Rasmus Lerdorf",
        "date": "1995",
        "description": "PHP is a general-purpose scripting language geared towards web development. It was  created by Danish-Canadian programmer Rasmus Lerdorf in 1993 and released in 1995. The PHP reference implementation is now produced by the PHP Group.",

        "location": "Denmark/Canada",
        "benefits": "Web development, vast ecosystem (WordPress, Laravel).",
        "related": "Perl, C, Java, JavaScript, Python",
        "authors": [
            { "name": "Rasmus Lerdorf" }
        ],
        "website": "https://www.php.net/",
        "history": "1995. Initially built for web pages, PHP scaled into server-side frameworks and CMS platforms, powering a significant fraction of the web.",
    },    "Prolog": {
        "creator": "Alain Colmerauer, Robert Kowalski",
        "date": "1972",
        "description": "Prolog is a logic programming language that has its origins in artificial intelligence, automated theorem proving,  and computational linguistics.\nProlog has its roots in first-order logic, a formal logic. Unlike many other programming languages, Prolog is intended primarily as a declarative programming language: the program is a set of facts and rules, which define relations.",

        "location": "University of Aix-Marseille and University of Edinburgh",
        "benefits": "Logic programming, AI, pattern matching, declarative.",
        "related": "Lisp, SQL, Datalog, Mercury, Erlang",
        "authors": [
            { "name": "Alain Colmerauer" },            { "name": "Robert Kowalski" }
        ],
        "history": "1972. A logic-programming paradigm where code expresses facts and rules; Prolog is well suited for symbolic reasoning, constraint solving and AI research.",
    },    "Python": {
        "creator": "Guido van Rossum",
        "date": "1991",
        "description": "Python is a high-level, general-purpose programming language. Its design philosophy emphasizes code readability with the use of significant indentation. Python is dynamically type-checked and garbage-collected.",

        "location": "CWI, Netherlands",
        "benefits": "Readability, versatility, massive ecosystem, beginner-friendly.",
        "related": "ABC, C, Haskell, Lisp, Perl, Ruby, Java",
        "paradigm": "Multi-paradigm, Functional, Imperative, Object-oriented, Structured",
        "typeSystem": "Dynamic, Strong",
        "authors": [
            { "name": "Guido van Rossum" }
        ],
        "website": "https://www.python.org/",
        "history": "1991. Guido van Rossum designed Python for readability and productivity; with an enormous ecosystem it excels in scripting, data science, automation and web services.",
    },    "R": {
        "creator": "Ross Ihaka, Robert Gentleman",
        "date": "1993",
        "description": "R is a programming language for statistical computing and data visualization. It has been widely adopted in the fields of data mining, bioinformatics, data analysis, and data science.\nThe core R language is extended by a large number of software packages, which contain reusable code, documentation, and sample data.",

        "location": "University of Auckland, New Zealand",
        "benefits": "Statistical analysis, data visualization, CRAN packages.",
        "related": "S, Scheme, Lisp, Python (Pandas), Julia",
        "authors": [
            { "name": "Ross Ihaka" },            { "name": "Robert Gentleman" }
        ],
        "website": "https://www.r-project.org/",
        "history": "1993. A language and environment for statistical computing and visualization, R offers domain-specific tools for data analysis and reproducible research.",
    },    "Racket": {
        "creator": "PLT Inc. (Matthew Flatt et al.)",
        "date": "1995",
        "description": "Racket is a general-purpose, multi-paradigm programming language. The Racket language is a modern dialect of Lisp and a descendant of Scheme. It is designed as a platform for programming language design and implementation.",

        "location": "Northeastern University, USA",
        "benefits": "Language-oriented programming, education, DSLs, macros.",
        "related": "Scheme, Lisp, Clojure, Haskell",
        "history": "1995. A descendant of Scheme created for language-oriented programming, education and building new DSLs with powerful macro systems.",
        "authors": [
            { "name": "PLT Inc." }, 
        ]
    },
    "Ruby": {
        "creator": "Yukihiro Matsumoto",
        "date": "1995",
        "description": "Ruby is a general-purpose programming language. It was designed with an emphasis on programming productivity and simplicity. In Ruby, everything is an object, including primitive data types.",

        "location": "Japan",
        "benefits": "Developer happiness, metaprogramming, Rails ecosystem.",
        "related": "Perl, Smalltalk, Eiffel, Ada, Lisp, Python",
        "authors": [
            { "name": "Yukihiro Matsumoto" }
        ],
        "website": "https://www.ruby-lang.org/",
        "history": "1995. Designed for programmer happiness, Ruby's elegant syntax and metaprogramming made it the language behind rapid web development frameworks like Rails.",
    },    "Rust": {
        "creator": "Graydon Hoare",
        "date": "2010",
        "description": "Rust is a general-purpose programming language. It is noted for its emphasis on performance, type safety, concurrency, and memory safety.\nRust supports multiple programming paradigms.",

        "location": "Mozilla, USA",
        "benefits": "Memory safety, concurrency, performance, no GC.",
        "related": "C++, ML, Haskell, Erlang, Swift, C",
        "paradigm": "Multi-paradigm, Concurrent, Functional, Imperative, Structured",
        "typeSystem": "Static, Strong, Safe",
        "authors": [
            { "name": "Graydon Hoare" }
        ],
        "website": "https://www.rust-lang.org/",
        "history": "2010. Rust targets safe, concurrent systems programming with compile-time guarantees that prevent many classes of runtime errors while delivering native performance.",
    },    "Scala": {
        "creator": "Martin Odersky",
        "date": "2004",
        "description": "Scala ( SKAH-lah) is a strongly statically typed high-level general-purpose programming language that supports both object-oriented programming and functional programming. Designed to be concise, many of Scala's design decisions are intended to address criticisms of Java.\nScala source code can be compiled to Java bytecode and run on a Java virtual machine (JVM).",

        "location": "EPFL, Switzerland",
        "benefits": "Functional + OOP, JVM, type safety, concurrency (Akka).",
        "related": "Java, ML, Haskell, Smalltalk, Erlang, Kotlin",
        "authors": [
            { "name": "Martin Odersky" }
        ],
        "website": "https://www.scala-lang.org/",
        "history": "2004. Scala fuses object-oriented and functional programming on the JVM, enabling concise, type-safe code for large systems and data pipelines.",
    },    "Scheme": {
        "creator": "Guy L. Steele Jr., Gerald Jay Sussman",
        "date": "1975",
        "description": "Scheme is a dialect of the Lisp family of programming languages. Scheme was created during the 1970s at the MIT Computer Science and Artificial Intelligence Laboratory (MIT CSAIL) and released by its developers, Guy L. Steele and Gerald Jay Sussman, via a series of memos now known as the Lambda Papers. It was the first dialect of Lisp to choose lexical scope and the first to require implementations to perform tail-call optimization, giving stronger support for functional programming and associated techniques such as recursive algorithms.",

        "location": "MIT AI Lab, USA",
        "benefits": "Minimalist, powerful macros, education, standards (R5RS, R7RS).",
        "related": "Lisp, Common Lisp, Racket, Clojure, JavaScript",
        "authors": [
            { "name": "Guy L. Steele Jr." },            { "name": "Gerald Jay Sussman" }
        ],
        "website": "https://www.scheme.org/",
        "history": "1975. A minimalist Lisp dialect focusing on clean semantics and first-class procedures; Scheme is central in programming language education.",
    },    "Swift": {
        "creator": "Chris Lattner et al.",
        "date": "2014",
        "description": "Swift is a high-level general-purpose, multi-paradigm, compiled programming language created by Chris Lattner in 2010 for Apple Inc. and maintained by the open-source community. Swift compiles to machine code and uses an LLVM-based compiler.",

        "location": "Apple Inc., USA",
        "benefits": "Safety, performance, modern syntax, Apple ecosystem.",
        "related": "Objective-C, Rust, C#, Python, Ruby",
        "authors": [
            { "name": "Chris Lattner" }
        ],
        "website": "https://www.swift.org/",
        "history": "2014. Apple's modern language focusing on safety, performance and developer ergonomics; Swift has largely superseded Objective-C for Apple platform development.",
    },    "Tcl": {
        "creator": "John Ousterhout",
        "date": "1988",
        "description": "Tcl (pronounced \"tickle\" or \"TCL\"; originally Tool Command Language) is a high-level, general-purpose, interpreted, dynamic programming language. It was designed with the goal of being very simple but powerful. Tcl casts everything into the mold of a command, even programming constructs like variable assignment and procedure definition.",

        "location": "UC Berkeley, USA",
        "benefits": "Scripting, GUI (Tk), embedding, simplicity.",
        "related": "Lisp, Shell, Python, Perl",
        "history": "1988. A simple, embeddable scripting language often paired with Tk for GUI applications; Tcl is valued for its ease of extension.",
        "authors": [
            { "name": "Tcl Core Team" }, 
        ]
    },
    "TypeScript": {
        "creator": "Anders Hejlsberg (Microsoft)",
        "date": "2012",
        "description": "TypeScript (TS) is a high-level programming language that adds static typing with optional type annotations to JavaScript. It is designed for developing large applications. It transpiles to JavaScript.",

        "location": "Microsoft, USA",
        "benefits": "Type safety, IDE support, scales to large codebases.",
        "related": "JavaScript, C#, Java, CoffeeScript",
        "authors": [
            { "name": "Anders Hejlsberg" }
        ],
        "website": "https://www.typescriptlang.org/",
        "history": "2012. Adds optional static typing and tooling to JavaScript to improve maintainability and catch errors early while compiling to standard JS.",
    },    "Vala": {
        "creator": "Jürg Billeter",
        "date": "2006",
        "description": "Vala is an object-oriented programming language with a self-hosting compiler that generates C code and uses the GObject system.\nVala is syntactically similar to C# and includes notable features such as anonymous functions, signals, properties, generics, assisted memory management, exception handling, type inference, and foreach statements. Its developers, J\u00fcrg Billeter and Raffaele Sandrini, wanted to bring these features to the plain C runtime with little overhead and no special runtime support by targeting the GObject object system.",

        "location": "GNOME Project",
        "benefits": "GObject integration, C performance, modern syntax.",
        "related": "C, C#, Java, GObject",
        "history": "2006. Offers modern language conveniences while compiling to C and targeting GObject, simplifying GNOME application development.",
        "authors": [
            { "name": "Jürg Billeter" },             { "name": "Rico Tzschichholz" },             { "name": "Raffaele Sandrini" }, 
        ]
    },
    "VisualBasic": {
        "creator": "Microsoft (Alan Cooper)",
        "date": "1991",
        "description": "Visual Basic is a name for a family of programming languages from Microsoft.",

        "location": "Microsoft, USA",
        "benefits": "RAD, ease of use, Windows integration, legacy support.",
        "related": "BASIC, QuickBASIC, VBA, C#",
        "history": "1991. Made event-driven Windows application development accessible with RAD tools and a beginner-friendly syntax.",
        "authors": [
        ]
    },
    "Zig": {
        "creator": "Andrew Kelley",
        "date": "2016",
        "description": "Zig is an in-development imperative, general-purpose, statically typed, compiled system programming language designed by Andrew Kelley. It is free and open-source software, released under an MIT License.\nIt was created as an improvement to the C programming language, with the intent of being lighter and simpler to program in, while offering more functionality.",

        "location": "USA",
        "benefits": "No hidden control flow, manual memory management, comptime.",
        "related": "C, C++, Rust, Go, Jai",
        "authors": [
            { "name": "Andrew Kelley" }
        ],
        "website": "https://ziglang.org/",
        "history": "2016. A modern systems language focused on simplicity, explicit control and predictable performance, positioning itself as a pragmatic C alternative.",
    },    "BASH": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "The Bourne Again SHell. The default shell for most Linux distributions and macOS.",

        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction.",
        "related": "Bourne Shell, Zsh, Ksh"
    },    "BASIC": {
        "creator": "John G. Kemeny, Thomas E. Kurtz",
        "date": "1964",
        "description": "BASIC (Beginner's All-purpose Symbolic Instruction Code) is a family of general-purpose, high-level programming languages designed for ease of use. The original version was created by John G. Kemeny and Thomas E. Kurtz at Dartmouth College in 1964. They wanted to enable students in non-scientific fields to use computers.",

        "location": "Dartmouth College, USA",
        "benefits": "Easy to learn, interactive, historical significance.",
        "related": "Fortran, Visual Basic, COMAL",
        "history": "1964. Beginner's All-purpose Symbolic Instruction Code (BASIC) was created by John Kemeny and Thomas Kurtz at Dartmouth College to give students easy access to computing. Early implementations ran on the Dartmouth Time-Sharing System and emphasized simple, interactive use (PRINT, LET, GOTO, line numbers). BASIC exploded in popularity on microcomputers in the 1970s and 1980s via many dialects (and influential ports such as Altair BASIC), helping introduce programming to hobbyists and a generation of developers.",
        "authors": [
            { "name": "John G. Kemeny" },
            { "name": "Thomas E. Kurtz" }
        ]
    },
    "Wren": {
        "creator": "Bob Nystrom",
        "date": "2013",
        "description": "A small, fast, class-based concurrent scripting language. Designed for embedding in applications, it is object-oriented and comparable in speed to Lua.",

        "location": "USA",
        "benefits": "High performance, small footprint, clean syntax.",
        "related": "Lua, Smalltalk, Dart, JavaScript",
        "history": "2013. A small, fast, class-based concurrent scripting language. Designed by Bob Nystrom, Wren aims to be a modern, improved version of Lua, featuring a small footprint and high performance.",
    },    "Red": {
        "creator": "Nenad Rakocevic",
        "date": "2011",
        "description": "Red is a programming language designed to overcome the limitations of the programming language Rebol. Red was introduced in 2011 by Nenad Rako\u010devi\u0107, and is both an imperative and functional programming language. Its syntax and general usage overlaps that of the interpreted Rebol language.",

        "location": "Global",
        "benefits": "Full-stack, symbolic, cross-platform.",
        "related": "Rebol, Lisp, Lua",
        "history": "2011. A next-generation functional and symbolic language inspired by REBOL. It's designed to be a full-stack language, capable of everything from low-level systems programming to high-level GUI application development.",
        "authors": [
            { "name": "Nenad Rakočević" }, 
        ]
    },
    "Pike": {
        "creator": "Fredrik Hübinette, Per Hedbor",
        "date": "1994",
        "description": "A general-purpose, high-level, cross-platform, dynamic programming language with a C-like syntax. It features garbage collection, advanced data types, and first-class anonymous functions.",

        "location": "Linköping, Sweden",
        "benefits": "C-like syntax, dynamic typing, concurrency.",
        "related": "LPC, C, C++, Python"
    },    "Fennel": {
        "creator": "Calvin Rose (Project Lead)",
        "date": "2016",
        "description": "A programming language that brings Lisp syntax and macros to the Lua architecture. It compiles to Lua and runs with zero overhead, offering full Lua compatibility.",

        "location": "Global",
        "benefits": "Lisp syntax, Lua speed, zero overhead, embeddable.",
        "related": "Lua, Lisp, Clojure"
    },    "Janet": {
        "creator": "Calvin Rose",
        "date": "2017",
        "description": "A functional and imperative programming language designed for system scripting, expressive automation, and embedding. It features a built-in PEG engine and Lisp-like macros.",

        "location": "USA",
        "benefits": "Embeddable, PEG parsing, structural editing.",
        "related": "Lua, Clojure, Lisp, C"
    },    "Io": {
        "creator": "Steve Dekorte",
        "date": "2002",
        "description": "A pure object-oriented, prototype-based programming language. In Io, everything is an object, and it uses a message-passing model similar to Smalltalk but with a prototype-based object model like Self.",

        "location": "USA",
        "benefits": "Simplicity, flexibility, concurrency (coroutines).",
        "related": "Smalltalk, Self, Lisp, Lua"
    },    "Factor": {
        "creator": "Slava Pestov",
        "date": "2003",
        "description": "A stack-oriented, concatenative, multi-paradigm programming language. It combines the flexibility of dynamic typing with the performance of native compilation.",

        "location": "USA",
        "benefits": "Concatenative, metaprogramming, performance.",
        "related": "Forth, Joy, Lisp, Smalltalk"
    },    "Icon": {
        "creator": "Ralph Griswold",
        "date": "1977",
        "description": "A very high-level programming language based on goal-directed execution and string scanning. It makes extensive use of generators and backtracking.",

        "location": "University of Arizona, USA",
        "benefits": "String processing, goal-directed execution, generators.",
        "related": "SNOBOL, Python, Unicon"
    },    "Objective-C": {
        "creator": "Brad Cox, Tom Love",
        "date": "1984",
        "description": "Objective-C is a high-level general-purpose, object-oriented programming language that adds Smalltalk-style message passing (messaging) to the C programming language. Originally developed by Brad Cox and Tom Love in the early 1980s, it was selected by NeXT for its NeXTSTEP operating system. Due to Apple macOS\u2019s direct lineage from NeXTSTEP, Objective-C was the standard language used, supported, and promoted by Apple for developing macOS and iOS applications (via their respective application programming interfaces (APIs), Cocoa and Cocoa Touch) from 1997, when Apple purchased NeXT, until the introduction of the Swift language in 2014.",

        "location": "Stepstone, USA",
        "benefits": "Dynamic runtime, C compatibility, Apple legacy.",
        "related": "C, Smalltalk, Swift, C++",
        "history": "1984. Combining C with Smalltalk-style messaging, Objective-C powered classic Apple development with a dynamic runtime and flexible object model.",
        "authors": [
        ]
    },
    "Raku": {
        "creator": "Larry Wall",
        "date": "2015",
        "description": "Raku is a member of the Perl family of programming languages. Formerly named Perl 6, it was renamed in October 2019. Raku introduces elements of many modern and historical languages.",

        "location": "Global",
        "benefits": "Expressiveness, grammars, concurrency.",
        "related": "Perl, Haskell, Python, Ruby",
        "authors": [
            { "name": "Raku community" }
        ]
    },
    "V": {
        "creator": "Alexander Medvednikov",
        "date": "2019",
        "description": "V, also known as vlang, is an in-development statically typed, compiled programming language created by Alexander Medvednikov in early 2019. It was inspired by Go, and other programming languages including Oberon, Swift, and Rust. It is free and open-source software released under the MIT License, and currently in beta.",

        "location": "Global",
        "benefits": "Fast compilation, simplicity, safety.",
        "related": "Go, Oberon, Rust, Swift, C",
        "authors": []
    }
};
