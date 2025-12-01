import * as fs from 'fs/promises';
import * as path from 'path';
import { glob } from 'glob';
import { exec } from 'child_process';
import * as util from 'util';
import { fileURLToPath } from 'url';
import puppeteer from 'puppeteer';

const execPromise = util.promisify(exec);
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

interface MetricResult {
    matrix: string;
    time: number;
    iterations: number;
    memory: number;
    cpu_user: number;
    cpu_sys: number;
    status: string;
}

interface SolverMetrics {
    solver: string;
    timestamp: string;
    results: MetricResult[];
}

// --- Data Structures ---

const languageHistories: Record<string, string> = {
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
    "SQL": "1974. The declarative standard for relational data queries and manipulation; SQL abstracts data retrieval and is foundational to many systems.",
    "Swift": "2014. Apple's modern language focusing on safety, performance and developer ergonomics; Swift has largely superseded Objective-C for Apple platform development.",
    "Tcl": "1988. A simple, embeddable scripting language often paired with Tk for GUI applications; Tcl is valued for its ease of extension.",
    "TypeScript": "2012. Adds optional static typing and tooling to JavaScript to improve maintainability and catch errors early while compiling to standard JS.",
    "Vala": "2006. Offers modern language conveniences while compiling to C and targeting GObject, simplifying GNOME application development.",
    "Verilog": "1984. An HDL for modeling and simulating digital circuits; Verilog is essential in hardware design and synthesis flows.",
    "VHDL": "1980. A strongly-typed hardware description language used for rigorous modelling and verification of digital systems in industry.",
    "Vimscript": "1991. The scripting language of Vim enabling powerful editor automation, macros and plugin development for efficient text editing.",
    "VisualBasic": "1991. Made event-driven Windows application development accessible with RAD tools and a beginner-friendly syntax.",
    "WebAssembly": "2017. A compact binary format that runs in browsers and other hosts, enabling near-native performance for multiple languages.",
    "Zig": "2016. A modern systems language focused on simplicity, explicit control and predictable performance, positioning itself as a pragmatic C alternative.",
};

const quotes: Record<string, string> = {
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
    "SQL": "Declarative logic in a procedural world. Asking the database to dream in Sudoku grids.",
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
    "Matrices": "The input data. The puzzle itself.",
    "Matrices_Backup": "The backup.",
    "Matrices_Filtered": "The filter."
};

const personalities: Record<string, Record<string, string>> = {
    "Standard": {
        "C": "C: Low-level systems workhorse — predictable, efficient, and widely taught.",
        "C++": "C++: High performance with complex abstractions — flexible but demanding.",
        "Rust": "Rust: Modern safety-first systems language with strong compile-time guarantees.",
        "Go": "Go: Pragmatic concurrency and simplicity for cloud services and tools.",
        "Python": "Python: Readable, batteries-included, ideal for scripting, data and prototypes.",
        "JavaScript": "JavaScript: The web's lingua franca — ubiquitous, dynamic, and evolving.",
        "TypeScript": "TypeScript: JavaScript with types — better tooling and maintainability.",
        "Java": "Java: Proven JVM ecosystem for enterprise systems and large codebases.",
        "C_Sharp": "C#: Mature, well-supported managed language with great IDEs and libraries.",
        "PHP": "PHP: Ubiquitous server-side web language with a vast legacy ecosystem.",
        "Ruby": "Ruby: Expressive and productive, often chosen for developer happiness.",
        "Swift": "Swift: Safe, modern language for Apple platforms and systems programming.",
        "Kotlin": "Kotlin: Concise JVM language with strong Android adoption and null-safety.",
        "Haskell": "Haskell: Pure functional language emphasizing correctness and types.",
        "OCaml": "OCaml: Practical functional language with strong typing and fast native code.",
        "Lisp": "Lisp: Macro-driven, symbolic language family with a long AI heritage.",
        "Clojure": "Clojure: Lisp on the JVM with immutable data structures and concurrency focus.",
        "Erlang": "Erlang: Concurrency and fault tolerance for telecom-grade systems.",
        "Elixir": "Elixir: Friendly syntax on Erlang VM, great for resilient distributed apps.",
        "SQL": "SQL: Declarative language for relational data querying and manipulation.",
        "Fortran": "Fortran: Battle-tested numerical computing language for high-performance math.",
        "Assembly": "Assembly: Maximum control and minimal abstraction — used where performance is critical.",
        "Bash": "Bash: Shell scripting glue for automating system tasks and pipelines.",
        "Perl": "Perl: Practical text-processing and scripting toolkit with expressive regexes.",
        "Lua": "Lua: Tiny, embeddable scripting language, beloved by games and embedded systems.",
        "Julia": "Julia: High-performance scientific computing with friendly syntax.",
        "WebAssembly": "WebAssembly: Portable, fast binary target for running multiple languages on the web.",
        "Zig": "Zig: Explicit, simple systems language aiming for predictable performance.",
        "default": "A language with its own strengths and tradeoffs."
    },

    "Neuromancer": {
        "C": "C: Bare metal whispers — minimal abstraction, maximum throughput.",
        "C++": "C++: Heavy machinery with countless gears; power hides complexity.",
        "Rust": "Rust: Memory ghosts are exorcised at compile-time; concurrency tamed.",
        "Go": "Go: Channels hum in the background — predictable, machine-like rhythm.",
        "Python": "Python: Rapid prototyping — a soft, readable veneer over deeper systems.",
        "JavaScript": "JavaScript: Event-driven chaos, patchworked into ubiquity.",
        "TypeScript": "TypeScript: Structured order on top of the web's entropy.",
        "Java": "Java: Industrial runtime with predictable steam-engine reliability.",
        "C_Sharp": "C#: Polished and managed — enterprise grade with neat tools.",
        "PHP": "PHP: Resilient, cobbled-together web logic surviving the net's underside.",
        "Ruby": "Ruby: Elegant spells that summon productivity — soft and expressive.",
        "Swift": "Swift: Precise and modern — tightened for platform harmony.",
        "Kotlin": "Kotlin: Pragmatic JVM sorcery — concise incantations with null-safety wards.",
        "Haskell": "Haskell: Pure functions etched in marble — correctness as creed.",
        "Erlang": "Erlang: Concurrency as ritual; processes fall and resurrect gracefully.",
        "Elixir": "Elixir: Erlang's resilience dressed in elegant syntax and tooling.",
        "SQL": "SQL: Declarative incantations to coax meaning from tables.",
        "WebAssembly": "WebAssembly: Binary synapses connecting worlds at near-native speed.",
        "default": "A net of languages, each a node with its own signal pattern."
    },

    "The Jockey": {
        "C": "C: The thoroughbred — raw speed out of the gate, but demands a skilled rider.",
        "C++": "C++: The heavy-weight contender — powerful when tamed, unwieldy when not.",
        "Rust": "Rust: The disciplined challenger — steady, precise, and hard to topple.",
        "Go": "Go: The reliable pacer — consistent lap times and low fuss.",
        "Python": "Python: The showy performer — flashy moves, not always fastest.",
        "JavaScript": "JavaScript: The wildcard — unpredictable but always in the race.",
        "TypeScript": "TypeScript: JavaScript with a coach — fewer mistakes, cleaner lines.",
        "Java": "Java: The endurance runner — long races suit its steady gait.",
        "C_Sharp": "C#: A well-trained athlete with excellent support crew and tooling.",
        "PHP": "PHP: The scrappy veteran — keeps going when others fold.",
        "Ruby": "Ruby: A graceful jumper — elegant but not built for sprints.",
        "Swift": "Swift: Shortboard specialist — quick bursts, smooth turns on Apple turf.",
        "Kotlin": "Kotlin: Agile and modern — nimble on the JVM track.",
        "Haskell": "Haskell: A cerebral racer — nails the lines with mathematical precision.",
        "Fortran": "Fortran: The old pro — still competitive in numerical sprints.",
        "Assembly": "Assembly: Rocket start, tiny margin for error — thrilling to watch.",
        "default": "The field is deep; every language brings its own racing style."
    },

    "The Professor": {
        "C": "C: Study of resource efficiency — minimal runtime and predictable costs.",
        "C++": "C++: Rich feature set enabling varied programming paradigms; complexity requires discipline.",
        "Rust": "Rust: Demonstrates ownership and type-system-based safety; an instructive modern design.",
        "Go": "Go: Engineered for simplicity and concurrency primitives; pragmatic trade-offs are explicit.",
        "Python": "Python: Excellent for algorithmic experimentation and rapid iteration; runtime overhead considered.",
        "JavaScript": "JavaScript: Flexible semantics that reward careful architecture and testing.",
        "TypeScript": "TypeScript: Type discipline applied to dynamic JavaScript ecosystems; valuable for scale.",
        "Java": "Java: Mature tooling and a stable runtime make it suitable for robust distributed systems.",
        "C_Sharp": "C#: A managed, strongly-typed language with advanced tooling and a rich standard library.",
        "Haskell": "Haskell: A case study in purity and strong types, illuminating functional program design.",
        "OCaml": "OCaml: Efficient functional programming with practical native performance.",
        "Erlang": "Erlang: Model of lightweight concurrency and fault-tolerance architectures.",
        "Elixir": "Elixir: Modern ergonomics on a time-tested concurrency VM; excellent for observable systems.",
        "SQL": "SQL: Declarative optimization relies heavily on the database engine's query planner.",
        "Fortran": "Fortran: Compiler-optimized numeric loops still relevant in HPC benchmarks.",
        "Assembly": "Assembly: Useful in microbenchmarks and bootstrapping but impractical for large systems.",
        "default": "Each language is an experiment in tradeoffs; measure, analyze, and choose appropriately."
    },

    "The Surfer": {
        "C": "C: Totally radical speed — like carving a perfect wave at dawn.",
        "C++": "C++: Big energy, epic turns, a little gnarly sometimes.",
        "Rust": "Rust: Clean lines, safe drops — no wipeouts from memory bugs.",
        "Go": "Go: Smooth, no fuss paddling — steady and dependable.",
        "Python": "Python: Chill and easygoing — fun to ride for the crew.",
        "JavaScript": "JavaScript: Wild seas but fun tricks if you know the breaks.",
        "TypeScript": "TypeScript: JS with clearer lines — less risky maneuvers.",
        "Java": "Java: Longboard cruising — stable and comfortable for long sessions.",
        "Ruby": "Ruby: Playful and elegant, like a retro twin-fin.",
        "PHP": "PHP: Scrappy — keeps paddling even in rough surf.",
        "Lua": "Lua: Lightweight, versatile — like a compact fishboard for tight spots.",
        "Swift": "Swift: Polished performance on Apple-shaped waves.",
        "Kotlin": "Kotlin: Smooth transitions and modern styling on the JVM lineup.",
        "Haskell": "Haskell: A meditative session — slow, precise, rewarding.",
        "Elixir": "Elixir: Reliable lineup and great community vibes for big waves.",
        "WebAssembly": "WebAssembly: Feels like jet propulsion in the water — near-native rip.",
        "default": "Catch the wave you like — every language has its own stoke."
    }
};


const methodologyTexts: Record<string, string> = {
    "Standard": `
        <p>The <strong>Total Score</strong> is a composite metric designed to compare overall efficiency against the C baseline.</p>
        <h3 style="color: var(--secondary);">The Baseline: C</h3>
        <p>The <strong>C</strong> implementation is the reference standard (1.0) for Time, Memory, and CPU usage.</p>
        <h3 style="color: var(--secondary);">The Formula</h3>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = (TimeRatio + MemRatio + CpuRatio) / 3
        </div>
        <p>Where:</p>
        <ul style="font-size: 0.9em; color: var(--text);">
            <li><strong>TimeRatio</strong> = Wall Clock Time / C Time</li>
            <li><strong>MemRatio</strong> = Peak Memory (RSS) / C Memory</li>
            <li><strong>CpuRatio</strong> = Total CPU Time (User+Sys) / C CPU Time</li>
        </ul>
        <h3 style="color: var(--secondary);">Interpretation</h3>
        <ul style="list-style: none; padding: 0;">
            <li style="margin-bottom: 8px;"><strong style="color: var(--primary);">1.0</strong> : Parity with C.</li>
            <li style="margin-bottom: 8px;"><strong style="color: #ff0055;">&gt; 1.0</strong> : Less Efficient.</li>
            <li style="margin-bottom: 8px;"><strong style="color: #00b8ff;">&lt; 1.0</strong> : More Efficient.</li>
        </ul>
        <p style="font-size: 0.9em; color: var(--muted); text-align: center; margin-top: 20px;"><em>Lower scores are better.</em></p>
    `,
    "Neuromancer": `
        <p>The construct uses a composite index. Time, Memory, CPU. Three vectors, one score.</p>
        <h3 style="color: var(--secondary);">Target: C</h3>
        <p><strong>C</strong> is the flatline (1.0). Deviate at your own risk.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = AVG(Time_R, Mem_R, CPU_R)
        </div>
        <p>Bloated memory? You burn. High CPU? You burn. Slow? You definitely burn.</p>
    `,
    "The Jockey": `
        <p>It's a triathlon now, folks! Speed, Stamina (Memory), and Effort (CPU)!</p>
        <h3 style="color: var(--secondary);">The Pace Car: C</h3>
        <p><strong>C</strong> runs the perfect line at 1.0.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Average of Time, Mem, and CPU vs C
        </div>
        <p>You gotta be fast AND light to win this cup!</p>
    `,
    "The Professor": `
        <p>The scoring methodology now employs a multivariate analysis. We evaluate Time, Resident Set Size, and CPU Time.</p>
        <h3 style="color: var(--secondary);">Control: C</h3>
        <p><strong>C</strong> (1.0) remains the baseline for all three dimensions.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = &Sigma;(Ratios) / 3
        </div>
        <p>This penalizes memory-managed languages that trade RAM for development speed.</p>
    `,
    "The Surfer": `
        <p>It's not just about speed, bro. It's about flow. Time, Memory, CPU. The whole vibe.</p>
        <h3 style="color: var(--secondary);">The Big Kahuna: C</h3>
        <p><strong>C</strong> is the perfect wave (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = (Time + Mem + CPU) / 3 ... vs C
        </div>
        <p>Don't be heavy, don't be slow. Just flow.</p>
    `
};

const languageMetadata: Record<string, any> = {
    "Assembly": {
        "creator": "Kathleen Booth",
        "date": "1947",
        "description": "Low-level symbolic representation of machine code. As close to the metal as you can get.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f6/Kathleen_Booth.jpg/440px-Kathleen_Booth.jpg",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Simpleicons_Interface_code-file-1.svg/64px-Simpleicons_Interface_code-file-1.svg.png",
        "location": "Birkbeck College, London",
        "benefits": "Direct hardware control, maximum performance, zero overhead."
    },
    "Awk": {
        "creator": "Aho, Weinberger, Kernighan",
        "date": "1977",
        "description": "A domain-specific language designed for text processing and typically used as a data extraction and reporting tool.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/25/Alfred_Aho.jpg/440px-Alfred_Aho.jpg", // Alfred Aho
        "location": "Bell Labs, USA",
        "benefits": "Excellent for text processing, one-liners, and data extraction."
    },
    "Bash": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "The Bourne Again SHell. The default shell for most Linux distributions and macOS.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Gnu-bash-logo.svg/1200px-Gnu-bash-logo.svg.png",
        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction."
    },
    "Basic": {
        "creator": "John G. Kemeny, Thomas E. Kurtz",
        "date": "1964",
        "description": "Beginner's All-purpose Symbolic Instruction Code. Designed to emphasize ease of use.",
        "image": "https://upload.wikimedia.org/wikipedia/en/thumb/5/52/Kemeny_and_Kurtz.jpg/300px-Kemeny_and_Kurtz.jpg",
        "location": "Dartmouth College, USA",
        "benefits": "Easy to learn, interactive, historical significance."
    },
    "C": {
        "creator": "Dennis Ritchie",
        "date": "1972",
        "description": "General-purpose systems programming language. Excels in operating systems, embedded systems, and performance-critical applications where direct hardware access is essential.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/23/Dennis_Ritchie_2011.jpg",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/C_Programming_Language.svg/64px-C_Programming_Language.svg.png",
        "location": "Bell Labs, USA",
        "benefits": "High performance, portability, low-level access, massive legacy."
    },
    "C++": {
        "creator": "Bjarne Stroustrup",
        "date": "1985",
        "description": "Multi-paradigm language combining procedural and object-oriented programming. Excels in game engines, high-performance applications, and systems requiring both low-level control and high-level abstractions.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/0/0e/Bjarne-stroustrup.jpg",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/ISO_C%2B%2B_Logo.svg/64px-ISO_C%2B%2B_Logo.svg.png",
        "location": "Bell Labs, USA",
        "benefits": "Performance, object-oriented, rich ecosystem, hardware control."
    },
    "C_Sharp": {
        "creator": "Anders Hejlsberg",
        "date": "2000",
        "description": "A modern, object-oriented, and type-safe programming language derived from C and C++.",
        "image": "https://github.com/ahejlsberg.png",
        "location": "Microsoft, USA",
        "benefits": "Strong typing, rich .NET ecosystem, modern features, tooling."
    },
    "Clojure": {
        "creator": "Rich Hickey",
        "date": "2007",
        "description": "A dynamic, general-purpose programming language, combining the approachability and interactive development of a scripting language with an efficient and robust infrastructure for multithreaded programming.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5d/Rich_Hickey_2013.jpg/440px-Rich_Hickey_2013.jpg",
        "location": "USA",
        "benefits": "Immutability, functional programming, JVM interoperability, simplicity."
    },
    "Cobol": {
        "creator": "CODASYL Committee (Grace Hopper)",
        "date": "1959",
        "description": "Common Business-Oriented Language. Designed for business use.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Commodore_Grace_M._Hopper%2C_USN_%28covered%29.jpg/440px-Commodore_Grace_M._Hopper%2C_USN_%28covered%29.jpg",
        "location": "USA",
        "benefits": "Business data processing, stability, massive legacy codebases."
    },
    "CoffeeScript": {
        "creator": "Jeremy Ashkenas",
        "date": "2009",
        "description": "A programming language that compiles to JavaScript. It adds syntactic sugar inspired by Ruby, Python and Haskell.",
        "image": "https://github.com/jashkenas.png",
        "location": "USA",
        "benefits": "Concise syntax, readability, compiles to clean JavaScript."
    },
    "CommonLisp": {
        "creator": "Committee (Guy L. Steele Jr. et al.)",
        "date": "1984",
        "description": "A dialect of the Lisp programming language, published in ANSI Standard X3.226-1994.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Guy_Steele.jpg/440px-Guy_Steele.jpg",
        "location": "USA",
        "benefits": "Macros, dynamic typing, interactive development, flexibility."
    },
    "Crystal": {
        "creator": "Ary Borenszweig",
        "date": "2014",
        "description": "A general-purpose, object-oriented programming language, designed and implemented by Ary Borenszweig, Juan Wajnerman, Brian Cardiff and more than 300 contributors.",
        "image": "https://github.com/asterite.png",
        "location": "Argentina",
        "benefits": "Ruby-like syntax, C-like performance, static typing."
    },
    "D": {
        "creator": "Walter Bright",
        "date": "2001",
        "description": "A general-purpose system programming language with a C-like syntax that compiles to native code.",
        "image": "https://github.com/WalterBright.png",
        "location": "Digital Mars, USA",
        "benefits": "System programming, performance, safety, metaprogramming."
    },
    "Dart": {
        "creator": "Lars Bak, Kasper Lund",
        "date": "2011",
        "description": "A client-optimized language for fast apps on any platform.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c8/Lars_Bak.jpg/440px-Lars_Bak.jpg",
        "location": "Google, Denmark/USA",
        "benefits": "UI optimized, fast compilation, cross-platform (Flutter)."
    },
    "Elixir": {
        "creator": "José Valim",
        "date": "2011",
        "description": "A dynamic, functional language designed for building scalable and maintainable applications.",
        "image": "https://github.com/josevalim.png",
        "location": "Plataformatec, Brazil",
        "benefits": "Concurrency, fault tolerance, functional, Ruby-like syntax."
    },
    "Erlang": {
        "creator": "Joe Armstrong",
        "date": "1986",
        "description": "A general-purpose, concurrent, functional programming language, and a garbage-collected runtime system.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Joe_Armstrong_2014.jpg/440px-Joe_Armstrong_2014.jpg",
        "location": "Ericsson, Sweden",
        "benefits": "Massive concurrency, fault tolerance, hot code swapping."
    },
    "F_Sharp": {
        "creator": "Don Syme",
        "date": "2005",
        "description": "A functional-first programming language that encompasses functional, imperative, and object-oriented programming methods.",
        "image": "https://github.com/dsyme.png",
        "location": "Microsoft Research, UK",
        "benefits": "Functional-first, .NET integration, type inference, concise."
    },
    "Fortran": {
        "creator": "John Backus",
        "date": "1957",
        "description": "The first high-level programming language. Still dominates scientific computing.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/b/b8/John_Backus_2.jpg",
        "location": "IBM, USA",
        "benefits": "Numerical computation, scientific computing, performance."
    },
    "Go": {
        "creator": "Robert Griesemer, Rob Pike, Ken Thompson",
        "date": "2009",
        "description": "Compiled, statically-typed language with built-in concurrency. Excels in cloud services, microservices, CLI tools, and networked applications with simple, readable syntax.",
        "image": "https://github.com/robpike.png",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Go_Logo_Blue.svg/64px-Go_Logo_Blue.svg.png",
        "location": "Google, USA",
        "benefits": "Simplicity, concurrency, fast compilation, static typing."
    },
    "Groovy": {
        "creator": "James Strachan",
        "date": "2003",
        "description": "A Java-syntax-compatible object-oriented programming language for the Java platform.",
        "image": "https://github.com/jstrachan.png",
        "location": "USA",
        "benefits": "Java compatibility, scripting, dynamic features, DSLs."
    },
    "Haskell": {
        "creator": "Committee (Simon Peyton Jones et al.)",
        "date": "1990",
        "description": "A standardized, general-purpose, purely functional programming language with non-strict semantics and strong static typing.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/1200px-Haskell-Logo.svg.png",
        "location": "Global",
        "benefits": "Pure functional, type safety, lazy evaluation, concurrency."
    },
    "Java": {
        "creator": "James Gosling",
        "date": "1995",
        "description": "Platform-independent, object-oriented language. Excels in enterprise applications, Android development, and large-scale distributed systems with strong backwards compatibility.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/1/14/James_Gosling_2008.jpg",
        "logo": "https://upload.wikimedia.org/wikipedia/en/thumb/3/30/Java_programming_language_logo.svg/64px-Java_programming_language_logo.svg.png",
        "location": "Sun Microsystems, USA",
        "benefits": "Portability, enterprise ecosystem, performance, stability."
    },
    "JavaScript": {
        "creator": "Brendan Eich",
        "date": "1995",
        "description": "Dynamic, prototype-based scripting language. Excels in web development (front-end and back-end with Node.js), browser automation, and building interactive user interfaces.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/d/d1/Brendan_Eich_Mozilla_Foundation_official_photo.jpg",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6a/JavaScript-logo.png/64px-JavaScript-logo.png",
        "location": "Netscape, USA",
        "benefits": "Ubiquity, web interactivity, huge ecosystem, flexibility."
    },
    "Julia": {
        "creator": "Jeff Bezanson, Stefan Karpinski, Viral B. Shah, Alan Edelman",
        "date": "2012",
        "description": "A high-level, high-performance, dynamic programming language for technical computing.",
        "image": "https://github.com/JeffBezanson.png",
        "location": "MIT, USA",
        "benefits": "High performance, numerical computing, ease of use."
    },
    "Kotlin": {
        "creator": "JetBrains",
        "date": "2011",
        "description": "A cross-platform, statically typed, general-purpose programming language with type inference.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Kotlin_Icon.png/1200px-Kotlin_Icon.png",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Kotlin_Icon.png/64px-Kotlin_Icon.png",
        "location": "Russia/International",
        "benefits": "Concise, null safety, Java interoperability, Android standard."
    },
    "Lua": {
        "creator": "Roberto Ierusalimschy et al.",
        "date": "1993",
        "description": "A lightweight, high-level, multi-paradigm programming language designed primarily for embedded use in applications.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cf/Lua-Logo.svg/1200px-Lua-Logo.svg.png",
        "location": "PUC-Rio, Brazil",
        "benefits": "Lightweight, embeddable, fast, simple."
    },
    "Nim": {
        "creator": "Andreas Rumpf",
        "date": "2008",
        "description": "A statically typed, compiled systems programming language. It combines successful concepts from mature languages like Python, Ada and Modula.",
        "image": "https://github.com/Araq.png",
        "location": "Germany",
        "benefits": "Performance, expressiveness, metaprogramming, C compilation."
    },
    "OCaml": {
        "creator": "Xavier Leroy et al.",
        "date": "1996",
        "description": "A general-purpose, multi-paradigm programming language which extends the Caml dialect of ML with object-oriented features.",
        "image": "https://github.com/xavierleroy.png",
        "location": "INRIA, France",
        "benefits": "Functional, type safety, performance, industrial strength."
    },
    "Octave": {
        "creator": "John W. Eaton",
        "date": "1988",
        "description": "A high-level language, primarily intended for numerical computations.",
        "image": "https://github.com/jwe.png",
        "location": "USA",
        "benefits": "Numerical computation, MATLAB compatibility, free software."
    },
    "Pascal": {
        "creator": "Niklaus Wirth",
        "date": "1970",
        "description": "An imperative and procedural programming language, designed as a small, efficient language intended to encourage good programming practices.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Pascal_programming_language_logo.svg/1200px-Pascal_programming_language_logo.svg.png",
        "location": "ETH Zurich, Switzerland",
        "benefits": "Structured programming, teaching, strong typing."
    },
    "Perl": {
        "creator": "Larry Wall",
        "date": "1987",
        "description": "A family of two high-level, general-purpose, interpreted, dynamic programming languages.",
        "image": "https://github.com/TimToady.png",
        "location": "USA",
        "benefits": "Text processing, scripting, flexibility, CPAN."
    },
    "PHP": {
        "creator": "Rasmus Lerdorf",
        "date": "1995",
        "description": "Originally 'Personal Home Page'. Powers a significant portion of the web (including WordPress).",
        "image": "https://github.com/rlerdorf.png",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/PHP-logo.svg/64px-PHP-logo.svg.png",
        "location": "Canada",
        "benefits": "Web development, ease of deployment, vast ecosystem."
    },
    "PostScript": {
        "creator": "John Warnock, Charles Geschke",
        "date": "1982",
        "description": "A page description language in the electronic publishing and desktop publishing business.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/John_Warnock.jpg/440px-John_Warnock.jpg",
        "location": "Adobe, USA",
        "benefits": "Printing, vector graphics, stack-based, device independence."
    },
    "PowerShell": {
        "creator": "Jeffrey Snover",
        "date": "2006",
        "description": "A task automation and configuration management framework from Microsoft.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/PowerShell_5.0_icon.png/1200px-PowerShell_5.0_icon.png",
        "location": "Microsoft, USA",
        "benefits": "Automation, system administration, object-oriented pipeline."
    },
    "Prolog": {
        "creator": "Alain Colmerauer",
        "date": "1972",
        "description": "A logic programming language associated with artificial intelligence and computational linguistics.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Alain_Colmerauer.jpg/440px-Alain_Colmerauer.jpg",
        "location": "University of Aix-Marseille, France",
        "benefits": "Logic programming, AI, pattern matching, declarative."
    },
    "Python": {
        "creator": "Guido van Rossum",
        "date": "1991",
        "description": "High-level, interpreted language emphasizing code readability. Excels in data science, machine learning, web development, automation, and rapid prototyping with extensive library ecosystem.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/e/e2/Guido-portrait-2014-drc.jpg",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Python-logo-notext.svg/64px-Python-logo-notext.svg.png",
        "location": "CWI, Netherlands",
        "benefits": "Readability, vast libraries, data science, AI."
    },
    "R": {
        "creator": "Ross Ihaka, Robert Gentleman",
        "date": "1993",
        "description": "A programming language and free software environment for statistical computing and graphics.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/1200px-R_logo.svg.png",
        "location": "University of Auckland, New Zealand",
        "benefits": "Statistics, data visualization, data analysis."
    },
    "Racket": {
        "creator": "Matthew Flatt",
        "date": "1995",
        "description": "A general-purpose, multi-paradigm programming language based on the Scheme dialect of Lisp.",
        "image": "https://github.com/mflatt.png",
        "location": "Rice University, USA",
        "benefits": "Language creation, education, macros, functional."
    },
    "Rexx": {
        "creator": "Mike Cowlishaw",
        "date": "1979",
        "description": "Restructured Extended Executor. A structured, high-level programming language designed for ease of learning and reading.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Mike_Cowlishaw.jpg/440px-Mike_Cowlishaw.jpg",
        "location": "IBM, UK",
        "benefits": "Scripting, mainframe automation, ease of use."
    },
    "Ruby": {
        "creator": "Yukihiro Matsumoto",
        "date": "1995",
        "description": "Designed for developer happiness. Famous for the Ruby on Rails framework.",
        "image": "https://github.com/matz.png",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Ruby_logo.svg/64px-Ruby_logo.svg.png",
        "location": "Japan",
        "benefits": "Developer happiness, web development, scripting, elegance."
    },
    "Rust": {
        "creator": "Graydon Hoare",
        "date": "2010",
        "description": "Systems programming language focused on safety and concurrency. Excels in systems software, WebAssembly, embedded systems, and performance-critical applications without garbage collection.",
        "image": "https://github.com/graydon.png",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d5/Rust_programming_language_black_logo.svg/64px-Rust_programming_language_black_logo.svg.png",
        "location": "Mozilla, Canada",
        "benefits": "Memory safety, performance, concurrency, modern tooling."
    },
    "Scala": {
        "creator": "Martin Odersky",
        "date": "2004",
        "description": "A general-purpose programming language providing support for both object-oriented programming and functional programming.",
        "image": "https://github.com/odersky.png",
        "location": "EPFL, Switzerland",
        "benefits": "Functional/OOP blend, JVM, scalability, conciseness."
    },
    "Scheme": {
        "creator": "Guy L. Steele Jr., Gerald Jay Sussman",
        "date": "1975",
        "description": "A minimalist dialect of the Lisp family of programming languages.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/3/39/Lambda_lc.svg/1200px-Lambda_lc.svg.png",
        "location": "MIT, USA",
        "benefits": "Minimalism, education, functional, macros."
    },
    "Sed": {
        "creator": "Lee E. McMahon",
        "date": "1974",
        "description": "A stream editor for filtering and transforming text.",
        "image": "https://upload.wikimedia.org/wikipedia/en/thumb/9/9e/Lee_McMahon.jpg/220px-Lee_McMahon.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Stream editing, text transformation, automation."
    },
    "Smalltalk": {
        "creator": "Alan Kay, Dan Ingalls, Adele Goldberg",
        "date": "1972",
        "description": "An object-oriented, dynamically typed reflective programming language.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Alan_Kay_2006.jpg/440px-Alan_Kay_2006.jpg",
        "location": "Xerox PARC, USA",
        "benefits": "Pure OOP, live environment, influence on modern GUIs."
    },
    "SNOBOL": {
        "creator": "David J. Farber, Ralph E. Griswold, Ivan P. Polonsky",
        "date": "1962",
        "description": "A series of computer programming languages developed between 1962 and 1967 at AT&T Bell Laboratories.",
        "image": "https://www2.cs.arizona.edu/people/griswold/reg.jpg",
        "location": "Bell Labs, USA",
        "benefits": "String manipulation, pattern matching, historical significance."
    },
    "SQL": {
        "creator": "Donald D. Chamberlin, Raymond F. Boyce",
        "date": "1974",
        "description": "A domain-specific language used in programming and designed for managing data held in a relational database management system.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/8/87/Sql_data_base_with_logo.png/1200px-Sql_data_base_with_logo.png",
        "location": "IBM, USA",
        "benefits": "Data management, standard, declarative, powerful queries."
    },
    "Swift": {
        "creator": "Chris Lattner",
        "date": "2014",
        "description": "Apple's replacement for Objective-C. Safe, fast, and expressive.",
        "image": "https://github.com/lattner.png",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/Swift_logo.svg/64px-Swift_logo.svg.png",
        "location": "Apple, USA",
        "benefits": "Safety, speed, modern syntax, Apple ecosystem."
    },
    "Tcl": {
        "creator": "John Ousterhout",
        "date": "1988",
        "description": "Tool Command Language. A dynamic programming language.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/John_Ousterhout.jpg/440px-John_Ousterhout.jpg",
        "location": "UC Berkeley, USA",
        "benefits": "Scripting, GUI (Tk), embedding, simplicity."
    },
    "TypeScript": {
        "creator": "Anders Hejlsberg",
        "date": "2012",
        "description": "A strict syntactical superset of JavaScript and adds optional static typing to the language.",
        "image": "https://github.com/ahejlsberg.png",
        "logo": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Typescript_logo_2020.svg/64px-Typescript_logo_2020.svg.png",
        "location": "Microsoft, USA",
        "benefits": "Type safety, scalability, tooling, JavaScript compatibility."
    },
    "Vala": {
        "creator": "Jürg Billeter, Rafał Pietrak",
        "date": "2006",
        "description": "A programming language that aims to bring modern language features to C developers without the overhead of a runtime environment.",
        "image": "https://github.com/juergbi.png",
        "location": "GNOME Project",
        "benefits": "GObject system, C performance, modern syntax, GNOME dev."
    },
    "Verilog": {
        "creator": "Phil Moorby",
        "date": "1984",
        "description": "A hardware description language (HDL) used to model electronic systems.",
        "image": "https://www.computerhistory.org/atchm/wp-content/uploads/2016/04/moorby-phil-2016-chm-fellow.jpg",
        "location": "Gateway Design Automation, USA",
        "benefits": "Hardware modeling, simulation, synthesis, industry standard."
    },
    "VHDL": {
        "creator": "US Department of Defense",
        "date": "1980",
        "description": "VHSIC Hardware Description Language. A hardware description language used in electronic design automation.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/VHDL_Logo.svg/1200px-VHDL_Logo.svg.png",
        "location": "USA",
        "benefits": "Hardware description, strong typing, concurrency, standard."
    },
    "Vimscript": {
        "creator": "Bram Moolenaar",
        "date": "1991",
        "description": "The scripting language of the Vim text editor.",
        "image": "https://github.com/brammool.png",
        "location": "Netherlands",
        "benefits": "Editor customization, automation, plugins."
    },
    "VisualBasic": {
        "creator": "Microsoft (Alan Cooper)",
        "date": "1991",
        "description": "A third-generation event-driven programming language from Microsoft for its Component Object Model (COM) programming model.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/4/4f/Alan_Cooper_2010.jpg",
        "location": "Microsoft, USA",
        "benefits": "RAD, ease of use, Windows integration, legacy support."
    },
    "WebAssembly": {
        "creator": "W3C Community Group",
        "date": "2017",
        "description": "A binary instruction format for a stack-based virtual machine.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1f/WebAssembly_Logo.svg/1200px-WebAssembly_Logo.svg.png",
        "location": "Global",
        "benefits": "Performance, portability, language agnostic, web standard."
    },

    "Jq": {
        "creator": "Stephen Dolan",
        "date": "2012",
        "description": "A lightweight and flexible command-line JSON processor.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/JSON_vector_logo.svg/1200px-JSON_vector_logo.svg.png",
        "location": "USA",
        "benefits": "JSON processing, functional, filter-based."
    },
    "Make": {
        "creator": "Stuart Feldman",
        "date": "1976",
        "description": "A build automation tool that automatically builds executable programs and libraries from source code.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Dependency management, build automation."
    },
    "Gnuplot": {
        "creator": "Thomas Williams, Colin Kelley",
        "date": "1986",
        "description": "A command-line program that can generate two- and three-dimensional plots of functions, data, and data fits.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3d/Gnuplot_logo.svg/1200px-Gnuplot_logo.svg.png",
        "location": "USA",
        "benefits": "Plotting, data visualization, scripting."
    },
    "XSLT": {
        "creator": "W3C",
        "date": "1998",
        "description": "A language for transforming XML documents into other XML documents.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/Xml_logo.svg/1200px-Xml_logo.svg.png",
        "location": "Worldwide",
        "benefits": "XML transformation, functional, template-based."
    },
    "Zsh": {
        "creator": "Paul Falstad",
        "date": "1990",
        "description": "A Unix shell that can be used as an interactive login shell and as a command interpreter for shell scripting.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Zsh_logo.svg/1200px-Zsh_logo.svg.png",
        "location": "Princeton University, USA",
        "benefits": "Interactive shell, scripting, plugin ecosystem."
    },
    "AppleScript": {
        "creator": "Apple Inc.",
        "date": "1993",
        "description": "A scripting language created by Apple Inc. that facilitates automated control over scriptable Mac applications.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/AppleScript_Logo.png/440px-AppleScript_Logo.png",
        "location": "Apple, USA",
        "benefits": "Automation, inter-application communication, English-like syntax."
    },

    "Expect": {
        "creator": "Don Libes",
        "date": "1990",
        "description": "A program to automate interactions with programs that expose a text terminal interface.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "NIST, USA",
        "benefits": "Automation, testing, interactive scripting."
    },
    "SQLite": {
        "creator": "D. Richard Hipp",
        "date": "2000",
        "description": "A C-language library that implements a small, fast, self-contained, high-reliability, full-featured, SQL database engine.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/SQLite370.svg/1200px-SQLite370.svg.png",
        "location": "USA",
        "benefits": "Serverless, zero-configuration, transactional SQL."
    },

    "Tcsh": {
        "creator": "Ken Greer",
        "date": "1975",
        "description": "An enhanced but completely compatible version of the Berkeley UNIX C shell (csh).",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Carnegie Mellon University, USA",
        "benefits": "Interactive shell, command completion, history."
    },
    "Ksh": {
        "creator": "David Korn",
        "date": "1983",
        "description": "A Unix shell which was developed by David Korn at Bell Labs in the early 1980s.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Scripting, interactive use, backward compatibility."
    },
    "Dash": {
        "creator": "Herbert Xu",
        "date": "1997",
        "description": "A POSIX-compliant implementation of /bin/sh that aims to be as small as possible.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Australia",
        "benefits": "Speed, POSIX compliance, minimal footprint."
    },
    "Fish": {
        "creator": "Axel Liljencrantz",
        "date": "2005",
        "description": "A smart and user-friendly command line shell for macOS, Linux, and the rest of the family.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Fish_shell_logo_ascii.svg/1200px-Fish_shell_logo_ascii.svg.png",
        "location": "Sweden",
        "benefits": "Autosuggestions, web-based configuration, clean syntax."
    },
    "Bc": {
        "creator": "Robert Morris, Lorinda Cherry",
        "date": "1975",
        "description": "An arbitrary-precision calculator language.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Arbitrary precision, math scripting."
    },
    "Dc": {
        "creator": "Robert Morris, Lorinda Cherry",
        "date": "1970",
        "description": "A reverse-Polish notation (RPN) calculator that operates on arbitrary-precision integers.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Bell Labs, USA",
        "benefits": "RPN, arbitrary precision, stack-based."
    },
    "EmacsLisp": {
        "creator": "Richard Stallman",
        "date": "1985",
        "description": "A dialect of the Lisp programming language used as a scripting language for GNU Emacs.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/1200px-EmacsIcon.svg.png",
        "location": "MIT, USA",
        "benefits": "Editor customization, extensibility, live coding."
    },
    "Forth": {
        "creator": "Charles H. Moore",
        "date": "1970",
        "description": "A stack-oriented, concatenative, procedural, and reflective programming language.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/Forth_logo.svg/1200px-Forth_logo.svg.png",
        "location": "NRAO, USA",
        "benefits": "Compactness, hardware control, interactivity."
    },
    "Haxe": {
        "creator": "Nicolas Cannasse",
        "date": "2005",
        "description": "A high-level cross-platform language that compiles to many other languages.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/28/Haxe_logo.svg/1200px-Haxe_logo.svg.png",
        "location": "France",
        "benefits": "Cross-platform, strong typing, single codebase."
    },
    "Jupyter": {
        "creator": "Fernando Pérez et al.",
        "date": "2014",
        "description": "An open-source project that supports interactive data science and scientific computing.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Jupyter_logo.svg/1200px-Jupyter_logo.svg.png",
        "location": "Global",
        "benefits": "Interactive computing, data visualization, education."
    },
    "M4": {
        "creator": "Brian Kernighan, Dennis Ritchie",
        "date": "1977",
        "description": "A general-purpose macro processor.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Macro expansion, text processing, configuration."
    },
    "Objective-C": {
        "creator": "Brad Cox, Tom Love",
        "date": "1984",
        "description": "A general-purpose, object-oriented programming language that adds Smalltalk-style messaging to C.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/440px-Dennis_Ritchie_2011.jpg",
        "location": "Stepstone, USA",
        "benefits": "Dynamic runtime, C compatibility, Apple legacy."
    },
    "Raku": {
        "creator": "Larry Wall",
        "date": "2015",
        "description": "A member of the Perl family. Expressive, multi-paradigm, and versatile.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Raku_logo.svg/1200px-Raku_logo.svg.png",
        "location": "Global",
        "benefits": "Expressiveness, grammars, concurrency."
    },
    "V": {
        "creator": "Alexander Medvednikov",
        "date": "2019",
        "description": "A statically typed, compiled language designed for maintainability and speed.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f2/V-logo.svg/1200px-V-logo.svg.png",
        "location": "Global",
        "benefits": "Fast compilation, simplicity, safety."
    },
    "Zig": {
        "creator": "Andrew Kelley",
        "date": "2016",
        "description": "A general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/5/51/Zig_logo.svg/1200px-Zig_logo.svg.png",
        "location": "USA",
        "benefits": "No hidden control flow, manual memory management, comptime."
    },
    "BASH": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "The Bourne Again SHell. The default shell for most Linux distributions and macOS.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Gnu-bash-logo.svg/1200px-Gnu-bash-logo.svg.png",
        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction."
    },
    "BASIC": {
        "creator": "John G. Kemeny, Thomas E. Kurtz",
        "date": "1964",
        "description": "Beginner's All-purpose Symbolic Instruction Code. Designed to emphasize ease of use.",
        "image": "https://upload.wikimedia.org/wikipedia/en/thumb/5/52/Kemeny_and_Kurtz.jpg/300px-Kemeny_and_Kurtz.jpg",
        "location": "Dartmouth College, USA",
        "benefits": "Easy to learn, interactive, historical significance."
    }
};

// --- Logic ---

async function findSolvers(rootDir: string): Promise<string[]> {
    // Find all runMe_ai.sh files
    const pattern = path.join(rootDir, 'Manual', '*', 'runMe_ai.sh');
    console.log(`Searching for solvers with pattern: ${pattern}`);
    const solvers = await glob(path.join(rootDir, 'Manual', '*/runMe_ai.sh'));
    const filteredSolvers = solvers.filter(s => !s.includes('/Racket/') && !s.includes('/Cobol/'));
    console.log(`Found ${filteredSolvers.length} solvers (excluding Racket and Cobol):`, filteredSolvers);
    return filteredSolvers;
}

async function runSolver(scriptPath: string): Promise<SolverMetrics | null> {
    const solverDir = path.dirname(scriptPath);
    const solverName = path.basename(solverDir);

    console.log(`Running solver: ${solverName}`);

    try {
        const { stdout, stderr } = await execPromise(`./runMe_ai.sh "../../Matrices/[12].matrix"`, { cwd: solverDir });

        try {
            const results = JSON.parse(stdout);
            return {
                solver: solverName,
                timestamp: new Date().toISOString(),
                results: results
            };
        } catch (e) {
            console.error(`Failed to parse JSON output from ${solverName}:`, e);
            console.log("Stdout:", stdout);
            return null;
        }
    } catch (e) {
        console.error(`Failed to run solver ${solverName}:`, e);
        return null;
    }
}

async function generateHtml(metrics: SolverMetrics[], history: any[], personalities: any, languageMetadata: any, methodologyTexts: any): Promise<string> {
    const maxMatrices = 5; // Focus on Matrix 1-5

    // Sort results by total time (fastest first)
    const sortedMetrics = [...metrics].sort((a, b) => {
        const timeA = a.results.reduce((acc, r) => acc + r.time, 0);
        const timeB = b.results.reduce((acc, r) => acc + r.time, 0);
        return timeA - timeB;
    });

    // Find C baseline
    const cMetrics = metrics.find(m => m.solver === 'C');
    const cTimes = cMetrics ? cMetrics.results.map(r => r.time) : [];
    const cTotalIters = cMetrics ? cMetrics.results.reduce((a, b) => a + b.iterations, 0) : 0;

    // C Baselines for Composite Score
    const cTotalTime = cTimes.reduce((a, b) => a + b, 0);
    const cTotalMem = cMetrics ? Math.max(...cMetrics.results.map(r => r.memory)) : 1; // Max RSS
    const cTotalCpu = cMetrics ? cMetrics.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0) : 1;

    // Calculate mismatch count
    let mismatchCount = 0;
    if (cTotalIters > 0) {
        mismatchCount = metrics.filter(m => {
            if (m.solver === 'C') return false;
            const totalIters = m.results.reduce((a, b) => a + b.iterations, 0);
            return totalIters !== cTotalIters;
        }).length;
    }

    let html = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Sudoku Benchmark - Neon</title>
    <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&display=swap" rel="stylesheet">
    <style>
        :root {
            --bg: #0d0d12;
            --surface: #16161e;
            --primary: #00ff9d;
            --secondary: #00b8ff;
            --text: #e0e0e0;
            --muted: #5c5c66;
            --border: #2a2a35;
        }
        body {
            font-family: 'JetBrains Mono', monospace;
            background-color: var(--bg);
            color: var(--text);
            margin: 0;
            padding: 40px;
        }
        h1 {
            text-align: center;
            color: var(--primary);
            text-transform: uppercase;
            letter-spacing: 2px;
            margin-bottom: 40px;
            text-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            overflow-x: auto;
        }
        table {
            width: 100%;
            border-collapse: separate;
            border-spacing: 0 4px;
        }
        th {
            text-align: center;
            padding: 15px;
            color: var(--secondary);
            font-size: 1.1em;
            text-transform: uppercase;
            border-bottom: 2px solid var(--border);
        }
        td {
            background-color: var(--surface);
            padding: 15px;
            border-top: 1px solid var(--border);
            border-bottom: 1px solid var(--border);
            transition: transform 0.2s, box-shadow 0.2s;
        }
        tr:hover td {
            background-color: #1c1c26;
            border-color: var(--secondary);
            transform: scale(1.01);
            box-shadow: 0 0 15px rgba(0, 184, 255, 0.1);
            z-index: 10;
            position: relative;
        }
        td:hover {
            background-color: #252530 !important;
            border: 1px solid var(--primary) !important;
            z-index: 30 !important;
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.3) !important;
        }
        .active-row td {
            background-color: #1c1c26;
            border-color: var(--primary);
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.2);
            z-index: 20;
            position: relative;
            animation: glow 1.5s infinite alternate;
        }
        @keyframes glow {
            from { box-shadow: 0 0 10px rgba(0, 255, 157, 0.1); }
            to { box-shadow: 0 0 20px rgba(0, 255, 157, 0.3); }
        }
        .lang-col {
            font-weight: bold;
            color: var(--primary);
            border-left: 3px solid transparent;
            cursor: pointer;
            text-decoration: underline;
            text-decoration-style: dotted;
            text-decoration-color: var(--secondary);
        }
        .lang-col:hover {
            border-left-color: var(--primary);
        }
        .lang-year {
            font-size: 0.7em;
            color: var(--muted);
            font-weight: normal;
            text-decoration: none;
            margin-top: 2px;
        }
        .active-row .lang-col {
            border-left-color: var(--primary);
        }
        .cell-content {
            display: flex;
            flex-direction: column;
            gap: 4px;
        }
        .time {
            font-size: 1.1em;
            font-weight: bold;
            color: #fff;
        }
        .meta {
            font-size: 0.75em;
            color: var(--muted);
            display: flex;
            gap: 10px;
        }
        .meta span {
            display: inline-block;
        }
        .total-time {
            font-size: 1.2em;
            color: var(--secondary);
            font-weight: bold;
        }
        .placeholder-row td {
            background-color: #3d0d0d !important;
            border-color: #ff4444;
        }
        .placeholder-row:hover td {
            background-color: #4d1111 !important;
            box-shadow: 0 0 15px rgba(255, 68, 68, 0.2);
        }
        .placeholder-row .lang-col {
            color: #ff4444;
        }
        .suspect td {
            /* border-top: 1px dashed #ffcc00; */
            /* border-bottom: 1px dashed #ffcc00; */
        }
        .suspect .lang-col {
            color: #ffcc00;
        }
        .suspect:hover td {
            box-shadow: 0 0 15px rgba(255, 204, 0, 0.2);
        }
        
        .imposter {
            color: #ff4444;
            font-weight: bold;
            text-decoration: underline;
            text-decoration-style: wavy;
        }
        
        /* Fastest/Slowest Highlights */
        .fastest-row td {
            border-top: 1px solid var(--primary);
            border-bottom: 1px solid var(--primary);
            background: rgba(0, 255, 157, 0.05);
        }
        .fastest-row .lang-col::after {
            content: " 👑";
            font-size: 0.8em;
        }
        .slowest-row td {
            border-top: 1px solid var(--danger);
            border-bottom: 1px solid var(--danger);
            background: rgba(255, 68, 68, 0.05);
        }
        .slowest-row .lang-col::after {
            content: " 🐢";
            font-size: 0.8em;
        }
        
        /* Tooltip */
        #tooltip {
            position: fixed;
            display: none;
            background: rgba(13, 13, 18, 0.95);
            border: 1px solid var(--primary);
            padding: 15px;
            color: var(--primary);
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            max-width: 300px;
            z-index: 1000;
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.2);
            pointer-events: none;
            backdrop-filter: blur(5px);
        }
        #tooltip::before {
            content: ">> SYSTEM MSG";
            display: block;
            font-size: 0.7em;
            color: var(--muted);
            margin-bottom: 5px;
            border-bottom: 1px solid var(--border);
            padding-bottom: 3px;
        }

        /* Controls */
        .controls {
            margin-bottom: 20px;
            display: flex;
            gap: 10px;
            justify-content: center;
        }
        .btn {
            background: var(--surface);
            border: 1px solid var(--primary);
            color: var(--primary);
            padding: 8px 16px;
            font-family: 'JetBrains Mono', monospace;
            cursor: pointer;
            transition: all 0.2s;
            text-transform: uppercase;
            font-size: 0.8em;
        }
        .btn:hover {
            background: var(--primary);
            color: var(--bg);
            box-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
        }
        .btn.active {
            background: var(--primary);
            color: var(--bg);
        }

        /* Modal */
        .modal-overlay {
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.8);
            z-index: 2000;
            backdrop-filter: blur(5px);
            justify-content: center;
            align-items: center;
        }
        .modal-content {
            background: var(--surface);
            border: 1px solid var(--primary);
            padding: 30px;
            border-radius: 8px;
            max-width: 500px;
            width: 90%;
            position: relative;
            box-shadow: 0 0 30px rgba(0, 255, 157, 0.2);
            text-align: center;
        }
        .modal-close {
            position: absolute;
            top: 10px;
            right: 15px;
            color: var(--muted);
            cursor: pointer;
            font-size: 1.5em;
        }
        .modal-close:hover {
            color: var(--primary);
        }
        .modal-image {
            width: 150px;
            height: 150px;
            border-radius: 50%;
            object-fit: cover;
            border: 3px solid var(--primary);
            margin-bottom: 20px;
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.3);
        }
        .modal-title {
            font-size: 2em;
            color: var(--primary);
            margin-bottom: 10px;
        }
        .modal-subtitle {
            font-size: 0.9em;
            color: var(--secondary);
            margin-bottom: 20px;
            text-transform: uppercase;
            letter-spacing: 1px;
        }
        .modal-desc {
            color: var(--text);
            line-height: 1.6;
            margin-bottom: 20px;
        }
        .lang-col {
            cursor: pointer;
            text-decoration: underline;
            text-decoration-style: dotted;
            text-decoration-color: var(--muted);
        }
        .lang-col:hover {
            color: #fff;
            text-decoration-color: var(--primary);
        }
        
        /* Matrix Sort Buttons */
        .sort-group {
            display: flex;
            gap: 2px;
            margin-top: 5px;
            justify-content: center;
        }
        .sort-btn {
            background: #2a2a35;
            border: none;
            color: #888;
            font-size: 0.7em;
            padding: 2px 6px;
            cursor: pointer;
            border-radius: 3px;
            font-family: 'JetBrains Mono', monospace;
        }
        .sort-btn:hover {
            background: var(--primary);
            color: var(--bg);
        }
        
        /* Rotation for Descending Sort */
        .rotate-180 {
            transform: rotate(180deg);
            display: inline-block;
        }
        .sort-btn, .btn {
            transition: transform 0.3s, background 0.2s, color 0.2s;
        }
        
        .top-bar {
            display: flex;
            justify-content: space-between;
            align-items: center;
            gap: 20px;
            margin-bottom: 20px;
            flex-wrap: wrap;
            position: relative;
        }
        .solver-counter {
            font-size: 2.5em;
            font-weight: bold;
            color: var(--primary);
            text-shadow: 0 0 15px rgba(0, 255, 157, 0.4);
            font-family: 'JetBrains Mono', monospace;
            /* Positioned in header now */
        }
        .mismatch-counter {
            font-size: 1.2em;
            font-weight: bold;
            color: #ff4444;
            text-shadow: 0 0 10px rgba(255, 68, 68, 0.4);
            font-family: 'JetBrains Mono', monospace;
            margin-right: 20px;
        }
        .header-counters {
            position: absolute;
            top: 20px;
            right: 20px;
            display: flex;
            flex-direction: column;
            align-items: flex-end;
            z-index: 100;
            background: rgba(13, 13, 18, 0.8);
            padding: 10px 20px;
            border: 1px solid var(--primary);
            border-radius: 4px;
        }
        .personality-intro {
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            color: var(--secondary);
            max-width: 400px;
            text-align: right;
            border-right: 2px solid var(--border);
            padding-right: 20px;
            line-height: 1.4;
            font-style: italic;
        }
        
        /* Score Styling */
        .score-fast {
            color: var(--primary);
            font-weight: bold;
        }
        .score-slow {
            color: var(--danger);
        }
        .total-score {
            font-size: 1.2em;
            font-weight: bold;
            color: var(--primary);
            text-align: right;
        }

        
        /* Toggle Button */
        .toggle-btn {
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .toggle-icon {
            width: 12px;
            height: 12px;
            border-radius: 50%;
            background: #333;
            transition: background 0.2s;
        }
        .btn.active .toggle-icon {
            background: var(--primary);
            box-shadow: 0 0 5px var(--primary);
        }
        
        /* Iteration Mismatch Highlight */
        .mismatch-iterations td {
            background-color: rgba(255, 165, 0, 0.3) !important;
        }
        .modal-location, .modal-benefits {
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            color: var(--secondary);
            margin-top: 10px;
            line-height: 1.4;
        }
        .modal-benefits {
            color: var(--primary);
            font-style: italic;
        }
        
        /* Matrix Screensaver */
        #matrix-canvas {
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            z-index: 10;
            display: none; /* Hidden by default */
            border-radius: 8px;
            pointer-events: none; /* Let clicks pass through if needed, though we hide chart anyway */
        }
    </style>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <div id="tooltip"></div>
    
    <!-- Modal -->
    <div id="langModal" class="modal-overlay" onclick="closeModal(event)">
        <div class="modal-content">
            <span class="modal-close" onclick="closeModal(event)">&times;</span>
            <img id="modalImg" class="modal-image" src="" alt="Creator">
            <div id="modalTitle" class="modal-title">Language</div>
            <div id="modalSubtitle" class="modal-subtitle">Creator • Date</div>
            <div id="modalLocation" class="modal-location">Location</div>
            <div id="modalBenefits" class="modal-benefits">Benefits</div>
            <div id="modalDesc" class="modal-desc">Description goes here.</div>
        </div>
    </div>

    <!-- Methodology Modal -->
    <div id="methodModal" class="modal-overlay" onclick="closeMethodology(event)">
        <div class="modal-content" style="text-align: left;">
            <span class="modal-close" onclick="closeMethodology(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Scoring Methodology</div>
            <div class="modal-desc">
                <p>The <strong>Total Score</strong> is a normalized metric designed to compare performance against a standard baseline.</p>
                
                <h3 style="color: var(--secondary);">The Baseline: C</h3>
                <p>The <strong>C</strong> implementation is used as the reference standard (1.0) because it represents a highly optimized, low-level approach.</p>
                
                <h3 style="color: var(--secondary);">The Formula</h3>
                <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
                    Score = (Solver Time) / (C Time)
                </div>
                
                <h3 style="color: var(--secondary);">Interpretation</h3>
                <ul style="list-style: none; padding: 0;">
                    <li style="margin-bottom: 8px;"><strong style="color: var(--primary);">1.0</strong> : Parity. Exact same speed as C.</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #ff0055;">&gt; 1.0</strong> : Slower. (e.g., 2.0 is 2x slower).</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #00b8ff;">&lt; 1.0</strong> : Faster. (e.g., 0.5 is 2x faster).</li>
                </ul>
                <p style="font-size: 0.9em; color: var(--muted); text-align: center; margin-top: 20px;"><em>Lower scores are better.</em></p>
            </div>
        </div>
    </div>

    <h1>Sudoku Benchmark Results</h1>
    
    <div class="header-counters">
        <div class="solver-counter">SOLVER ${metrics.length} OF ${metrics.length}</div>
        ${mismatchCount > 0 ? `<div class="mismatch-counter">MISMATCHES: ${mismatchCount}</div>` : ''}
    </div>
    
    <div style="width: 95%; margin: 0 auto 40px auto; background: #16161e; padding: 20px; border-radius: 8px; border: 1px solid #2a2a35; height: 500px; position: relative;">
        <div id="d3-chart-container" style="width: 100%; height: 100%;"></div>
        <canvas id="matrix-canvas"></canvas>
    </div>
    
    <div class="top-bar">
        <!-- Removed solver-counter from here -->
        <div id="personality-intro" class="personality-intro">
            Welcome to the Sudoku Benchmark. Click on any language name for creator details. Use the controls to sort data and analyze performance metrics across different languages.
        </div>
        <div class="controls">
            <input type="text" id="search-input" class="btn" placeholder="Search Language..." onkeyup="filterLanguages()" style="cursor: text; width: 150px;">
            <select id="personality-selector" class="btn" onchange="changePersonality()">
                <option value="Standard">Standard</option>
                <option value="Neuromancer">Neuromancer</option>
                <option value="The Jockey">The Jockey</option>
                <option value="The Professor">The Professor</option>
                <option value="The Surfer">The Surfer</option>
            </select>
            <button class="btn" onclick="sortRows('lang', this)">Name</button>
            <button class="btn active" onclick="sortRows('time', this)">Time (Fastest)</button>
            <button class="btn" onclick="sortRows('mem', this)">Memory (Highest)</button>
            <button class="btn" onclick="sortRows('iters', this)">Iterations</button>
            <button class="btn" onclick="sortRows('score', this)">Total Score</button>
            <button class="btn toggle-btn" onclick="toggleImposters()" id="btn-imposters">
                <span class="toggle-icon"></span> Hide Imposters
            </button>
            <button class="btn" onclick="showMethodology()">Methodology</button>
        </div>
    </div>

    <div class="container">
        <table>
            <thead>
                <tr>
                    <th>
                        Language
                        <div class="sort-group">
                            <button class="sort-btn" onclick="sortRows('lang', this)" title="Sort by Name">N</button>
                            <button class="sort-btn" onclick="sortRows('year', this)" title="Sort by Year">Y</button>
                        </div>
                    </th>
                    <th>
                        Score
                        <div class="sort-group">
                            <button class="sort-btn" onclick="sortRows('score', this)" title="Sort by Score">S</button>
                        </div>
                    </th>
`;

    for (let i = 0; i < maxMatrices; i++) {
        html += `<th>
            Matrix ${i + 1}
            <div class="sort-group">
                <button class="sort-btn" onclick="sortMatrix(${i}, 'time', this)" title="Sort by Time">S</button>
                <button class="sort-btn" onclick="sortMatrix(${i}, 'iters', this)" title="Sort by Iterations">I</button>
                <button class="sort-btn" onclick="sortMatrix(${i}, 'mem', this)" title="Sort by Memory">M</button>
                <button class="sort-btn" onclick="sortMatrix(${i}, 'score', this)" title="Sort by Score">Sc</button>
            </div>
        </th>`;
    }

    html += `<th>
        <span>Total Time</span>
        <div class="sort-group">
            <button class="sort-btn" onclick="sortRows('time', this)" title="Sort by Total Time">S</button>
        </div>
    </th>
    </tr></thead><tbody>`;

    for (const m of sortedMetrics) {
        const lang = m.solver;
        const times = m.results.map(r => r.time);
        const iters = m.results.map(r => r.iterations);
        const mems = m.results.map(r => r.memory);

        const totalTime = times.reduce((a, b) => a + b, 0);
        const totalIters = iters.reduce((a, b) => a + b, 0);
        const maxMem = mems.length > 0 ? Math.max(...mems) : 0;

        // Efficiency Score: Memory (MB) / Seconds
        const memMbTotal = maxMem / 1024 / 1024;
        const efficiencyScore = totalTime > 0 ? memMbTotal / totalTime : 0;

        // Composite Score (vs C)
        // Score = (TimeRatio + MemRatio + CpuRatio) / 3

        const totalCpu = m.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0);

        const timeRatio = (cTotalTime > 0) ? (totalTime / cTotalTime) : 0;
        const memRatio = (cTotalMem > 0) ? (maxMem / cTotalMem) : 0;
        const cpuRatio = (cTotalCpu > 0) ? (totalCpu / cTotalCpu) : 0;

        const normalizedScore = (timeRatio + memRatio + cpuRatio) / 3;

        // Find min/max for highlighting
        const minTime = Math.min(...sortedMetrics.map(m => m.results.reduce((a, b) => a + b.time, 0)));
        const maxTime = Math.max(...sortedMetrics.map(m => m.results.reduce((a, b) => a + b.time, 0)));

        const isFastest = totalTime === minTime;
        const isSlowest = totalTime === maxTime;

        // Suspect Logic
        const isSuspect = m.results.length !== maxMatrices;

        // Iteration Mismatch Logic
        const cTotalIters = cMetrics ? cMetrics.results.reduce((a, b) => a + b.iterations, 0) : 0;
        const isMismatch = m.solver !== 'C' && cTotalIters > 0 && totalIters !== cTotalIters;

        let rowClass = "";
        if (isSuspect) rowClass += " suspect";
        if (isMismatch) rowClass += " mismatch-iterations";

        // Quote
        const quote = (personalities['Standard'] as any)[lang] || "A mystery wrapped in code.";
        const safeQuote = quote.replace(/'/g, "&apos;") + ` Efficiency: ${efficiencyScore.toFixed(2)} MB/s`;

        // Metadata
        const meta = languageMetadata[lang] || {};
        const year = meta.date || "0000";
        const displayName = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
        const historyText = (languageHistories[lang] || "Unknown.").replace(/'/g, "&apos;");
        const logoUrl = meta.logo || meta.image;

        // Data Attributes
        let matrixDataAttrs = "";
        for (let i = 0; i < maxMatrices; i++) {
            const r = m.results[i];
            const t = r ? r.time : 999999;
            const it = r ? r.iterations : -1;
            const mem = r ? r.memory : -1;

            let score = 0;
            if (r && cTimes[i] && cTimes[i] > 0) {
                score = t / cTimes[i];
            }

            matrixDataAttrs += ` data-m${i}-time='${t}' data-m${i}-iters='${it}' data-m${i}-mem='${mem}' data-m${i}-score='${score.toFixed(2)}'`;
        }

        html += `<tr class="${rowClass} ${isFastest ? 'fastest-row' : ''} ${isSlowest ? 'slowest-row' : ''}" 
            data-lang="${lang}" 
            data-year="${year}" 
            data-time="${totalTime.toFixed(6)}" 
            data-iters="${totalIters}" 
            data-mem="${maxMem}" 
            data-score="${normalizedScore.toFixed(2)}"
            data-score-breakdown="Time: ${timeRatio.toFixed(2)}x | Mem: ${memRatio.toFixed(2)}x | CPU: ${cpuRatio.toFixed(2)}x"
            data-quote="${quote}" data-history='${historyText}' ${matrixDataAttrs}>
            <td class='lang-col'>
                ${logoUrl ? `<img src="${logoUrl}" alt="${displayName}" style="width: 24px; height: 24px; margin-right: 8px; vertical-align: middle; object-fit: contain;">` : ''}
                <div style="display: inline-block; vertical-align: middle;">
                    <div>${displayName}</div>
                    <div class='lang-year'>${year}</div>
                </div>
            </td>
            <td class="score-col">
                <div class="total-score" style="text-align: center; color: ${normalizedScore <= 1.0 ? 'var(--primary)' : '#ff0055'};">
                    ${normalizedScore.toFixed(2)}
                </div>
            </td>`;

        for (let i = 0; i < maxMatrices; i++) {
            const r = m.results[i];
            if (r) {
                const memMb = r.memory / 1024 / 1024;

                let scoreDisplay = "";
                if (cTimes[i] && cTimes[i] > 0) {
                    const scoreVal = r.time / cTimes[i];
                    scoreDisplay = `${scoreVal.toFixed(2)}x`;
                }

                html += `<td class="matrix-cell" data-matrix-index="${i}">
                    <div class="cell-content">

                        <div class="time" title="Wall Clock Time">${r.time.toFixed(5)}s</div>
                        <div class="meta">
                            ${(() => {
                        const cRes = cMetrics?.results.find(res => res.matrix === r.matrix);
                        const cIterations = cRes ? cRes.iterations : null;
                        const isImposter = m.solver !== 'C' && cIterations !== null && r.iterations !== cIterations;
                        return `<span title="Iterations" class="${isImposter ? 'imposter' : ''}">#${r.iterations}</span>`;
                    })()}
                            <span title="Memory">${memMb.toFixed(1)}M</span>
                        </div>
                    </div>
                </td>`;
            } else {
                html += `<td class="matrix-cell" data-matrix-index="${i}"><span style='color: #333'>-</span></td>`;
            }
        }

        html += `<td class='total-time'><div style='display:flex;flex-direction:column;align-items:flex-end;'><div>${totalTime.toFixed(4)}s</div><div style='font-size:0.6em;color:#5c5c66;'>${totalIters.toLocaleString()} iters</div></div></td></tr>`;
    }

    html += `
        </tbody></table></div>
        <script>
            const personalities = ${JSON.stringify(personalities)};
            const languageMetadata = ${JSON.stringify(languageMetadata)};
            const methodologyTexts = ${JSON.stringify(methodologyTexts)};
            
            let currentSort = { metric: 'time', dir: 1 }; // 1 = Asc, -1 = Desc

            // Search Logic
            function filterLanguages() {
                const input = document.getElementById('search-input');
                const filter = input.value.toUpperCase();
                const tbody = document.querySelector('tbody');
                const rows = tbody.getElementsByTagName('tr');

                for (let i = 0; i < rows.length; i++) {
                    const row = rows[i];
                    const lang = row.getAttribute('data-lang');
                    if (lang) {
                        if (lang.toUpperCase().indexOf(filter) > -1) {
                            row.style.display = "";
                        } else {
                            row.style.display = "none";
                        }
                    }
                }
            }

            // Sorting Logic
            function sortRows(metric, btn) {
                const tbody = document.querySelector('tbody');
                const rows = Array.from(tbody.querySelectorAll('tr'));
                
                // Toggle direction
                if (currentSort.metric === metric) {
                    currentSort.dir *= -1;
                } else {
                    currentSort.metric = metric;
                    currentSort.dir = 1;
                }
                
                // Update buttons
                document.querySelectorAll('.btn, .sort-btn').forEach(b => {
                    b.classList.remove('active');
                    b.classList.remove('rotate-180');
                });
                
                if (btn) {
                    btn.classList.add('active');
                    if (currentSort.dir === -1) {
                        btn.classList.add('rotate-180');
                    }
                }

                rows.sort((a, b) => {
                    const aVal = a.getAttribute('data-' + metric);
                    const bVal = b.getAttribute('data-' + metric);
                    
                    if (metric === 'lang') {
                        return aVal.localeCompare(bVal) * currentSort.dir;
                    } else if (metric === 'year') {
                        return (parseInt(aVal) - parseInt(bVal)) * currentSort.dir;
                    } else {
                        return (parseFloat(aVal) - parseFloat(bVal)) * currentSort.dir;
                    }
                });

                rows.forEach(row => tbody.appendChild(row));
            }
            
            function sortMatrix(index, metric, btn) {
                const tbody = document.querySelector('tbody');
                const rows = Array.from(tbody.querySelectorAll('tr'));
                const attr = 'data-m' + index + '-' + metric;
                const fullMetric = 'm' + index + '_' + metric;
                
                if (currentSort.metric === fullMetric) {
                    currentSort.dir *= -1;
                } else {
                    currentSort.metric = fullMetric;
                    currentSort.dir = metric === 'time' || metric === 'score' ? 1 : -1;
                }

                // Update buttons
                document.querySelectorAll('.btn, .sort-btn').forEach(b => {
                    b.classList.remove('active');
                    b.classList.remove('rotate-180');
                });
                
                if (btn) {
                    btn.classList.add('active');
                    if (currentSort.dir === -1) {
                        btn.classList.add('rotate-180');
                    }
                }

                rows.sort((a, b) => {
                    const aVal = parseFloat(a.getAttribute(attr));
                    const bVal = parseFloat(b.getAttribute(attr));
                    return (aVal - bVal) * currentSort.dir;
                });

                rows.forEach(row => tbody.appendChild(row));
            }
            
            // Imposter Filter
            function toggleImposters() {
                const btn = document.getElementById('btn-imposters');
                const rows = document.querySelectorAll('.placeholder-row');
                const isHidden = btn.classList.contains('active');
                
                if (isHidden) {
                    rows.forEach(r => r.style.display = 'table-row');
                    btn.classList.remove('active');
                    btn.innerHTML = '<span class="toggle-icon"></span> Hide Imposters';
                } else {
                    rows.forEach(r => r.style.display = 'none');
                    btn.classList.add('active');
                    btn.innerHTML = '<span class="toggle-icon"></span> Show Imposters';
                }
            }

            // Personality Selector
            function changePersonality() {
                const selector = document.getElementById('personality-selector');
                const persona = selector.value;
                const intro = document.getElementById('personality-intro');
                
                // Update Intro Text
                if (persona === "Neuromancer") intro.innerText = "The sky above the port was the color of television, tuned to a dead channel.";
                else if (persona === "The Jockey") intro.innerText = "And they're off! The race is on!";
                else if (persona === "The Professor") intro.innerText = "Let us analyze the computational complexity.";
                else if (persona === "The Surfer") intro.innerText = "Catch the wave of data, dude.";
                else intro.innerText = "Welcome to the Sudoku Benchmark. Click on any language name for creator details.";

                // Update Tooltips
                const rows = document.querySelectorAll('tbody tr');
                rows.forEach(row => {
                    const lang = row.getAttribute('data-lang');
                    const quotes = personalities[persona] || personalities['Standard'];
                    let quote = quotes[lang] || quotes['default'] || "Unknown.";
                    
                    // Append Efficiency
                    const score = row.getAttribute('data-score');
                    quote += " Efficiency: " + parseFloat(score).toFixed(2) + " MB/s";
                    
                    row.setAttribute('data-quote', quote);
                });
                // Update Methodology Modal
                const methodDesc = document.querySelector('#methodModal .modal-desc');
                if (methodDesc) {
                    methodDesc.innerHTML = methodologyTexts[persona] || methodologyTexts['Standard'];
                }

            }

            // Tooltip Logic
            const tooltip = document.getElementById('tooltip');
            
            // Attach to all cells
            // Attach to all cells
            // Attach to all cells
            
            document.querySelectorAll('tbody td').forEach(cell => {
                cell.addEventListener('mousemove', (e) => {
                    const row = cell.parentElement;
                    const lang = row.getAttribute('data-lang');
                    
                    let content = "";
                    
                    if (cell.classList.contains('lang-col')) {
                        // Language Cell -> Show History
                        const history = row.getAttribute('data-history');
                        if (history) {
                            content = '<strong style="color: var(--primary)">' + lang + '</strong><br>' +
                                '<div style="max-width: 250px; white-space: normal; margin-top: 5px;">' + history + '</div>';
                        }
                    } else if (cell.classList.contains('matrix-cell')) {
                        // Matrix Cell -> Detailed Metrics
                        const matrixIdx = parseInt(cell.getAttribute('data-matrix-index'));
                        const time = row.getAttribute('data-m' + matrixIdx + '-time');
                        const iters = row.getAttribute('data-m' + matrixIdx + '-iters');
                        const mem = row.getAttribute('data-m' + matrixIdx + '-mem');
                        const score = row.getAttribute('data-m' + matrixIdx + '-score');
                        
                        if (time && time !== '999999') {
                            const memMb = (parseFloat(mem) / 1024 / 1024).toFixed(1);
                            content = '<strong style="color: var(--primary)">Matrix ' + (matrixIdx + 1) + '</strong><br>' +
                                '<span style="color: var(--secondary)">' + lang + '</span><br>' +
                                '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                                'Time: <span style="color: #fff">' + parseFloat(time).toFixed(5) + 's</span><br>' +
                                'Score: <span style="color: ' + (parseFloat(score) <= 1 ? 'var(--primary)' : '#ff0055') + '">' + score + 'x</span><br>' +
                                'Iters: ' + parseInt(iters).toLocaleString() + '<br>' +
                                'Mem: ' + memMb + ' MB';
                        } else {
                            content = row.getAttribute('data-quote');
                        }
                    } else if (cell.classList.contains('score-col')) {
                        // Score Cell -> Show Breakdown + Quote
                        const breakdown = row.getAttribute('data-score-breakdown');
                        const quote = row.getAttribute('data-quote');
                        content = '<strong style="color: var(--primary)">Composite Score</strong><br>' +
                                  '<span style="font-size: 0.8em; color: var(--secondary)">' + breakdown + '</span><br>' +
                                  '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                                  quote;
                    }

                    if (content) {
                        const tooltip = document.getElementById('tooltip');
                        tooltip.style.display = 'block';
                        tooltip.style.left = (e.clientX + 15) + 'px';
                        tooltip.style.top = (e.clientY + 15) + 'px';
                        tooltip.innerHTML = content;
                    }
                });
                
                cell.addEventListener('mouseleave', () => {
                    const tooltip = document.getElementById('tooltip');
                    tooltip.style.display = 'none';
                });
            });
            
            // Add click handler to language cells to show modal
            document.querySelectorAll('.lang-col').forEach(cell => {
                cell.addEventListener('click', () => {
                    const row = cell.parentElement;
                    const lang = row.getAttribute('data-lang');
                    if (lang) {
                        showLanguageDetails(lang);
                    }
                });
                cell.style.cursor = 'pointer';  // Make it obvious it's clickable
            });
            // Modal Logic
            function showLanguageDetails(lang) {
                const modal = document.getElementById('langModal');
                const meta = languageMetadata[lang];
                if (!meta) return;

                document.getElementById('modalImg').src = meta.image;
                const displayName = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
                document.getElementById('modalTitle').innerText = displayName;
                document.getElementById('modalSubtitle').innerText = meta.creator + " • " + meta.date;
                document.getElementById('modalLocation').innerText = "📍 " + (meta.location || "Unknown Location");
                document.getElementById('modalBenefits').innerText = "✨ " + (meta.benefits || "Unknown Benefits");
                document.getElementById('modalDesc').innerText = meta.description;
                
                modal.style.display = 'flex';
            }

            function closeModal(event) {
                if (event.target.id === 'langModal' || event.target.classList.contains('modal-close')) {
                    document.getElementById('langModal').style.display = 'none';
                }
            }

            function showMethodology() {
                document.getElementById('methodModal').style.display = 'flex';
            }

            function closeMethodology(event) {
                if (event.target.id === 'methodModal' || event.target.classList.contains('modal-close')) {
                    document.getElementById('methodModal').style.display = 'none';
                }
            }

            // Initialize
            toggleImposters(); // Default hide
            
            // --- D3.js Chart Implementation ---
            (function() {
                let data = ${JSON.stringify(metrics)};
                
                // Filter out mismatched solvers
                const cSolver = data.find(s => s.solver === 'C');
                if (cSolver) {
                    const cIters = {};
                    cSolver.results.forEach(r => cIters[r.matrix] = r.iterations);
                    
                    data = data.filter(s => {
                        if (s.solver === 'C') return true;
                        // Check if any result mismatches C's iterations
                        const hasMismatch = s.results.some(r => {
                            const expected = cIters[r.matrix];
                            return expected && r.iterations !== expected;
                        });
                        return !hasMismatch;
                    });
                }
                // Calculate min/max from individual matrix times, not totals
                const allTimes = data.flatMap(m => m.results.map(r => r.time)).filter(t => t > 0);
                const minTime = ${Math.min(...metrics.flatMap(m => m.results.map(r => r.time)).filter(t => t > 0))};
                const maxTime = ${Math.max(...metrics.flatMap(m => m.results.map(r => r.time)))};
                const container = document.getElementById('d3-chart-container');
                const width = container.clientWidth;
                const height = container.clientHeight;
                const margin = { top: 20, right: 120, bottom: 50, left: 60 };
                
                const svg = d3.select("#d3-chart-container")
                    .append("svg")
                    .attr("width", width)
                    .attr("height", height)
                    .append("g")
                    .attr("transform", \`translate(\${margin.left},\${margin.top})\`);
                    
                const chartWidth = width - margin.left - margin.right;
                const chartHeight = height - margin.top - margin.bottom;
                
                // Prepare Data
                // We want to plot Time vs Matrix for each solver
                const matrices = ["1.matrix", "2.matrix", "3.matrix", "4.matrix", "5.matrix", "6.matrix"];
                
                // X Axis: Matrix Names
                const x = d3.scalePoint()
                    .domain(matrices)
                    .range([0, chartWidth])
                    .padding(0.5);
                    
                svg.append("g")
                    .attr("transform", \`translate(0,\${chartHeight})\`)
                    .call(d3.axisBottom(x))
                    .selectAll("text")
                    .style("fill", "#e0e0e0")
                    .style("font-family", "JetBrains Mono");
                    
                svg.append("text")
                    .attr("text-anchor", "end")
                    .attr("x", chartWidth)
                    .attr("y", chartHeight + 40)
                    .style("fill", "#5c5c66")
                    .style("font-size", "12px")
                    .text("Matrix Input");

                // Y Axis: Time (Log Scale)
                const y = d3.scaleLog()
                    .domain([minTime, maxTime])
                    .range([chartHeight, 0]);

                svg.append("g")
                    .call(d3.axisLeft(y)
                        .ticks(5)
                        .tickFormat((d, i, nodes) => {
                            // Only show labels for major ticks (every other tick or specific values)
                            const ticks = y.ticks(5);
                            if (ticks.indexOf(d) % 2 === 0 || ticks.length <= 3) {
                                return d >= 1 ? d3.format(".1f")(d) + "s" : d3.format(".2f")(d) + "s";
                            }
                            return "";  // Hide minor tick labels
                        })
                    )
                    .selectAll("text")
                    .style("fill", "#e0e0e0")
                    .style("font-family", "JetBrains Mono");

                svg.append("text")
                    .attr("transform", "rotate(-90)")
                    .attr("y", 0 - margin.left)
                    .attr("x", 0 - (chartHeight / 2))
                    .attr("dy", "1em")
                    .style("text-anchor", "middle")
                    .style("fill", "#5c5c66")
                    .style("font-size", "12px")
                    .text("Time (seconds) - Log Scale");
                
                
// Grid lines
svg.append("g")
    .attr("class", "grid")
    .attr("opacity", 0.1)
    .call(d3.axisLeft(y)
        .tickSize(-chartWidth)
        .tickFormat("")
    );

// Color Palette
const color = d3.scaleOrdinal()
    .domain(data.map(d => d.solver))
    .range(["#00ff9d", "#00b8ff", "#ff0055", "#ffcc00", "#bd00ff", "#00ffff"]);

// Line Generator
const line = d3.line()
    .x(d => x(d.matrix))
    .y(d => y(Math.max(d.time, minTime)));

// Draw Lines
data.forEach(solver => {
    // Filter results to match our x-axis domain
    const solverData = solver.results.filter(r => matrices.includes(r.matrix));
    const safeSolverClass = "dot-" + solver.solver.replace(/[^a-zA-Z0-9]/g, '_');

    // Path
    svg.append("path")
        .datum(solverData)
        .attr("fill", "none")
        .attr("stroke", color(solver.solver))
        .attr("stroke-width", 2)
        .attr("d", line)
        .attr("class", "line-path")
    // Dots
    svg.selectAll("." + safeSolverClass)
        .data(solverData)
        .enter().append("circle")
        .attr("class", safeSolverClass)
        .attr("cx", d => x(d.matrix))
        .attr("cy", d => y(Math.max(d.time, minTime)))
        .attr("r", 4)
        .attr("fill", "#16161e")
        .attr("stroke", color(solver.solver))
        .attr("stroke-width", 2)
        .on("mouseover", function (event, d) {
            d3.select(this).attr("r", 6).attr("fill", color(solver.solver));

            const tooltip = document.getElementById('tooltip');
            tooltip.style.display = 'block';
            tooltip.style.left = (event.clientX + 15) + 'px';
            tooltip.style.top = (event.clientY + 15) + 'px';
            tooltip.innerHTML = \`
                                <strong style="color:\${color(solver.solver)}">\${solver.solver}</strong><br>
                                Matrix: \${d.matrix}<br>
                                Time: \${d.time.toFixed(6)}s<br>
                                Iters: \${d.iterations}
                            \`;
                        })
                        .on("mouseout", function() {
                            d3.select(this).attr("r", 4).attr("fill", "#16161e");
                            document.getElementById('tooltip').style.display = 'none';
                        });
                        
                    // Label at the end of the line
                    const lastPoint = solverData[solverData.length - 1];
                    if (lastPoint) {
                        svg.append("text")
                            .attr("x", x(lastPoint.matrix) + 10)
                            .attr("y", y(Math.max(lastPoint.time, minTime)))
                            .attr("dy", "0.35em")
                            .style("fill", color(solver.solver))
                            .style("font-size", "12px")
                            .style("font-weight", "bold")
                            .text(solver.solver);
                    }
                });
            })();

            // Matrix Screensaver Logic
            (function() {
                const canvas = document.getElementById('matrix-canvas');
                const ctx = canvas.getContext('2d');
                const container = canvas.parentElement;
                const chartContainer = document.getElementById('d3-chart-container');
                
                let width, height;
                let columns;
                let drops = [];
                let animationId;
                let active = false;
                
                // Matrix characters (Katakana + Latin)
                const chars = 'アァカサタナハマヤャラワガザダバパイィキシチニヒミリヰギジヂビピウゥクスツヌフムユュルグズブヅプエェケセテネヘメレヱゲゼデベペオォコソトノホモヨョロヲゴゾドボポヴッン0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
                
                function resize() {
                    width = container.clientWidth;
                    height = container.clientHeight;
                    canvas.width = width;
                    canvas.height = height;
                    
                    const fontSize = 14;
                    columns = Math.ceil(width / fontSize);
                    
                    // Initialize drops
                    drops = [];
                    for (let i = 0; i < columns; i++) {
                        drops[i] = Math.random() * -100; // Start above screen
                    }
                }
                
                function draw() {
                    // Black background with opacity for trail effect
                    ctx.fillStyle = 'rgba(0, 0, 0, 0.05)';
                    ctx.fillRect(0, 0, width, height);
                    
                    ctx.fillStyle = '#0F0'; // Green text
                    ctx.font = '14px monospace';
                    
                    for (let i = 0; i < drops.length; i++) {
                        const text = chars.charAt(Math.floor(Math.random() * chars.length));
                        const x = i * 14;
                        const y = drops[i] * 14;
                        
                        ctx.fillText(text, x, y);
                        
                        // Reset drop to top randomly after it has crossed the screen
                        if (y > height && Math.random() > 0.975) {
                            drops[i] = 0;
                        }
                        
                        drops[i]++;
                    }
                    
                    if (active) {
                        animationId = requestAnimationFrame(draw);
                    }
                }
                
                function startScreensaver() {
                    if (active) return;
                    active = true;
                    
                    // Hide chart, show canvas
                    chartContainer.style.visibility = 'hidden'; // Use visibility to keep layout
                    canvas.style.display = 'block';
                    
                    resize();
                    draw();
                }
                
                function stopScreensaver() {
                    if (!active) return;
                    active = false;
                    cancelAnimationFrame(animationId);
                    
                    // Show chart, hide canvas
                    chartContainer.style.visibility = 'visible';
                    canvas.style.display = 'none';
                }
                
                // Idle Timer
                let idleTime = 0;
                const idleLimit = 5 * 60 * 1000; // 5 minutes
                // const idleLimit = 5000; // 5 seconds for testing
                
                let idleInterval = setInterval(() => {
                    idleTime += 1000;
                    if (idleTime >= idleLimit) {
                        startScreensaver();
                    }
                }, 1000);
                
                function resetTimer() {
                    idleTime = 0;
                    if (active) {
                        stopScreensaver();
                    }
                }
                
                // Events to reset timer
                window.onload = resetTimer;
                document.onmousemove = resetTimer;
                document.onkeypress = resetTimer;
                document.onclick = resetTimer;
                document.onscroll = resetTimer;
                
                // Handle resize
                window.addEventListener('resize', () => {
                    if (active) resize();
                });
            })();
        </script>
</body>
</html>
`;
    return html;
}

async function main() {
    const rootDir = path.resolve(__dirname, '..');
    const metricsFile = path.join(rootDir, 'metrics.json');
    const htmlFile = path.join(rootDir, 'benchmark_report.html');

    const generateOnly = process.argv.includes('--generate-only');
    let allMetrics: SolverMetrics[] = [];

    if (generateOnly) {
        console.log("Generate-only mode: Reading existing metrics...");
        try {
            const data = await fs.readFile(metricsFile, 'utf-8');
            allMetrics = JSON.parse(data);
            console.log(`Loaded ${allMetrics.length} metrics from ${metricsFile}`);
        } catch (e) {
            console.error(`Failed to read ${metricsFile}:`, e);
            process.exit(1);
        }
    } else {
        const solverScripts = await findSolvers(rootDir);

        for (const script of solverScripts) {
            const metrics = await runSolver(script);
            if (metrics) {
                allMetrics.push(metrics);
            }
        }

        // Save metrics.json
        await fs.writeFile(metricsFile, JSON.stringify(allMetrics, null, 2));
        console.log(`Saved metrics to ${metricsFile}`);
    }

    // Generate HTML
    const htmlContent = await generateHtml(allMetrics, [], personalities, languageMetadata, methodologyTexts);
    await fs.writeFile(htmlFile, htmlContent);
    console.log(`Generated HTML report at ${htmlFile}`);

    // Capture Screenshot
    await captureScreenshot(htmlFile);
}

async function captureScreenshot(htmlFilePath: string) {
    const screenshotsDir = path.join(__dirname, 'screenshots');
    await fs.mkdir(screenshotsDir, { recursive: true });

    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const screenshotPath = path.join(screenshotsDir, `benchmark_${timestamp}.png`);

    console.log("Capturing screenshot...");
    const browser = await puppeteer.launch({ headless: true });
    const page = await browser.newPage();

    // Set viewport to 1920x1080 for high quality video frames
    await page.setViewport({ width: 1920, height: 1080 });

    await page.goto(`file://${htmlFilePath}`, { waitUntil: 'networkidle0' });

    // Wait for D3 chart to render
    try {
        await page.waitForSelector('#d3-chart-container svg', { timeout: 5000 });
    } catch (e) {
        console.warn("Chart selector not found or timed out, proceeding with screenshot anyway.");
    }

    // Modify page for screenshot: Sort by Score and Hide rows > 10
    await page.evaluate(() => {
        // 1. Sort by Score (Ascending)
        // We need to find the sort button for Score and click it, or call the function directly
        // The function is sortRows('score', btn)
        // Let's call it directly if possible, or simulate click

        // Reset sort to ensure we start clean
        // Accessing the global function defined in the HTML
        (window as any).currentSort = { metric: 'score', dir: -1 }; // Set to -1 so the next call flips it to 1 (Asc)
        (window as any).sortRows('score', null);

        // 2. Hide rows beyond top 10
        const rows = document.querySelectorAll('tbody tr');
        rows.forEach((row, index) => {
            if (index >= 10) {
                (row as HTMLElement).style.display = 'none';
            }
        });

        // 3. Hide the "Show Imposters" button and other controls to clean up UI
        const controls = document.querySelector('.controls');
        if (controls) (controls as HTMLElement).style.display = 'none';

        const personalityIntro = document.getElementById('personality-intro');
        if (personalityIntro) (personalityIntro as HTMLElement).style.display = 'none';
    });

    await page.screenshot({ path: screenshotPath, fullPage: true });

    await browser.close();
    console.log(`Screenshot saved to ${screenshotPath}`);
}

main().catch(console.error);
