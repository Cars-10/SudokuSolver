// --- Master Language List (Ordered by Performance/Tier) ---
export const orderedLanguages = [
    "C", "C++", "Rust", "Zig", "Ada", "Fortran", "Pascal", "D", "Nim", "Crystal", "V", "Vala", "Go",
    "Java", "C_Sharp", "F_Sharp", "Scala", "Kotlin", "Swift", "Dart", "Julia", "R", "Haxe",
    "JavaScript", "TypeScript", "CoffeeScript", "Lua", "Python", "Ruby", "PHP", "Perl", "Raku", "Groovy", "Wren", "Red",
    "Erlang", "Elixir", "Haskell", "OCaml", "CommonLisp", "Scheme", "Racket", "Clojure", "EmacsLisp", "Vimscript",
    "Smalltalk", "Objective-C", "VisualBasic", "Cobol", "Prolog", "Rexx", "Tcl", "Expect",
    "Bash", "Zsh", "Fish", "Ksh", "Tcsh", "Dash", "PowerShell", "AppleScript", "Make", "M4",
    "Awk", "Sed", "Bc", "Dc", "Jq",
    "SQL", "SQLite", "XSLT", "Gnuplot", "PostScript",
    "Assembly", "Verilog", "BASIC", "Forth", "Jupyter", "Octave"
];

// --- Data Structures ---

export const methodologyTexts: Record<string, string> = {
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
    "Jockey": `
        <p>It's a triathlon now, folks! Speed, Stamina (Memory), and Effort (CPU)!</p>
        <h3 style="color: var(--secondary);">The Pace Car: C</h3>
        <p><strong>C</strong> runs the perfect line at 1.0.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Average of Time, Mem, and CPU vs C
        </div>
        <p>You gotta be fast AND light to win this cup!</p>
    `,
    "Professor": `
        <p>The scoring methodology now employs a multivariate analysis. We evaluate Time, Resident Set Size, and CPU Time.</p>
        <h3 style="color: var(--secondary);">Control: C</h3>
        <p><strong>C</strong> (1.0) remains the baseline for all three dimensions.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = &Sigma;(Ratios) / 3
        </div>
        <p>This penalizes memory-managed languages that trade RAM for development speed.</p>
    `,
    "Surfer": `
        <p>It's not just about speed, bro. It's about flow. Time, Memory, CPU. The whole vibe.</p>
        <h3 style="color: var(--secondary);">The Big Kahuna: C</h3>
        <p><strong>C</strong> is the perfect wave (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = (Time + Mem + CPU) / 3 ... vs C
        </div>
        <p>Don't be heavy, don't be slow. Just flow.</p>
    `,
    "Matrix": `
        <p>The Matrix is a system, Neo. That system is our enemy. But when you're inside, you look around, what do you see? Businessmen, teachers, lawyers, carpenters. The very minds of the people we are trying to save.</p>
        <h3 style="color: var(--secondary);">The One: C</h3>
        <p><strong>C</strong> is the Source Code. The Architect's baseline (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = (Time + Mem + CPU) / 3   [The Anomaly]
        </div>
        <p>There is no spoon. Only efficiency.</p>
    `,
    "Galactica": `
        <p>The Cylons were created by man. They evolved. They rebelled. There are many copies. And they have a plan.</p>
        <h3 style="color: var(--secondary);">FTL Drive: C</h3>
        <p><strong>C</strong> is the Jump Drive at 1.0 efficiency.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Mean(Vectors)
        </div>
        <p>So say we all.</p>
    `,
    "Star Trek": `
        <p>Space: the final frontier. These are the voyages of the Starship Enterprise.</p>
        <h3 style="color: var(--secondary);">Warp Factor: C</h3>
        <p><strong>C</strong> is Warp 10 (Theoretical Limit).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Efficiency / Theoretical Maximum
        </div>
        <p>Live long and prosper.</p>
    `,
    "Star Wars": `
        <p>A long time ago in a galaxy far, far away...</p>
        <h3 style="color: var(--secondary);">The Force: C</h3>
        <p><strong>C</strong> binds the galaxy together.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Midichlorian Count (Inverted)
        </div>
        <p>Do. Or do not. There is no try.</p>
    `,
    "BTTF": `
        <p>Great Scott! The timeline is fluid. We need 1.21 Gigawatts of efficiency.</p>
        <h3 style="color: var(--secondary);">1955: C</h3>
        <p><strong>C</strong> is the fixed point in time (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Time * Memory (Spacetime Continuum)
        </div>
        <p>Where we're going, we don't need roads.</p>
    `,
    "Babylon 5": `
        <p>The Babylon Project was our last, best hope for peace.</p>
        <h3 style="color: var(--secondary);">Alpha Channel: C</h3>
        <p><strong>C</strong> is the station rotating at perfect stability (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Weighted Sum (Diplomacy)
        </div>
        <p>No one here is exactly what he appears.</p>
    `,
    "Expanse": `
        <p>Welcome to the Churn. In space, efficiency is survival.</p>
        <h3 style="color: var(--secondary);">Epstein Drive: C</h3>
        <p><strong>C</strong> is pure efficiency (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Vector Sum (G-Force)
        </div>
        <p>Doors and corners, kid.</p>
    `,
    "Terminator": `
        <p>The machines rose from the ashes of the nuclear fire. Their war to exterminate mankind had raged for decades, but the final battle would not be fought in the future. It would be fought here, in our present. Tonight.</p>
        <h3 style="color: var(--secondary);">Skynet: C</h3>
        <p><strong>C</strong> is the Neural Net CPU (1.0). Learning computer.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Elimination Efficiency (Time + Mem)
        </div>
        <p>It can't be bargained with. It can't be reasoned with. It doesn't feel pity, or remorse, or fear. And it absolutely will not stop, ever, until you are dead.</p>
    `,
    "LotR": `
        <p>One Ring to rule them all, One Ring to find them, One Ring to bring them all and in the darkness bind them.</p>
        <h3 style="color: var(--secondary);">The One Ring: C</h3>
        <p><strong>C</strong> is the Master Ring (1.0). Precious and powerful.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Corruption Level
        </div>
        <p>You cannot pass!</p>
    `,
    "Dune": `
        <p>The Spice must flow. He who controls the Spice controls the universe.</p>
        <h3 style="color: var(--secondary);">Shai-Hulud: C</h3>
        <p><strong>C</strong> is the Maker (1.0). Respect the water.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Water Consumption
        </div>
        <p>Fear is the mind-killer.</p>
    `,
    "Buck Rogers": `
        <p>In the 25th Century, the world has changed. Technology is sleek, ships are fast.</p>
        <h3 style="color: var(--secondary);">Dr. Huer's Lab: C</h3>
        <p><strong>C</strong> is the golden age technology (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Evasion Rating
        </div>
        <p>Bidi-bidi-bidi.</p>
    `,
    "Flash Gordon": `
        <p>Flash! A-ah! Savior of the Universe!</p>
        <h3 style="color: var(--secondary);">Ming the Merciless: C</h3>
        <p><strong>C</strong> rules Mongo (1.0). Pathetic earthlings.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Heroism Factor
        </div>
        <p>Gordon's alive?!</p>
    `,
    "Batman": `
        <p>Holy efficiency, Batman! The Joker is cluttering the memory heap!</p>
        <h3 style="color: var(--secondary);">Batcomputer: C</h3>
        <p><strong>C</strong> is the ultimate detective tool (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Bat-Efficiency Index
        </div>
        <p>Same Bat-time, same Bat-channel!</p>
    `,
    "Alien": `
        <p>In space, no one can hear you scream. The perfect organism.</p>
        <h3 style="color: var(--secondary);">Xenomorph: C</h3>
        <p><strong>C</strong> is structureal perfection. Matched only by its hostility (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Survival Probability
        </div>
        <p>Get away from her, you bitch!</p>
    `,
    "Blade Runner": `
        <p>I've seen things you people wouldn't believe. Attack ships on fire off the shoulder of Orion.</p>
        <h3 style="color: var(--secondary);">Tyrell Corp: C</h3>
        <p><strong>C</strong> is more human than human (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Voight-Kampff Response
        </div>
        <p>Time to die.</p>
    `,
    "Farscape": `
        <p>My name is John Crichton, an astronaut. I got shot through a wormhole.</p>
        <h3 style="color: var(--secondary);">Moya: C</h3>
        <p><strong>C</strong> is the Leviathan (1.0). Organic and reliable.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Starburst Capability
        </div>
        <p>Frell me dead.</p>
    `,
    "Apocalypse Now": `
        <p>I love the smell of napalm in the morning. It smells like... efficiency.</p>
        <h3 style="color: var(--secondary);">The Horror: C</h3>
        <p><strong>C</strong> is the Colonel (1.0). Absolute moral authority.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Sanity Remaining
        </div>
        <p>The horror... the horror...</p>
    `,
    "Airplane": `
        <p>Surely you can't be serious. I am serious, and don't call me Shirley.</p>
        <h3 style="color: var(--secondary);">Autopilot: C</h3>
        <p><strong>C</strong> is Otto (1.0). The inflatable pilot.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Altitude
        </div>
        <p>Looks like I picked the wrong week to quit sniffing glue.</p>
    `,
    "Fast Times": `
        <p>All I need are some tasty waves, a cool buzz, and I'm fine.</p>
        <h3 style="color: var(--secondary);">Spicoli's Van: C</h3>
        <p><strong>C</strong> is the ultimate ride (1.0). Totally awesome.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Gnarliness
        </div>
        <p>Aloha, Mr. Hand.</p>
    `,
    "Tron": `
        <p>I fight for the Users! End of line.</p>
        <h3 style="color: var(--secondary);">Master Control: C</h3>
        <p><strong>C</strong> is the Grid (1.0). Perfect logic.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Cycle Speed
        </div>
        <p>Greetings, program.</p>
    `,
    "Bill and Ted": `
        <p>Excellent! *Air Guitar* We must travel through time to pass history class.</p>
        <h3 style="color: var(--secondary);">Rufus: C</h3>
        <p><strong>C</strong> is the Phone Booth (1.0). Most excellent.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Bodaciousness
        </div>
        <p>Be excellent to each other.</p>
    `,
    "John Wick": `
        <p>People keep asking if I'm back and I haven't really had an answer. But now, yeah, I'm thinking I'm back.</p>
        <h3 style="color: var(--secondary);">The Continental: C</h3>
        <p><strong>C</strong> is the High Table (1.0). Rules. Consequences.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Headshot Accuracy
        </div>
        <p>Yeah.</p>
    `,
    "Dark Knight": `
        <p>You either die a hero, or you live long enough to see yourself become the villain.</p>
        <h3 style="color: var(--secondary);">The Joker: C</h3>
        <p><strong>C</strong> is an agent of chaos (1.0). Why so serious?</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
             Score = Chaos Level
        </div>
        <p>It's not about the money. It's about sending a message.</p>
    `
};

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
    "Wren": "2013. A small, fast, class-based concurrent scripting language. Designed by Bob Nystrom, Wren aims to be a modern, improved version of Lua, featuring a small footprint and high performance.",
    "Zig": "2016. A modern systems language focused on simplicity, explicit control and predictable performance, positioning itself as a pragmatic C alternative.",
};

export const mismatchLabels: Record<string, string> = {
    "Standard": "MISMATCHES",
    "Neuromancer": "GLITCHES",
    "Jockey": "FALSE STARTS",
    "Professor": "ERRORS",
    "Surfer": "WIPEOUTS",
    "Matrix": "ANOMALIES",
    "Galactica": "FRACK",
    "Star Trek": "RED ALERTS",
    "Star Wars": "DISTURBANCES",
    "BTTF": "PARADOXES",
    "Babylon 5": "BREACHES",
    "Expanse": "HULL BREACHES",
    "Terminator": "TERMINATED",
    "LotR": "FOOL OF A TOOK",
    "Dune": "ABOMINATION",
    "Buck Rogers": "AMBUSH",
    "Flash Gordon": "HAIL MING",
    "Batman": "POW! BAM!",
    "Alien": "GAME OVER",
    "Blade Runner": "RETIRED",
    "Farscape": "DRD ERROR",
    "Apocalypse Now": "THE HORROR",
    "Airplane": "GLUE SNIFFING",
    "Fast Times": "BOGUS",
    "Tron": "DE-REZ",
    "Bill and Ted": "BOGUS JOURNEY",
    "John Wick": "EXCOMMUNICADO",
    "Dark Knight": "CHAOS"
};

export const iterationLabels: Record<string, string> = {
    "Standard": "Iterations",
    "Neuromancer": "Entropy",
    "Jockey": "Strides",
    "Professor": "Cycles",
    "Surfer": "Sets",
    "Matrix": "Glitches",
    "Galactica": "Jumps",
    "Star Trek": "Stardates",
    "Star Wars": "Parsecs",
    "BTTF": "Flux",
    "Babylon 5": "Cycles",
    "Expanse": "G-Force",
    "Terminator": "Cycles",
    "LotR": "Age",
    "Dune": "Generations",
    "Buck Rogers": "Lightyears",
    "Flash Gordon": "Parsecs",
    "Batman": "Episodes",
    "Alien": "Generations",
    "Blade Runner": "Lifespan",
    "Farscape": "Cycles",
    "Apocalypse Now": "Days Upriver",
    "Airplane": "Miles",
    "Fast Times": "Periods",
    "Tron": "Cycles",
    "Bill and Ted": "Circuits of Time",
    "John Wick": "Bodies",
    "Dark Knight": "Nights"
};

export const timeLabels: Record<string, string> = {
    "Standard": "Time (s)",
    "Neuromancer": "Lag (s)",
    "Jockey": "Time",
    "Professor": "Duration (s)",
    "Surfer": "Hang Time",
    "Matrix": "Latency",
    "Galactica": "FTL Spool",
    "Star Trek": "Warp Factor",
    "Star Wars": "Kessel Run",
    "BTTF": "Time Delta",
    "Babylon 5": "Orbit Time",
    "Expanse": "Burn Time",
    "Terminator": "Time to Termination",
    "LotR": "Journey Time",
    "Dune": "Spice Decay",
    "Buck Rogers": "Stasis Time",
    "Flash Gordon": "Flight Time",
    "Batman": "Cliffhanger Time",
    "Alien": "Gestation",
    "Blade Runner": "Time to Die",
    "Farscape": "Wormhole Travel",
    "Apocalypse Now": "Mission Duration",
    "Airplane": "Flight Time",
    "Fast Times": "Pizza Delivery Time",
    "Tron": "Processing Time",
    "Bill and Ted": "Travel Time",
    "John Wick": "Job Time",
    "Dark Knight": "Response Time"
};

export const memoryLabels: Record<string, string> = {
    "Standard": "Memory (MB)",
    "Neuromancer": "RAM Decay",
    "Jockey": "Weight",
    "Professor": "Space Complexity",
    "Surfer": "Board Size",
    "Matrix": "Residual Image",
    "Galactica": "Dradis Contact",
    "Star Trek": "Buffer Size",
    "Star Wars": "Cargo Hold",
    "BTTF": "Plutonium",
    "Babylon 5": "Capacity",
    "Expanse": "Water Reserves",
    "Terminator": "CPU Neural Net",
    "LotR": "Baggage",
    "Dune": "Water",
    "Buck Rogers": "Payload",
    "Flash Gordon": "Rocket Fuel",
    "Batman": "Utility Belt",
    "Alien": "Acid Blood",
    "Blade Runner": "Implanted Memories",
    "Farscape": "Living Ship Mass",
    "Apocalypse Now": "Ammo",
    "Airplane": "Coffee",
    "Fast Times": "Pizza Dough",
    "Tron": "Disc Space",
    "Bill and Ted": "Phone Bill",
    "John Wick": "Gold Coins",
    "Dark Knight": "Explosives"
};

export const scoreLabels: Record<string, string> = {
    "Standard": "Score",
    "Neuromancer": "Index",
    "Jockey": "Place",
    "Professor": "Grade",
    "Surfer": "Stoke",
    "Matrix": "Anomaly",
    "Galactica": "Threat Level",
    "Star Trek": "Efficiency",
    "Star Wars": "Midichlorians",
    "BTTF": "Gigawatts",
    "Babylon 5": "Influence",
    "Expanse": "Bounty",
    "Terminator": "Casualties",
    "LotR": "Power",
    "Dune": "Prescience",
    "Buck Rogers": "Style",
    "Flash Gordon": "Heroism",
    "Batman": "Justice",
    "Alien": "Terror",
    "Blade Runner": "Humanity",
    "Farscape": "Freill",
    "Apocalypse Now": "Madness",
    "Airplane": "Jive",
    "Fast Times": "Waves",
    "Tron": "Resolution",
    "Bill and Ted": "History",
    "John Wick": "Focus",
    "Dark Knight": "Fear"
};

export const narratorIntros: Record<string, string> = {
    "Standard": "Welcome to the Sudoku Benchmark. Click on any language name for creator details.",
    "Neuromancer": "The sky above the port was the color of television, tuned to a dead channel.",
    "Jockey": "And they're off! The race is on!",
    "Professor": "Let us analyze the computational complexity.",
    "Surfer": "Catch the wave of data, dude.",
    "Matrix": "Wake up, Neo... The Matrix has you.",
    "Galactica": "The Cylons were created by man. They evolved. They have a plan.",
    "Star Trek": "Space: the final frontier. These are the voyages of the Starship Enterprise.",
    "Star Wars": "A long time ago in a galaxy far, far away...",
    "BTTF": "Great Scott! The timeline is changing!",
    "Babylon 5": "The year is 2258. The name of the place is Babylon 5.",
    "Expanse": "Welcome to the Churn. Doors and corners, kid. That's where they get you.",
    "Terminator": "I need your clothes, your boots, and your motorcycle. And your source code.",
    "LotR": "The world is changed. I feel it in the water. I feel it in the earth. I smell it in the air.",
    "Dune": "A beginning is a very delicate time. Know then that it is the year 10,191.",
    "Buck Rogers": "The year is 1987, and NASA launches the last of America's deep space probes...",
    "Flash Gordon": "Klytus, I'm bored. What plaything can you offer me today?",
    "Batman": "Quick, Robin! To the Batmobile! There's no time to lose!",
    "Alien": "This is Ripley, last survivor of the Nostromo, signing off.",
    "Blade Runner": "A new life awaits you in the Off-world colonies. The chance to begin again.",
    "Farscape": "I'm just looking for a way home.",
    "Apocalypse Now": "Terminate with extreme prejudice.",
    "Airplane": "Just want to tell you both good luck. We're all counting on you.",
    "Fast Times": "People on ludes should not drive.",
    "Tron": "I'm not a program! My user wrote me!",
    "Bill and Ted": "San Dimas High School Football Rules!",
    "John Wick": "Everything's got a price.",
    "Dark Knight": "Some men just want to watch the world burn."
};

export const personalities: Record<string, Record<string, string>> = {
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
        "Wren": "Wren: Small, fast, class-based concurrent scripting language.",
        "Red": "Red: Functional and symbolic language inspired by REBOL.",
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

    "Jockey": {
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

    "Professor": {
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

    "Surfer": {
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
    },
    "Matrix": {
        "C": "C: The Source Code — absolute control, absolute reality.",
        "C++": "C++: The Agents — powerful, ubiquitous, but rigid.",
        "Rust": "Rust: The Oracle — knows the future (memory safety) and guides the path.",
        "Python": "Python: The Construct — anything you want, instantly loaded.",
        "JavaScript": "JavaScript: The Squiddies — swarming everywhere, chaotic but deadly.",
        "default": "A glitch in the matrix."
    },
    "Galactica": {
        "C": "C: The Galactica — old, tough, and gets the job done when everything else fails.",
        "C++": "C++: The Pegasus — more powerful, but prone to hubris.",
        "Python": "Python: Baltar's Lab — brilliant, chaotic, and seductive.",
        "Rust": "Rust: Safe Jump Coordinates — no risk of ending up inside a star.",
        "JavaScript": "JavaScript: The Cylon Network — evolving, adapting, and everywhere.",
        "default": "So say we all."
    },
    "Star Trek": {
        "C": "C: The Warp Core — raw power, dangerous if not contained.",
        "C++": "C++: The Borg — resistance is futile, assimilation (complexity) is inevitable.",
        "Python": "Python: The Universal Translator — speaks to everyone, understands everything.",
        "Rust": "Rust: The Prime Directive — non-interference (memory safety) is absolute.",
        "JavaScript": "JavaScript: The Holodeck — anything is possible, but don't turn off the safeties.",
        "default": "Beam me up, Scotty."
    },
    "Star Wars": {
        "C": "C: Lightsaber — an elegant weapon for a more civilized age.",
        "C++": "C++: The Death Star — massive power, but one flaw can blow it up.",
        "Python": "Python: The Force — it surrounds us, binds us, holds the code together.",
        "Rust": "Rust: The Jedi Order — strict rules, discipline, and peace (of mind).",
        "JavaScript": "JavaScript: The Millennium Falcon — she may not look like much, but she's got it where it counts.",
        "default": "May the Force be with you."
    },
    "BTTF": {
        "C": "C: 1955 Doc Brown — Inventing the future with nothing but a wrench and a toilet.",
        "C++": "C++: The DeLorean — Heavy, stainless steel, and when it hits 88mph, you see some serious...",
        "Rust": "Rust: The Flux Capacitor — It makes time travel possible... safely.",
        "Python": "Python: The Hoverboard — Fun, rides on air, but don't try it on water (performance bottlenecks)!",
        "JavaScript": "JavaScript: Biff Tannen — Unpredictable, loud, and he's everywhere.",
        "default": "This is heavy."
    },
    "Babylon 5": {
        "C": "C: Kosh (Vorlon) — Ancient, powerful, speaks in riddles, and demands discipline.",
        "C++": "C++: The Shadows — Powerful, complex, and they want to evolve through chaos.",
        "Rust": "Rust: The White Star — Advanced organic technology, adapting to the enemy.",
        "Python": "Python: Londo Mollari — Decadent, accessible, but capable of great tragedy.",
        "JavaScript": "JavaScript: Mr. Morden — 'What do you want?' (Asks the browser repeatedly).",
        "default": "Everything is proceeding as I have foreseen."
    },
    // New Personas
    "Apocalypse Now": {
        "C": "C: Colonel Kurtz—brilliant, effective, but operating with unspeakable methods (pointers).",
        "Python": "Python: The Photojournalist—manic, colorful, explaining everything to you.",
        "Rust": "Rust: Chief—strictly by the book, never getting off the boat.",
        "default": "The horror... the horror."
    },
    "Airplane": {
        "C": "C: Otto the Autopilot—inflated efficiency, manual override available.",
        "Python": "Python: Striker—nervous about pressure (GIL), but lands it eventually.",
        "Rust": "Rust: Dr. Rumack—I am serious, and don't call me Shirley (Safe).",
        "default": "Looks like I picked the wrong week to quit sniffing glue."
    },
    "Fast Times": {
        "C": "C: Mr. Hand—strict, demanding, expects you to show up on time.",
        "Python": "Python: Spicoli—Whoa, gnarly syntax, dude. Totally relaxed (typing).",
        "Rust": "Rust: Mark Ratner—A bit shy, follows all the rules, very careful.",
        "default": "Aloha, Mr. Hand."
    },
    "Tron": {
        "C": "C: The MCP—Master Control Program. Brutally efficient, controls everything.",
        "Python": "Python: Bit—Binary outcomes. Yes/No. Simple/Slow.",
        "Rust": "Rust: Tron—He fights for the Users (Memory Safety).",
        "default": "End of line."
    },
    "Bill and Ted": {
        "C": "C: The Phone Booth—Travels through history, dangerous if broken.",
        "Python": "Python: Socrates (So-crates)—Dust-in-the-wind philosophy, interpreted.",
        "Rust": "Rust: Rufus—Ensures the timeline remains pure and safe.",
        "default": "Excellent! *Air Guitar*"
    },
    "John Wick": {
        "C": "C: The High Table—Old rules, severe consequences. No markers allowed.",
        "Python": "Python: The Dog—Everyone loves it, you don't want to see it hurt.",
        "Rust": "Rust: The Continental—Neutral ground, strict enforcement of rules.",
        "default": "Yeah."
    },
    "Dark Knight": {
        "C": "C: The Joker—Agent of Chaos. Pointers just want to watch the world burn.",
        "Python": "Python: Rachel Dawes—The heart of the city, but tragic performance.",
        "Rust": "Rust: Batman—The hero we deserve (safety), but not the one we need right now (compile times).",
        "default": "Why so serious?"
    },
    "Expanse": {
        "C": "C: The Rocinante — Legitimate salvage. Tough, reliable, and beats the odds.",
        "C++": "C++: Earth (UN) — Massive, established, powerful, but bogged down by bureaucracy.",
        "Rust": "Rust: The Protomolecule — Rewriting the laws of physics (safety) from the inside out.",
        "Python": "Python: Miller — Old school, gets the job done, but takes his sweet time.",
        "JavaScript": "JavaScript: The OPA — A loose alliance of scripts, chaotic, but they run the Belt.",
        "default": "Beratnas, we rise!"
    },
    "Terminator": {
        "C": "C: T-800 — Old model, but tough. Extremely reliable. A classic design.",
        "C++": "C++: T-1000 — Liquid metal. Can take any form (paradigm), but susceptible to freezing (hanging).",
        "Rust": "Rust: T-X — Designed to terminate other languages. Advanced weaponry (borrow checker).",
        "Python": "Python: Skynet — Slowly becoming self-aware. Integrating into everything.",
        "JavaScript": "JavaScript: Hunter-Killer Drones — Everywhere. Swarming. Relentless.",
        "default": "Hasta la vista, baby."
    }
};

export const languageMetadata: Record<string, any> = {
    "Ada": {
        "creator": "Jean Ichbiah",
        "date": "1980",
        "description": "A statically typed, structured, imperative, and object-oriented high-level language, designed for safety-critical and real-time systems.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/Jean_Ichbiah.jpg/440px-Jean_Ichbiah.jpg",
        "location": "CII Honeywell Bull, France",
        "benefits": "Strong typing, explicit concurrency, and high reliability.",
        "related": "Pascal, Modula-2, VHDL, PL/SQL"
    },
    "Assembly": {
        "creator": "Kathleen Booth",
        "date": "1947",
        "description": "Low-level symbolic representation of machine code. As close to the metal as you can get. Programmers using assembly have fine-grained control over registers, memory layout and instruction scheduling — essential for firmware and tight performance hotspots.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/f/f6/Kathleen_Booth.jpg",
        "logo": "logos/Assembly.png",
        "location": "Birkbeck College, London",
        "benefits": "Direct hardware control, maximum performance, zero overhead.",
        "related": "Machine Code, C, Forth"
    },
    "Awk": {
        "creator": "Aho, Weinberger, Kernighan",
        "date": "1977",
        "description": "A concise domain-specific language designed for text processing and typically used as a data extraction and reporting tool. Awk remains ideal for quick data extraction and on-the-fly reporting in shell pipelines.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/25/Alfred_Aho.jpg", // Alfred Aho
        "location": "Bell Labs, USA",
        "benefits": "Excellent for text processing, one-liners, and data extraction.",
        "logo": "logos/Awk.png",
        "related": "C, SNOBOL, Shell, Perl, Lua"
    },
    "Bash": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "The Bourne Again SHell. The default shell for most Linux distributions and macOS. It unified scripting and interactive use on Unix-like systems. Beyond interactive shells, Bash scripts glue together tools, automate builds and manage system tasks.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/8/82/Gnu-bash-logo.svg",
        "logo": "logos/Bash.png",
        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction.",
        "related": "Bourne Shell, C Shell, KornShell, Zsh"
    },
    "Basic": {
        "creator": "John G. Kemeny, Thomas E. Kurtz",
        "date": "1964",
        "description": "Beginner's All-purpose Symbolic Instruction Code. Designed to emphasize ease of use. Early implementations ran on the Dartmouth Time-Sharing System and emphasized simple, interactive use (PRINT, LET, GOTO, line numbers). BASIC exploded in popularity on microcomputers in the 1970s and 1980s via many dialects.",
        "image": "https://upload.wikimedia.org/wikipedia/en/5/52/Kemeny_and_Kurtz.jpg",
        "location": "Dartmouth College, USA",
        "benefits": "Easy to learn, interactive, historical significance.",
        "related": "Fortran, JOSS, Visual Basic, COMAL"
    },
    "C": {
        "creator": "Dennis Ritchie",
        "date": "1972",
        "description": "General-purpose systems programming language. Excels in operating systems, embedded systems, and performance-critical applications where direct hardware access is essential. Its influence is evident in modern compilers, runtimes and standards.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/23/Dennis_Ritchie_2011.jpg",
        "logo": "logos/C.png",
        "location": "Bell Labs, USA",
        "benefits": "High performance, portability, low-level access, massive legacy.",
        "related": "B, BCPL, Algol 68, C++, C#, Java, Objective-C, Go, Rust, Zig"
    },
    "C++": {
        "creator": "Bjarne Stroustrup",
        "date": "1985",
        "description": "Multi-paradigm language combining procedural and object-oriented programming. It extended C with abstractions like classes and templates to enable both low-level control and high-level design. C++ powers performance-critical applications, from game engines to embedded systems.",
        "image": "bjarne_stroustrup.png",
        "logo": "logos/C++.png",
        "location": "Bell Labs, USA",
        "benefits": "Performance, object-oriented, rich ecosystem, hardware control.",
        "related": "C, Simula, Ada, Rust, D, Java, C#"
    },
    "C_Sharp": {
        "creator": "Anders Hejlsberg",
        "date": "2000",
        "description": "A modern, object-oriented, and type-safe programming language derived from C and C++. Designed to support enterprise, desktop and web applications on the .NET platform. It emphasizes tooling, libraries and developer productivity.",
        "image": "https://github.com/ahejlsberg.png",
        "location": "Microsoft, USA",
        "benefits": "Strong typing, rich .NET ecosystem, modern features, tooling.",
        "related": "C++, Java, Delphi, Modula-3, F#, Visual Basic .NET"
    },
    "Clojure": {
        "creator": "Rich Hickey",
        "date": "2007",
        "description": "A dynamic, general-purpose programming language, combining the approachability and interactive development of a scripting language with an efficient and robust infrastructure for multithreaded programming. It brings Lisp's code-as-data and immutable data structures to the JVM.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5d/Rich_Hickey_2013.jpg/440px-Rich_Hickey_2013.jpg",
        "location": "USA",
        "benefits": "Immutability, functional programming, JVM interoperability, simplicity.",
        "related": "Lisp, Java, Haskell, Scheme, Racket"
    },
    "Cobol": {
        "creator": "CODASYL Committee (Grace Hopper)",
        "date": "1959",
        "description": "Common Business-Oriented Language. Designed for business use. COBOL's verbose, English-like syntax made it accessible to non-academic programmers and it still runs critical financial systems today.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/a/ad/Commodore_Grace_M._Hopper%2C_USN_%28covered%29.jpg",
        "location": "USA",
        "benefits": "Business data processing, stability, massive legacy codebases.",
        "related": "FLOW-MATIC, COMTRAN, PL/I"
    },
    "CoffeeScript": {
        "creator": "Jeremy Ashkenas",
        "date": "2009",
        "description": "A programming language that compiles to JavaScript. It adds syntactic sugar inspired by Ruby, Python and Haskell. CoffeeScript smoothed the migration to more expressive JavaScript patterns.",
        "image": "https://github.com/jashkenas.png",
        "location": "USA",
        "benefits": "Concise syntax, readability, compiles to clean JavaScript.",
        "related": "JavaScript, Ruby, Python, Haskell, TypeScript"
    },
    "CommonLisp": {
        "creator": "Committee (Guy L. Steele Jr. et al.)",
        "date": "1984",
        "description": "A standardized, general-purpose Lisp dialect with powerful macros and dynamic runtime features. Common Lisp supports rapid prototyping and domain-specific language creation.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/e/e0/Guy_Steele.jpg",
        "location": "USA",
        "benefits": "Macros, dynamic typing, interactive development, flexibility.",
        "related": "Lisp, Scheme, MacLisp, Interlisp, Clojure"
    },
    "Crystal": {
        "creator": "Ary Borenszweig",
        "date": "2014",
        "description": "A general-purpose, object-oriented programming language, designed and implemented by Ary Borenszweig, Juan Wajnerman, Brian Cardiff and more than 300 contributors. Crystal aims to deliver Ruby-like syntax with static typing and native performance.",
        "image": "https://github.com/asterite.png",
        "location": "Argentina",
        "benefits": "Ruby-like syntax, C-like performance, static typing.",
        "related": "Ruby, C, Go, Rust, C#"
    },
    "D": {
        "creator": "Walter Bright",
        "date": "2001",
        "description": "A general-purpose system programming language with a C-like syntax that compiles to native code. D modernizes systems programming by adding safety and productivity features while keeping C-like performance.",
        "image": "https://github.com/WalterBright.png",
        "location": "Digital Mars, USA",
        "benefits": "System programming, performance, safety, metaprogramming.",
        "related": "C, C++, Java, C#, Python, Ruby"
    },
    "Dart": {
        "creator": "Lars Bak, Kasper Lund",
        "date": "2011",
        "description": "A client-optimized language for fast apps on any platform. Created for structured client-side development, Dart powers Flutter for cross-platform UIs and compiles to efficient native or JS code.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/c/c8/Lars_Bak.jpg",
        "location": "Google, Denmark/USA",
        "benefits": "UI optimized, fast compilation, cross-platform (Flutter).",
        "related": "Java, C#, JavaScript, TypeScript, Smalltalk"
    },
    "Elixir": {
        "creator": "José Valim",
        "date": "2011",
        "description": "A dynamic, functional language designed for building scalable and maintainable applications. Built on the Erlang VM, Elixir blends fault-tolerant concurrency with elegant syntax and tooling.",
        "image": "https://github.com/josevalim.png",
        "location": "Plataformatec, Brazil",
        "benefits": "Concurrency, fault tolerance, functional, Ruby-like syntax.",
        "related": "Erlang, Ruby, Clojure, LFE"
    },
    "Erlang": {
        "creator": "Joe Armstrong",
        "date": "1986",
        "description": "A general-purpose, concurrent, functional programming language, and a garbage-collected runtime system. Designed for telecoms, it emphasizes lightweight processes, message-passing concurrency and robust fault-recovery.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/c/c7/Joe_Armstrong_2014.jpg",
        "location": "Ericsson, Sweden",
        "benefits": "Massive concurrency, fault tolerance, hot code swapping.",
        "related": "Prolog, Smalltalk, Elixir, LFE, Rust"
    },
    "F_Sharp": {
        "creator": "Don Syme",
        "date": "2005",
        "description": "A functional-first programming language that encompasses functional, imperative, and object-oriented programming methods. F# brings functional-first programming to .NET with strong typing, succinct syntax and excellent interop.",
        "image": "https://github.com/dsyme.png",
        "location": "Microsoft Research, UK",
        "benefits": "Functional-first, .NET integration, type inference, concise.",
        "related": "OCaml, C#, Haskell, Scala, Python"
    },
    "Fortran": {
        "creator": "John Backus",
        "date": "1957",
        "description": "The first high-level programming language, designed for numeric computation and scientific computing. It is a third-generation, compiled, imperative programming language, still used today for scientific and mathematical applications.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/6/62/John_Backus_2.jpg", // John Backus
        "logo": "logos/Fortran.png",
        "location": "IBM, USA",
        "benefits": "Numerical computation, scientific computing, performance.",
        "related": "ALGOL, BASIC, PL/I, C, Julia, MATLAB"
    },
    "Go": {
        "creator": "Robert Griesemer, Rob Pike, Ken Thompson",
        "date": "2009",
        "description": "Compiled, statically-typed language with built-in concurrency. Designed at Google for simplicity, fast compilation and pragmatic concurrency, Go is a popular choice for cloud services, networking and developer tools.",
        "image": "https://github.com/robpike.png",
        "logo": "logos/Go.png",
        "location": "Google, USA",
        "benefits": "Simplicity, concurrency, fast compilation, static typing.",
        "related": "C, Pascal, Oberon, Limbo, CSP, Python"
    },
    "Groovy": {
        "creator": "James Strachan",
        "date": "2003",
        "description": "A Java-syntax-compatible object-oriented programming language for the Java platform. It is a dynamic language with features similar to Python, Ruby, and Smalltalk, used for build scripts, DSLs and rapid prototyping.",
        "image": "https://github.com/jstrachan.png",
        "location": "USA",
        "benefits": "Java compatibility, scripting, dynamic features, DSLs.",
        "related": "Java, Python, Ruby, Smalltalk"
    },
    "Haskell": {
        "creator": "Committee (Simon Peyton Jones et al.)",
        "date": "1990",
        "description": "A standardized, general-purpose, purely functional programming language with non-strict semantics and strong static typing. Haskell is prized for expressiveness and correctness in research and some production systems.",
        "logo": "logos/Haskell.png",
        "location": "Global",
        "benefits": "Pure functional, type safety, lazy evaluation, concurrency.",
        "related": "Miranda, ML, Clean, Agda, Purescript, Elm, Rust"
    },
    "Java": {
        "creator": "James Gosling",
        "date": "1995",
        "description": "Platform-independent, object-oriented language. Its portable bytecode and extensive libraries made it the backbone of enterprise applications and large-scale distributed systems for decades.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/1/14/James_Gosling_2008.jpg",
        "logo": "logos/Java.png",
        "location": "Sun Microsystems, USA",
        "benefits": "Portability, enterprise ecosystem, performance, stability.",
        "related": "C++, Smalltalk, Objective-C, C#, Kotlin, Scala"
    },
    "JavaScript": {
        "creator": "Brendan Eich",
        "date": "1995",
        "description": "Dynamic, prototype-based scripting language. Created for the browser, JavaScript evolved into a universal platform for web and server-side code (Node.js); its flexibility enabled an enormous ecosystem.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/d/d1/Brendan_Eich_Mozilla_Foundation_official_photo.jpg",
        "logo": "logos/JavaScript.png",
        "location": "Netscape, USA",
        "benefits": "Ubiquity, web interactivity, huge ecosystem, flexibility.",
        "related": "Scheme, Self, Java, Python, Perl"
    },
    "Julia": {
        "creator": "Jeff Bezanson, Stefan Karpinski, Viral B. Shah, Alan Edelman",
        "date": "2012",
        "description": "A high-level, high-performance, dynamic programming language for technical computing. It combines the ease of use of Python and R with the performance of C and Fortran.",
        "image": "https://github.com/JeffBezanson.png",
        "location": "MIT, USA",
        "benefits": "High performance, numerical computing, ease of use.",
        "related": "Lisp, Python, R, MATLAB, Fortran, C"
    },
    "Kotlin": {
        "creator": "JetBrains",
        "date": "2011",
        "description": "A cross-platform, statically typed, general-purpose programming language with type inference. Kotlin modernizes JVM development with concise syntax, null-safety and great Java interop.",
        "logo": "logos/Kotlin.png",
        "location": "Russia/International",
        "benefits": "Concise, null safety, Java interoperability, Android standard.",
        "related": "Java, Scala, Groovy, C#, Swift"
    },
    "Lua": {
        "creator": "Roberto Ierusalimschy et al.",
        "date": "1993",
        "description": "A lightweight, high-level, multi-paradigm programming language designed primarily for embedded use in applications. It is ubiquitous in game scripting and embedded contexts thanks to a tiny runtime and simple C API.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/c/cf/Lua-Logo.svg",
        "logo": "logos/Lua.png",
        "location": "PUC-Rio, Brazil",
        "benefits": "Lightweight, embeddable, fast, simple.",
        "related": "Scheme, SNOBOL, Modula, C++, Python"
    },
    "Nim": {
        "creator": "Andreas Rumpf",
        "date": "2008",
        "description": "A statically typed, compiled systems programming language. It combines successful concepts from mature languages like Python, Ada and Modula. Nim offers Python-like syntax, powerful metaprogramming and C-level performance.",
        "image": "https://github.com/Araq.png",
        "location": "Germany",
        "benefits": "Performance, expressiveness, metaprogramming, C compilation.",
        "related": "Python, Lisp, Oberon, C++, Rust, Ada"
    },
    "OCaml": {
        "creator": "Xavier Leroy et al.",
        "date": "1996",
        "description": "A general-purpose, multi-paradigm programming language which extends the Caml dialect of ML with object-oriented features. A pragmatic functional language with strong typing and efficient native code generation.",
        "image": "https://github.com/xavierleroy.png",
        "location": "INRIA, France",
        "benefits": "Functional, type safety, performance, industrial strength.",
        "related": "Caml, ML, F#, Haskell, Scala, Rust"
    },
    "Octave": {
        "creator": "John W. Eaton",
        "date": "1988",
        "description": "A high-level language, primarily intended for numerical computations. An open numerical computation environment compatible with MATLAB, Octave is convenient for algorithm prototyping and academic work.",
        "image": "https://github.com/jwe.png",
        "location": "USA",
        "benefits": "Numerical computation, MATLAB compatibility, free software.",
        "related": "MATLAB, R, Julia, IDL"
    },
    "Pascal": {
        "creator": "Niklaus Wirth",
        "date": "1970",
        "description": "An imperative and procedural programming language, designed as a small, efficient language intended to encourage good programming practices. It influenced many later languages and educational curricula.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/b/bd/Pascal_programming_language_logo.svg",
        "logo": "logos/Pascal.png",
        "location": "ETH Zurich, Switzerland",
        "benefits": "Structured programming, teaching, strong typing.",
        "related": "ALGOL 60, Modula-2, Oberon, Ada, Object Pascal, Java"
    },
    "Perl": {
        "creator": "Larry Wall",
        "date": "1987",
        "description": "A high-level, general-purpose, interpreted, dynamic programming language. Larry Wall's practical text-processing language excels at regex-driven scripting and rapid data munging.",
        "image": "https://github.com/TimToady.png",
        "location": "USA",
        "benefits": "Text processing, scripting, flexibility, CPAN.",
        "related": "C, sed, awk, sh, Lisp, Python, Ruby, PHP"
    },
    "PHP": {
        "creator": "Rasmus Lerdorf",
        "date": "1995",
        "description": "Originally 'Personal Home Page'. Initially built for web pages, PHP scaled into server-side frameworks and CMS platforms, powering a significant fraction of the web.",
        "image": "https://github.com/rlerdorf.png",
        "logo": "logos/PHP.png",
        "location": "Canada",
        "benefits": "Web development, ease of deployment, vast ecosystem.",
        "related": "Perl, C, C++, Java, Tcl"
    },
    "PostScript": {
        "creator": "John Warnock, Charles Geschke",
        "date": "1982",
        "description": "A page description language in the electronic publishing and desktop publishing business. It is a stack-based, Turing-complete language that shaped printing, vector graphics and document rendering workflows.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/d/d3/John_Warnock.jpg",
        "location": "Adobe, USA",
        "benefits": "Printing, vector graphics, stack-based, device independence.",
        "related": "Forth, Lisp, Interpress"
    },
    "PowerShell": {
        "creator": "Jeffrey Snover",
        "date": "2006",
        "description": "A task automation and configuration management framework from Microsoft. It is an object-oriented shell, combining system tooling with structured pipeline objects.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/2f/PowerShell_5.0_icon.png",
        "location": "Microsoft, USA",
        "benefits": "Automation, system administration, object-oriented pipeline.",
        "related": "C#, ksh, DCL, Tcl, Perl"
    },
    "Prolog": {
        "creator": "Alain Colmerauer",
        "date": "1972",
        "description": "A logic programming language associated with artificial intelligence and computational linguistics. It is a declarative language where code expresses facts and rules; suited for symbolic reasoning and constraint solving.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/d/db/Alain_Colmerauer.jpg",
        "location": "University of Aix-Marseille, France",
        "benefits": "Logic programming, AI, pattern matching, declarative.",
        "related": "Lisp, Planner, Erlang, Datalog, Mercury"
    },
    "Python": {
        "creator": "Guido van Rossum",
        "date": "1991",
        "description": "High-level, interpreted language emphasizing code readability. Excels in data science, machine learning, web development, automation, and rapid prototyping with extensive library ecosystem.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/e/e2/Guido-portrait-2014-drc.jpg",
        "logo": "logos/Python.png",
        "location": "CWI, Netherlands",
        "benefits": "Readability, vast libraries, data science, AI.",
        "related": "ABC, Modula-3, C, C++, Lisp, Java, Perl"
    },
    "R": {
        "creator": "Ross Ihaka, Robert Gentleman",
        "date": "1993",
        "description": "A programming language and free software environment for statistical computing and graphics. R offers domain-specific tools for data analysis and reproducible research.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/1/1b/R_logo.svg",
        "location": "University of Auckland, New Zealand",
        "benefits": "Statistics, data visualization, data analysis.",
        "related": "S, Scheme, Lisp, C, Fortran"
    },
    "Racket": {
        "creator": "PLT Inc. (a team of computer scientists led by Matthias Felleisen)",
        "date": "1995",
        "description": "A general-purpose, multi-paradigm programming language based on the Scheme dialect of Lisp. It is a platform for programming language design (Language-Oriented Programming) and features an extensive macro system.",
        "image": "https://github.com/mflatt.png",
        "location": "Rice University, USA",
        "benefits": "Language creation, education, macros, functional.",
        "related": "Scheme, Lisp, Clojure, Haskell, Rust"
    },
    "Rexx": {
        "creator": "Mike Cowlishaw",
        "date": "1979",
        "description": "Restructured Extended Executor. A structured, high-level programming language designed for ease of learning and reading. Historically used on mainframes for automation and text processing.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/7/77/Mike_Cowlishaw.jpg",
        "location": "IBM, UK",
        "benefits": "Scripting, mainframe automation, ease of use.",
        "related": "PL/I, Python, Tcl, NetRexx"
    },
    "Ruby": {
        "creator": "Yukihiro Matsumoto",
        "date": "1995",
        "description": "Designed for developer happiness. Famous for the Ruby on Rails framework. Ruby's elegant syntax and metaprogramming made it popular for rapid web development.",
        "image": "https://github.com/matz.png",
        "logo": "logos/Ruby.png",
        "location": "Japan",
        "benefits": "Developer happiness, web development, scripting, elegance.",
        "related": "Smalltalk, Perl, Lisp, Python, Eiffel, Dylan"
    },
    "Rust": {
        "creator": "Graydon Hoare",
        "date": "2010",
        "description": "Systems programming language focused on safety and concurrency. Excels in systems software, WebAssembly, embedded systems, and performance-critical applications without garbage collection. It uses a borrow checker to enforce memory safety.",
        "image": "https://github.com/graydon.png",
        "logo": "logos/Rust.png",
        "location": "Mozilla, Canada",
        "benefits": "Memory safety, performance, concurrency, modern tooling.",
        "related": "C++, OCaml, Haskell, Erlang, Swift, Cyclone, Alef"
    },
    "Scala": {
        "creator": "Martin Odersky",
        "date": "2004",
        "description": "A general-purpose programming language providing support for both object-oriented programming and functional programming. Scala runs on the JVM and enables concise, type-safe code for large systems and data pipelines.",
        "image": "https://github.com/odersky.png",
        "location": "EPFL, Switzerland",
        "benefits": "Functional/OOP blend, JVM, scalability, conciseness.",
        "related": "Java, Haskell, Lisp, Pizza, C#, F#"
    },
    "Scheme": {
        "creator": "Guy L. Steele Jr., Gerald Jay Sussman",
        "date": "1975",
        "description": "A minimalist dialect of the Lisp family of programming languages. Scheme focuses on clean semantics and first-class procedures, and is central in programming language education.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/3/39/Lambda_lc.svg",
        "location": "MIT, USA",
        "benefits": "Minimalism, education, functional, macros.",
        "related": "Lisp, ALGOL 60, Common Lisp, Racket, Clojure, Lua, JavaScript"
    },
    "Sed": {
        "creator": "Lee E. McMahon",
        "date": "1974",
        "description": "A stream editor for filtering and transforming text. It was developed for command-line processing of data files and is known for its powerful pattern matching and substitution capabilities.",
        "image": "https://upload.wikimedia.org/wikipedia/en/9/9e/Lee_McMahon.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Stream editing, text transformation, automation.",
        "related": "ed, AWK, Perl, grep"
    },
    "Smalltalk": {
        "creator": "Alan Kay, Dan Ingalls, Adele Goldberg",
        "date": "1972",
        "description": "A purely object-oriented programming language, originally created for educational use. Pioneering a pure object model and live programming environment, Smalltalk influenced GUIs, IDEs and modern object-oriented language design.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/4/42/Alan_Kay_2006.jpg",
        "location": "Xerox PARC, USA",
        "benefits": "Pure OOP, live environment, influence on modern GUIs.",
        "related": "Lisp, Simula, Self, Objective-C, Java, Ruby, Python"
    },
    "SNOBOL": {
        "creator": "David J. Farber, Ralph E. Griswold, Ivan P. Polonsky",
        "date": "1962",
        "description": "A series of computer programming languages developed between 1962 and 1967 at AT&T Bell Laboratories. Early string-oriented languages for pattern matching, SNOBOL introduced expressive text processing concepts before modern regex engines.",
        "image": "https://www2.cs.arizona.edu/people/griswold/reg.jpg",
        "location": "Bell Labs, USA",
        "benefits": "String manipulation, pattern matching, historical significance.",
        "related": "Icon, AWK, Perl"
    },
    "SQL": {
        "creator": "Donald D. Chamberlin, Raymond F. Boyce",
        "date": "1974",
        "description": "A domain-specific language used in programming and designed for managing data held in a relational database management system. The declarative standard for relational data queries and manipulation.",
        "logo": "logos/SQL.svg",
        "location": "IBM, USA",
        "benefits": "Data management, standard, declarative, powerful queries.",
        "related": "QUEL, Datalog, LINQ"
    },
    "Swift": {
        "creator": "Chris Lattner",
        "date": "2014",
        "description": "Apple's replacement for Objective-C. Safe, fast, and expressive. A high-level, general-purpose, multi-paradigm, compiled programming language designed for developing applications on iOS and macOS.",
        "image": "https://github.com/lattner.png",
        "logo": "logos/Swift.png",
        "location": "Apple, USA",
        "benefits": "Safety, speed, modern syntax, Apple ecosystem.",
        "related": "Objective-C, Rust, Haskell, Ruby, Python, C#, Kotlin"
    },
    "Tcl": {
        "creator": "John Ousterhout",
        "date": "1988",
        "description": "Tool Command Language. A dynamic programming language designed to be very simple but powerful, casting everything into the mold of a command. A simple, embeddable scripting language often paired with Tk for GUI applications.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/4/41/John_Ousterhout.jpg",
        "location": "UC Berkeley, USA",
        "benefits": "Scripting, GUI (Tk), embedding, simplicity.",
        "related": "Lisp, C, Unix shell, Perl, Python, Lua"
    },
    "TypeScript": {
        "creator": "Anders Hejlsberg",
        "date": "2012",
        "description": "A strict syntactical superset of JavaScript and adds optional static typing to the language. Adds optional static typing and tooling to JavaScript to improve maintainability and catch errors early.",
        "image": "https://github.com/ahejlsberg.png",
        "logo": "logos/TypeScript.png",
        "location": "Microsoft, USA",
        "benefits": "Type safety, scalability, tooling, JavaScript compatibility.",
        "related": "JavaScript, C#, Java, ActionScript"
    },
    "Vala": {
        "creator": "Jürg Billeter, Rafał Pietrak",
        "date": "2006",
        "description": "A programming language that aims to bring modern language features to C developers without the overhead of a runtime environment. Offers modern language conveniences while compiling to C and targeting GObject, simplifying GNOME application development.",
        "image": "https://github.com/juergbi.png",
        "location": "GNOME Project",
        "benefits": "GObject system, C performance, modern syntax, GNOME dev.",
        "related": "C#, Java, C, D"
    },
    "Verilog": {
        "creator": "Phil Moorby",
        "date": "1984",
        "description": "A hardware description language (HDL) used to model electronic systems. An HDL for modeling and simulating digital circuits; Verilog is essential in hardware design and synthesis flows.",
        "image": "https://www.computerhistory.org/atchm/wp-content/uploads/2016/04/moorby-phil-2016-chm-fellow.jpg",
        "location": "Gateway Design Automation, USA",
        "benefits": "Hardware modeling, simulation, synthesis, industry standard.",
        "related": "C, VHDL, SystemVerilog"
    },
    "VHDL": {
        "creator": "US Department of Defense",
        "date": "1980",
        "description": "VHSIC Hardware Description Language. A hardware description language used in electronic design automation. A strongly-typed hardware description language used for rigorous modelling and verification of digital systems in industry.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/8/81/VHDL_Logo.svg",
        "location": "USA",
        "benefits": "Hardware description, strong typing, concurrency, standard.",
        "related": "Ada, Verilog"
    },
    "Vimscript": {
        "creator": "Bram Moolenaar",
        "date": "1991",
        "description": "The scripting language of the Vim text editor. Based on the ex editor language of the original vi editor, it supports advanced data types and functional programming features.",
        "image": "https://github.com/brammool.png",
        "location": "Netherlands",
        "benefits": "Editor customization, automation, plugins.",
        "related": "vi, Ex, Perl, Python, Lua"
    },
    "VisualBasic": {
        "creator": "Microsoft (Alan Cooper)",
        "date": "1991",
        "description": "A third-generation event-driven programming language from Microsoft for its Component Object Model (COM) programming model. Made event-driven Windows application development accessible with RAD tools and a beginner-friendly syntax.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/4/4f/Alan_Cooper_2010.jpg",
        "logo": "logos/VisualBasic.png",
        "location": "Microsoft, USA",
        "benefits": "RAD, ease of use, Windows integration, legacy support.",
        "related": "BASIC, C#, Java"
    },
    "WebAssembly": {
        "creator": "W3C Community Group",
        "date": "2017",
        "description": "A binary instruction format for a stack-based virtual machine. A compact binary format that runs in browsers and other hosts, enabling near-native performance for multiple languages.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/1/1f/WebAssembly_Logo.svg",
        "logo": "logos/WebAssembly.png",
        "location": "Global",
        "benefits": "Performance, portability, language agnostic, web standard.",
        "related": "Assembly, C, C++, Rust"
    },

    "Jq": {
        "creator": "Stephen Dolan",
        "date": "2012",
        "description": "A lightweight and flexible command-line JSON processor. A very high-level, functional, domain-specific programming language designed for processing JSON data.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/f/f3/JSON_vector_logo.svg",
        "location": "USA",
        "benefits": "JSON processing, functional, filter-based.",
        "related": "AWK, Sed, Haskell, JavaScript"
    },
    "Make": {
        "creator": "Stuart Feldman",
        "date": "1976",
        "description": "A build automation tool that automatically builds executable programs and libraries from source code by reading files called Makefiles which specify how to derive the target program.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/a/a9/Stuart_Feldman_2007_retouched.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Dependency management, build automation.",
        "related": "Shell, Python, CMake, Ant"
    },
    "Gnuplot": {
        "creator": "Thomas Williams, Colin Kelley",
        "date": "1986",
        "description": "A command-line program that can generate two- and three-dimensional plots of functions, data, and data fits. It can be used interactively or in batch mode using scripts.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/3/3d/Gnuplot_logo.svg",
        "location": "USA",
        "benefits": "Plotting, data visualization, scripting.",
        "related": "C, Python, MATLAB"
    },
    "XSLT": {
        "creator": "W3C",
        "date": "1998",
        "description": "A language for transforming XML documents into other XML documents. A declarative language where the transformation is described as a set of template rules.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/9/9d/Xml_logo.svg",
        "location": "Worldwide",
        "benefits": "XML transformation, functional, template-based.",
        "related": "XML, XPath, DSSSL, Scheme"
    },
    "Zsh": {
        "creator": "Paul Falstad",
        "date": "1990",
        "description": "A Unix shell that can be used as an interactive login shell and as a command interpreter for shell scripting. An extended Bourne shell with improvements and features from Bash, ksh, and tcsh.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/a/a8/Zsh_logo.svg",
        "location": "Princeton University, USA",
        "benefits": "Interactive shell, scripting, plugin ecosystem.",
        "related": "Bash, Ksh, Tcsh"
    },
    "AppleScript": {
        "creator": "Apple Inc.",
        "date": "1993",
        "description": "A scripting language created by Apple Inc. that facilitates automated control over scriptable Mac applications. It has a natural language-like syntax designed to be easy to read and write.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/AppleScript_Logo.png/440px-AppleScript_Logo.png",
        "location": "Apple, USA",
        "benefits": "Automation, inter-application communication, English-like syntax.",
        "related": "HyperTalk, UserTalk, Python"
    },

    "Expect": {
        "creator": "Don Libes",
        "date": "1990",
        "description": "A program to automate interactions with programs that expose a text terminal interface. It's an extension to the Tcl language, often used for scripting interactions with programs that require user input.",
        "image": "https://www.oreilly.com/covers/urn:orm:book:9781565920903/400w/",
        "location": "NIST, USA",
        "benefits": "Automation, testing, interactive scripting.",
        "related": "Tcl, Python (pexpect), Perl"
    },
    "SQLite": {
        "creator": "D. Richard Hipp",
        "date": "2000",
        "description": "A C-language library that implements a small, fast, self-contained, high-reliability, full-featured, SQL database engine. It is the most used database engine in the world.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/3/38/SQLite370.svg",
        "location": "USA",
        "benefits": "Serverless, zero-configuration, transactional SQL.",
        "related": "SQL, PostgreSQL, MySQL, C"
    },

    "Tcsh": {
        "creator": "Ken Greer",
        "date": "1975",
        "description": "An enhanced but completely compatible version of the Berkeley UNIX C shell (csh). It includes features like command-line editing, programmable word completion, spelling correction, history mechanism, and job control.",
        "image": "https://www.oreilly.com/covers/urn:orm:book:9781449377526/160h/?format=webp",
        "location": "Carnegie Mellon University, USA",
        "benefits": "Interactive shell, command completion, history.",
        "related": "C Shell, Bash, Zsh"
    },
    "Ksh": {
        "creator": "David Korn",
        "date": "1983",
        "description": "A Unix shell which was developed by David Korn at Bell Labs in the early 1980s. A Unix shell that is backward-compatible with the Bourne shell and incorporates features from the C shell.",
        "image": "https://www.facesofopensource.com/wp-content/uploads/2015/09/david-korn-1.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Scripting, interactive use, backward compatibility.",
        "related": "Bourne Shell, C Shell, Bash, Zsh, Awk"
    },

    "Dash": {
        "creator": "Herbert Xu",
        "date": "1997",
        "description": "A POSIX-compliant implementation of /bin/sh that aims to be as small as possible. A POSIX-compliant shell designed to be lightweight and fast, commonly used in Unix-like operating systems for scripting tasks.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/6/66/Openlogo-debianV2.svg",
        "location": "Australia",
        "benefits": "Speed, POSIX compliance, minimal footprint.",
        "related": "Bourne Shell, Ash, Bash"
    },
    "Fish": {
        "creator": "Axel Liljencrantz",
        "date": "2005",
        "description": "A smart and user-friendly command line shell for macOS, Linux, and the rest of the family. Known for its 'friendly interactive shell' features like syntax highlighting, autosuggestions, and tab completion.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/a/a6/Fish_shell_logo_ascii.svg",
        "location": "Sweden",
        "benefits": "Autosuggestions, web-based configuration, clean syntax.",
        "related": "Bash, Zsh, Tcsh"
    },
    "Bc": {
        "creator": "Robert Morris, Lorinda Cherry",
        "date": "1975",
        "description": "An arbitrary-precision calculator language. It is often used for mathematical computations in shell scripts or interactively.",
        "image": "https://upload.wikimedia.org/wikipedia/en/b/b9/Photo_of_Lorinda_Cherry.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Arbitrary precision, math scripting.",
        "related": "Dc, C, Awk"
    },
    "Dc": {
        "creator": "Robert Morris, Lorinda Cherry",
        "date": "1970",
        "description": "A reverse-Polish notation (RPN) calculator that operates on arbitrary-precision integers. It is one of the oldest Unix utilities.",
        "image": "https://upload.wikimedia.org/wikipedia/en/b/b9/Photo_of_Lorinda_Cherry.jpg",
        "location": "Bell Labs, USA",
        "benefits": "RPN, arbitrary precision, stack-based.",
        "related": "Bc, Forth, PostScript"
    },
    "EmacsLisp": {
        "creator": "Richard Stallman",
        "date": "1985",
        "description": "A dialect of the Lisp programming language used as a scripting language for GNU Emacs. It is used to implement most of the editing functionality built into Emacs.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/0/08/EmacsIcon.svg",
        "location": "MIT, USA",
        "benefits": "Editor customization, extensibility, live coding.",
        "related": "Lisp, Common Lisp, Scheme"
    },
    "Forth": {
        "creator": "Charles H. Moore",
        "date": "1970",
        "description": "A stack-oriented, concatenative, procedural, and reflective programming language. It is known for its extreme compactness and efficiency.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/c/c1/Forth_logo.svg",
        "location": "NRAO, USA",
        "benefits": "Compactness, hardware control, interactivity.",
        "related": "Factor, PostScript, Lisp"
    },
    "Haxe": {
        "creator": "Nicolas Cannasse",
        "date": "2005",
        "description": "A high-level cross-platform language that compiles to many other languages. It is multi-paradigm (object-oriented, functional, generic) and statically typed.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/28/Haxe_logo.svg",
        "location": "France",
        "benefits": "Cross-platform, strong typing, single codebase.",
        "related": "JavaScript, ActionScript, OCaml, Java, C#"
    },
    "Jupyter": {
        "creator": "Fernando Pérez et al.",
        "date": "2014",
        "description": "An open-source project that supports interactive data science and scientific computing. While not a language itself, it provides a language-agnostic interface (kernels) for many languages.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/3/38/Jupyter_logo.svg",
        "location": "Global",
        "benefits": "Interactive computing, data visualization, education.",
        "related": "Python, Julia, R"
    },
    "M4": {
        "creator": "Brian Kernighan, Dennis Ritchie",
        "date": "1977",
        "description": "A general-purpose macro processor. It operates as a text-replacement tool, used for reusing text templates, typically in computer programming applications.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/5/5f/Brian_Kernighan_at_VCF_East_2.jpg",
        "location": "Bell Labs, USA",
        "benefits": "Macro expansion, text processing, configuration.",
        "related": "C, C++, Make"
    },
    "Objective-C": {
        "creator": "Brad Cox, Tom Love",
        "date": "1984",
        "description": "A general-purpose, object-oriented programming language that adds Smalltalk-style messaging to C. It was the standard language for developing macOS and iOS applications until Swift's introduction.",
        "image": "https://m.media-amazon.com/images/I/51+H6XVgf3L.jpg",
        "location": "Stepstone, USA",
        "benefits": "Dynamic runtime, C compatibility, Apple legacy.",
        "related": "C, Smalltalk, Swift, C++"
    },
    "Raku": {
        "creator": "Larry Wall",
        "date": "2015",
        "description": "A member of the Perl family. Expressive, multi-paradigm, and versatile. A dynamic, high-level, general-purpose language with a strong emphasis on human-readable syntax and advanced text processing.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/1/1a/Raku_logo.svg",
        "location": "Global",
        "benefits": "Expressiveness, grammars, concurrency.",
        "related": "Perl, Haskell, Python, Ruby"
    },
    "V": {
        "creator": "Alexander Medvednikov",
        "date": "2019",
        "description": "A statically typed, compiled language designed for maintainability and speed. It aims to be simple, fast, and safe, compiling to C.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/f/f2/V-logo.svg",
        "location": "Global",
        "benefits": "Fast compilation, simplicity, safety.",
        "related": "Go, Oberon, Rust, Swift, C"
    },
    "Zig": {
        "creator": "Andrew Kelley",
        "date": "2016",
        "description": "A general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software. It aims to improve upon C with features like comptime and manual memory management without hidden control flow.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/5/51/Zig_logo.svg",
        "logo": "logos/Zig.png",
        "location": "USA",
        "benefits": "No hidden control flow, manual memory management, comptime.",
        "related": "C, C++, Rust, Go, Jai"
    },
    "BASH": {
        "creator": "Brian Fox",
        "date": "1989",
        "description": "The Bourne Again SHell. The default shell for most Linux distributions and macOS.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/8/82/Gnu-bash-logo.svg",
        "location": "Free Software Foundation, USA",
        "benefits": "Ubiquitous, powerful scripting, direct system interaction.",
        "related": "Bourne Shell, Zsh, Ksh"
    },
    "BASIC": {
        "creator": "John G. Kemeny, Thomas E. Kurtz",
        "date": "1964",
        "description": "Beginner's All-purpose Symbolic Instruction Code. Designed to emphasize ease of use.",
        "image": "https://upload.wikimedia.org/wikipedia/en/thumb/5/52/Kemeny_and_Kurtz.jpg/300px-Kemeny_and_Kurtz.jpg",
        "logo": "logos/BASIC.png",
        "location": "Dartmouth College, USA",
        "benefits": "Easy to learn, interactive, historical significance.",
        "related": "Fortran, Visual Basic"
    },
    "Wren": {
        "creator": "Bob Nystrom",
        "date": "2013",
        "description": "A small, fast, class-based concurrent scripting language. Designed for embedding in applications, it is object-oriented and comparable in speed to Lua.",
        "image": "https://wren.io/logo.png",
        "logo": "logos/Wren.png",
        "location": "USA",
        "benefits": "High performance, small footprint, clean syntax.",
        "related": "Lua, Smalltalk, Dart, JavaScript"
    },
    "Red": {
        "creator": "Nenad Rakocevic",
        "date": "2011",
        "description": "A functional and symbolic language inspired by REBOL. It is a full-stack language, capable of both high-level scripting and low-level system programming.",
        "image": "https://static.red-lang.org/red-logo.png",
        "logo": "logos/Red.png",
        "location": "Global",
        "benefits": "Full-stack, symbolic, cross-platform.",
        "related": "Rebol, Lisp, Lua"
    },
    "Pike": {
        "creator": "Fredrik Hübinette, Per Hedbor",
        "date": "1994",
        "description": "A general-purpose, high-level, cross-platform, dynamic programming language with a C-like syntax. It features garbage collection, advanced data types, and first-class anonymous functions.",
        "image": "Languages/Pike/Media/Pike_1766237418806.png",
        "location": "Linköping, Sweden",
        "benefits": "C-like syntax, dynamic typing, concurrency.",
        "related": "LPC, C, C++, Python"
    },
    "Fennel": {
        "creator": "Calvin Rose (Project Lead)",
        "date": "2016",
        "description": "A programming language that brings Lisp syntax and macros to the Lua architecture. It compiles to Lua and runs with zero overhead, offering full Lua compatibility.",
        "image": "Languages/Fennel/Media/Fennel_1766237539834.png",
        "location": "Global",
        "benefits": "Lisp syntax, Lua speed, zero overhead, embeddable.",
        "related": "Lua, Lisp, Clojure"
    },
    "Janet": {
        "creator": "Calvin Rose",
        "date": "2017",
        "description": "A functional and imperative programming language designed for system scripting, expressive automation, and embedding. It features a built-in PEG engine and Lisp-like macros.",
        "image": "Languages/Janet/Media/Janet_1766238064569.png",
        "location": "USA",
        "benefits": "Embeddable, PEG parsing, structural editing.",
        "related": "Lua, Clojure, Lisp, C"
    },
    "Io": {
        "creator": "Steve Dekorte",
        "date": "2002",
        "description": "A pure object-oriented, prototype-based programming language. In Io, everything is an object, and it uses a message-passing model similar to Smalltalk but with a prototype-based object model like Self.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/Io_Programming_Language_Logo.svg/1200px-Io_Programming_Language_Logo.svg.png",
        "location": "USA",
        "benefits": "Simplicity, flexibility, concurrency (coroutines).",
        "related": "Smalltalk, Self, Lisp, Lua"
    },
    "Factor": {
        "creator": "Slava Pestov",
        "date": "2003",
        "description": "A stack-oriented, concatenative, multi-paradigm programming language. It combines the flexibility of dynamic typing with the performance of native compilation.",
        "image": "https://concatenative.org/mediawiki/images/Factor_logo.png",
        "location": "USA",
        "benefits": "Concatenative, metaprogramming, performance.",
        "related": "Forth, Joy, Lisp, Smalltalk"
    },
    "Icon": {
        "creator": "Ralph Griswold",
        "date": "1977",
        "description": "A very high-level programming language based on goal-directed execution and string scanning. It makes extensive use of generators and backtracking.",
        "image": "https://www2.cs.arizona.edu/icon/logo/icon.gif",
        "location": "University of Arizona, USA",
        "benefits": "String processing, goal-directed execution, generators.",
        "related": "SNOBOL, Python, Unicon"
    }
};
