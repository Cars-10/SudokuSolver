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
    "Assembly": "1949. The lowest level of human-readable code. Direct control over hardware, where every byte matters.",
    "Awk": "1977. Aho, Weinberger, and Kernighan at Bell Labs. The original text processing powerhouse.",
    "Bash": "1989. Brian Fox. The 'Bourne Again SHell', the glue that holds the Linux world together.",
    "Basic": "1964. Kemeny and Kurtz at Dartmouth. Designed to be easy for students, it launched the home computing revolution.",
    "Befunge": "1993. Chris Pressey. A two-dimensional esoteric language designed to be difficult to compile.",
    "Brainfuck": "1993. Urban Müller. A minimalist esoteric language with only 8 commands, designed to challenge programmers.",
    "C": "1972. Dennis Ritchie at Bell Labs. The mother of modern languages, balancing high-level logic with low-level power.",
    "C++": "1985. Bjarne Stroustrup. 'C with Classes'. Added object-oriented features to C, becoming the standard for systems programming.",
    "C_Sharp": "2000. Microsoft (Anders Hejlsberg). A modern, object-oriented language for the .NET framework.",
    "Clojure": "2007. Rich Hickey. A modern Lisp dialect for the JVM, emphasizing immutability and functional programming.",
    "Cobol": "1959. Grace Hopper. 'Common Business-Oriented Language'. Verbose and resilient, it still runs the world's banking systems.",
    "CoffeeScript": "2009. Jeremy Ashkenas. 'It's just JavaScript'. Added syntactic sugar to JS, inspiring modern ES6 features.",
    "CommonLisp": "1984. A standardization of various Lisp dialects. The programmable programming language.",
    "Crystal": "2014. Ary Borenszweig. Ruby-like syntax with C-like performance and static typing.",
    "D": "2001. Walter Bright. Designed to re-engineer C++, improving safety and productivity.",
    "Dart": "2011. Google. Optimized for UI development, now the heart of Flutter.",
    "Elixir": "2011. José Valim. Built on the Erlang VM, bringing Ruby-like joy to distributed, fault-tolerant systems.",
    "EmacsLisp": "1985. Richard Stallman. The scripting language that makes Emacs an operating system disguised as an editor.",
    "Erlang": "1986. Ericsson. Built for massive concurrency and fault tolerance in telecom systems.",
    "F_Sharp": "2005. Microsoft (Don Syme). A functional-first language on .NET, inspired by OCaml.",
    "Fortran": "1957. John Backus at IBM. The first high-level language, still dominating scientific computing.",
    "Go": "2009. Google (Griesemer, Pike, Thompson). Designed for simplicity, concurrency, and scalability in the cloud era.",
    "Groovy": "2003. James Strachan. A dynamic language for the JVM, blending Java with Python/Ruby features.",
    "Haskell": "1990. A purely functional language named after logician Haskell Curry. Famous for its type system and laziness.",
    "Java": "1995. James Gosling at Sun Microsystems. 'Write Once, Run Anywhere'. Revolutionized enterprise computing.",
    "JavaScript": "1995. Brendan Eich at Netscape. Created in 10 days. It now runs the web.",
    "Julia": "2012. Jeff Bezanson et al. High-performance numerical analysis, aiming to solve the 'two-language problem'.",
    "Kotlin": "2011. JetBrains. A concise, modern alternative to Java, now the standard for Android.",
    "Logo": "1967. Wally Feurzeig and Seymour Papert. Famous for its 'Turtle' graphics, teaching kids to think like computers.",
    "Lua": "1993. PUC-Rio (Brazil). Lightweight and embeddable, the scripting king of the gaming industry.",
    "M4": "1977. Kernighan and Ritchie. A powerful macro processor, often hidden deep within build systems.",
    "Nim": "2008. Andreas Rumpf. Python-like syntax with C-like performance and metaprogramming.",
    "Objective-C": "1984. Brad Cox. Added Smalltalk-style messaging to C. The heart of Apple's ecosystem for decades.",
    "OCaml": "1996. INRIA. Industrial strength functional programming with a powerful type system.",
    "Octave": "1988. A high-level language for numerical computations, largely compatible with MATLAB.",
    "Pascal": "1970. Niklaus Wirth. Designed to teach structured programming and data structuring.",
    "Perl": "1987. Larry Wall. 'The Swiss Army chainsaw of scripting'. Famous for text processing and one-liners.",
    "PHP": "1995. Rasmus Lerdorf. 'Personal Home Page'. The engine behind a massive portion of the web (WordPress, Facebook).",
    "PostScript": "1982. Adobe (Warnock and Geschke). A stack-based language for describing pages, the ancestor of PDF.",
    "PowerShell": "2006. Microsoft. A task automation shell built on .NET, treating everything as objects.",
    "Prolog": "1972. Colmerauer and Kowalski. Logic programming. You describe facts and rules; the computer finds the solution.",
    "Python": "1991. Guido van Rossum. 'Executable pseudocode'. Named after Monty Python, it's now the world's most popular language.",
    "R": "1993. Ross Ihaka and Robert Gentleman. Built for statistics and data visualization.",
    "Racket": "1995. Matthew Flatt. A descendant of Scheme, designed for creating new programming languages.",
    "Rexx": "1979. Mike Cowlishaw at IBM. A structured high-level language designed for ease of learning and reading.",
    "Ruby": "1995. Yukihiro Matsumoto. Designed for developer happiness. 'A programmer's best friend'.",
    "Rust": "2010. Graydon Hoare at Mozilla. Memory safety without garbage collection. Loved for its reliability.",
    "Scala": "2004. Martin Odersky. Blends object-oriented and functional programming on the JVM.",
    "Scheme": "1975. Sussman and Steele. A minimalist dialect of Lisp. 'The Little Schemer'.",
    "Sed": "1974. Lee E. McMahon at Bell Labs. A stream editor for filtering and transforming text.",
    "Smalltalk": "1972. Alan Kay at Xerox PARC. The father of object-oriented programming and GUIs.",
    "SNOBOL": "1962. Bell Labs. A string-oriented symbolic language, pioneering pattern matching.",
    "SQL": "1974. Chamberlin and Boyce at IBM. The standard language for relational database management.",
    "Swift": "2014. Apple. A modern, safe, and fast replacement for Objective-C.",
    "Tcl": "1988. John Ousterhout. 'Tool Command Language'. Known for its simplicity and integration with Tk.",
    "TypeScript": "2012. Microsoft (Anders Hejlsberg). JavaScript with syntax for types. 'JavaScript that scales'.",
    "Vala": "2006. Jürg Billeter. Brings modern language features to C via the GObject system.",
    "Verilog": "1984. Gateway Design Automation. A hardware description language for modeling electronic systems.",
    "VHDL": "1980. US DoD. VHSIC Hardware Description Language. Ada-based syntax for hardware modeling.",
    "Vimscript": "1991. Bram Moolenaar. The scripting language built into the Vim editor.",
    "VisualBasic": "1991. Microsoft. Derived from BASIC, it made Windows GUI programming accessible to everyone.",
    "WebAssembly": "2017. W3C. A binary instruction format for a stack-based virtual machine, bringing near-native speed to the web.",
    "Zig": "2016. Andrew Kelley. A modern system language intended to replace C. No hidden control flow, no allocations.",
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
    "Standard": quotes,
    "Neuromancer": quotes,
    "The Jockey": {
        "C": "AND THEY'RE OFF! C takes the lead with raw speed! Look at that optimization!",
        "C++": "C++ is thundering down the track! A heavy beast but unstoppable once it gets going!",
        "Rust": "Rust is challenging for the lead! Safe, fast, and fearless! What a machine!",
        "Go": "Go is keeping a steady pace! Efficient, reliable, not breaking a sweat!",
        "Python": "Python is trailing the pack! It's not about speed, folks, it's about style!",
        "JavaScript": "JavaScript is all over the place! Is it running? Is it crashing? It's chaos on the track!",
        "Java": "Java is a steady runner! Not the fastest, but it will finish the race!",
        "PHP": "PHP is... still running! It refuses to quit! What a survivor!",
        "Assembly": "Assembly is a rocket! Pure speed! Dangerous but exciting!",
        "Fortran": "The old veteran Fortran is showing the young ones how it's done!",
        "default": "A dark horse enters the race!"
    },
    "The Professor": {
        "C": "A classic example of low-level efficiency. Note the minimal overhead.",
        "C++": "Complex, yes, but the zero-cost abstractions are theoretically sound.",
        "Rust": "A fascinating case study in memory safety without garbage collection.",
        "Go": "An exercise in pragmatic engineering. Simple, concurrent, effective.",
        "Python": "Optimal for rapid prototyping, though the runtime overhead is significant.",
        "JavaScript": "Dynamically typed and event-driven. A pedagogical nightmare.",
        "Java": "The embodiment of object-oriented strictness. Verbose but structured.",
        "PHP": "A pragmatic solution for web development, despite its inconsistencies.",
        "Assembly": "The closest we get to the metal. Educational but impractical for most.",
        "Fortran": "Historically significant. The foundation of numerical computing.",
        "default": "An interesting specimen for further study."
    },
    "The Surfer": {
        "C": "Whoa, C is blazing fast, dude! Like catching the perfect wave!",
        "C++": "C++ is gnarly, man! Big, powerful, and kinda scary!",
        "Rust": "Rust is totally radical! Safe vibes only, no memory leaks here!",
        "Go": "Go is chill, bro. Simple flow, good vibes.",
        "Python": "Python is super chill. Easy riding, no stress.",
        "JavaScript": "JS is wild, man! Like a choppy sea, but you ride it anyway!",
        "Java": "Java is like a longboard. Big, steady, cruises for days.",
        "PHP": "PHP is scrappy, dude! It keeps paddling no matter what!",
        "Assembly": "Assembly is hardcore! Shredding the bits!",
        "Fortran": "Old school cool. Respect the elders, man.",
        "default": "Just catching the vibes, man."
    }
};


const methodologyTexts: Record<string, string> = {
    "Standard": `
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
    `,
    "Neuromancer": `
        <p>The construct uses a normalized metric. We compare the raw data against the C baseline. It's a flatline comparison.</p>
        <h3 style="color: var(--secondary);">Target: C</h3>
        <p>The <strong>C</strong> code is the icebreaker. It sets the standard (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = (Time) / (C_Time)
        </div>
        <p>Below 1.0? You're hacking the Gibson. Above? You're just a tourist.</p>
    `,
    "The Jockey": `
        <p>We're handicapping the field! The Total Score compares everyone to the speed demon, C.</p>
        <h3 style="color: var(--secondary);">The Pace Car: C</h3>
        <p><strong>C</strong> is setting the pace at 1.0. Try to keep up!</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = (Time) / (C Time)
        </div>
        <p>If you're over 1.0, you're eating dust! Under 1.0? You're a thoroughbred!</p>
    `,
    "The Professor": `
        <p>The scoring methodology employs a normalization technique. The C implementation serves as the control group.</p>
        <h3 style="color: var(--secondary);">Control: C</h3>
        <p><strong>C</strong> (1.0) provides the baseline for computational efficiency.</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = T(solver) / T(c)
        </div>
        <p>Values < 1.0 indicate superior optimization. Values > 1.0 suggest overhead.</p>
    `,
    "The Surfer": `
        <p>So, like, the Score is just comparing how chill the run was against C.</p>
        <h3 style="color: var(--secondary);">The Big Kahuna: C</h3>
        <p><strong>C</strong> is the wave we're measuring against (1.0).</p>
        <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
            Score = Time / C_Time
        </div>
        <p>If it's 1.0, it's tubular. Lower? Totally radical. Higher? Wipeout city, man.</p>
    `
};

const languageMetadata: Record<string, any> = {
    "C": {
        "creator": "Dennis Ritchie",
        "date": "1972",
        "description": "The foundational language of modern computing. Created at Bell Labs to develop the Unix operating system. Known for its efficiency and control.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/23/Dennis_Ritchie_2011.jpg"
    },
    "C++": {
        "creator": "Bjarne Stroustrup",
        "date": "1985",
        "description": "An extension of C with object-oriented features. Dominates game development and high-performance systems.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/0/0e/Bjarne-stroustrup.jpg"
    },
    "Python": {
        "creator": "Guido van Rossum",
        "date": "1991",
        "description": "A high-level language emphasizing readability. The king of Data Science and AI.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/e/e2/Guido-portrait-2014-drc.jpg"
    },
    "Java": {
        "creator": "James Gosling",
        "date": "1995",
        "description": "Write Once, Run Anywhere. The backbone of enterprise software.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/1/14/James_Gosling_2008.jpg"
    },
    "JavaScript": {
        "creator": "Brendan Eich",
        "date": "1995",
        "description": "Created in 10 days. The language of the web, running on billions of devices.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/d/d1/Brendan_Eich_Mozilla_Foundation_official_photo.jpg"
    },
    "Rust": {
        "creator": "Graydon Hoare",
        "date": "2010",
        "description": "Focuses on safety and performance. Enforces memory safety without garbage collection.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/d/d5/Rust_programming_language_black_logo.svg"
    },
    "Go": {
        "creator": "Robert Griesemer, Rob Pike, Ken Thompson",
        "date": "2009",
        "description": "Created at Google to improve programming productivity in an era of multicore machines.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/23/Golang.png"
    },
    "PHP": {
        "creator": "Rasmus Lerdorf",
        "date": "1995",
        "description": "Originally 'Personal Home Page'. Powers a significant portion of the web (including WordPress).",
        "image": "https://upload.wikimedia.org/wikipedia/commons/2/2a/Rasmus_Lerdorf_at_Wikipedia_10.jpg"
    },
    "Ruby": {
        "creator": "Yukihiro Matsumoto",
        "date": "1995",
        "description": "Designed for developer happiness. Famous for the Ruby on Rails framework.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/7/76/Yukihiro_Matsumoto.JPG"
    },
    "Swift": {
        "creator": "Chris Lattner",
        "date": "2014",
        "description": "Apple's replacement for Objective-C. Safe, fast, and expressive.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/9/9d/Swift_logo.svg"
    },
    "Fortran": {
        "creator": "John Backus",
        "date": "1957",
        "description": "The first high-level programming language. Still dominates scientific computing.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/b/b8/John_Backus_2.jpg"
    },
    "Assembly": {
        "creator": "Kathleen Booth",
        "date": "1947",
        "description": "Low-level symbolic representation of machine code. As close to the metal as you can get.",
        "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f6/Kathleen_Booth.jpg/440px-Kathleen_Booth.jpg"
    }
};

// --- Logic ---

async function findSolvers(rootDir: string): Promise<string[]> {
    // Find all runMe_ai.sh files
    const pattern = path.join(rootDir, 'Manual', '*', 'runMe_ai.sh');
    const files = await glob(pattern);
    return files;
}

async function runSolver(scriptPath: string): Promise<SolverMetrics | null> {
    const solverDir = path.dirname(scriptPath);
    const solverName = path.basename(solverDir);

    console.log(`Running solver: ${solverName}`);

    try {
        const { stdout, stderr } = await execPromise(`./runMe_ai.sh`, { cwd: solverDir });

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
            border-top: 1px dashed #ffcc00;
            border-bottom: 1px dashed #ffcc00;
        }
        .suspect .lang-col {
            color: #ffcc00;
        }
        .suspect:hover td {
            box-shadow: 0 0 15px rgba(255, 204, 0, 0.2);
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
        
        /* Top Bar Layout */
        .top-bar {
            display: flex;
            justify-content: center;
            align-items: center;
            gap: 20px;
            margin-bottom: 20px;
            flex-wrap: wrap;
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
    
    <div style="width: 95%; margin: 0 auto 40px auto; background: #16161e; padding: 20px; border-radius: 8px; border: 1px solid #2a2a35; height: 500px; position: relative;">
        <div id="d3-chart-container" style="width: 100%; height: 100%;"></div>
    </div>
    
    <div class="top-bar">
        <div id="personality-intro" class="personality-intro">
            Welcome to the Sudoku Benchmark. Use the controls to sort data and analyze performance metrics across different languages.
        </div>
        <div class="controls">
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

        // Normalized Score (vs C)
        const cTotalTime = cTimes.reduce((a, b) => a + b, 0);
        const normalizedScore = (cTotalTime > 0) ? (totalTime / cTotalTime) : 0;

        // Suspect Logic
        const isSuspect = m.results.length !== maxMatrices;

        let rowClass = "";
        if (isSuspect) rowClass += " suspect";

        // Quote
        const quote = (personalities['Standard'] as any)[lang] || "A mystery wrapped in code.";
        const safeQuote = quote.replace(/'/g, "&apos;") + ` Efficiency: ${efficiencyScore.toFixed(2)} MB/s`;

        // Metadata
        const meta = languageMetadata[lang] || {};
        const year = meta.date || "0000";
        const displayName = lang === "C_Sharp" ? "C#" : lang;
        const historyText = (languageHistories[lang] || "Unknown.").replace(/'/g, "&apos;");

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

        html += `<tr class='${rowClass}' data-quote='${safeQuote}' data-history='${historyText}' data-lang='${lang}' data-year='${year}' data-time='${totalTime}' data-mem='${maxMem}' data-iters='${totalIters}' data-score='${normalizedScore.toFixed(2)}' ${matrixDataAttrs}>
            <td class='lang-col'>
                <div>${displayName}</div>
                <div class='lang-year'>${year}</div>
            </td>
            <td>
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

                html += `<td>
                    <div class="cell-content">

                        <div class="time" title="Wall Clock Time">${r.time.toFixed(5)}s</div>
                        <div class="meta">
                            <span title="Iterations">#${r.iterations}</span>
                            <span title="Memory">${memMb.toFixed(1)}M</span>
                        </div>
                    </div>
                </td>`;
            } else {
                html += `<td><span style='color: #333'>-</span></td>`;
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
                else intro.innerText = "Welcome to the Sudoku Benchmark.";

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
                    
                    const cellIndex = cell.cellIndex;
                    const maxMatrices = 5; 
                    
                    let content = "";
                    
                    if (cellIndex === 0) {
                        // Language Cell -> Show Metadata
                        const meta = languageMetadata[lang];
                        if (meta) {
                            content = '<strong style="color: var(--primary)">' + lang + '</strong><br>' +
                                '<span style="color: var(--secondary)">' + meta.creator + ' (' + meta.date + ')</span><br>' +
                                '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                                '<div style="max-width: 250px; white-space: normal;">' + meta.description + '</div>';
                        }
                    } else if (cellIndex >= 2 && cellIndex < 2 + maxMatrices) {
                        // Matrix Cell -> Detailed Metrics
                        const matrixIdx = cellIndex - 2;
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
                    } else {
                        // Score Cell, Total Time, etc. -> Show Narrator Quote
                        content = row.getAttribute('data-quote');
                    }

                    if (content) {
                        const tooltip = document.getElementById('tooltip');
                        tooltip.style.display = 'block';
                        tooltip.style.left = (e.pageX + 15) + 'px';
                        tooltip.style.top = (e.pageY + 15) + 'px';
                        tooltip.innerHTML = content;
                    }
                });
                
                cell.addEventListener('mouseleave', () => {
                    const tooltip = document.getElementById('tooltip');
                    tooltip.style.display = 'none';
                });
            });
            // Modal Logic
            function showLanguageDetails(lang) {
                const modal = document.getElementById('langModal');
                const meta = languageMetadata[lang];
                if (!meta) return;

                document.getElementById('modalImg').src = meta.image;
                document.getElementById('modalTitle').innerText = lang;
                document.getElementById('modalSubtitle').innerText = meta.creator + " • " + meta.date;
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
                const data = ${JSON.stringify(metrics)};
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
                // Find min and max time across all data
                let minTime = Infinity;
                let maxTime = 0;
                
                data.forEach(d => {
                    d.results.forEach(r => {
                        if (r.time > 0) {
                            if (r.time < minTime) minTime = r.time;
                            if (r.time > maxTime) maxTime = r.time;
                        }
                    });
                });
                
                // Buffer for log scale
                minTime = Math.max(minTime, 0.0001); 
                
                const y = d3.scaleLog()
                    .domain([minTime * 0.8, maxTime * 1.2])
                    .range([chartHeight, 0]);
                    
                svg.append("g")
                    .call(d3.axisLeft(y).ticks(5, ".4f"))
                    .selectAll("text")
                    .style("fill", "#e0e0e0")
                    .style("font-family", "JetBrains Mono");
                    
                svg.append("text")
                    .attr("text-anchor", "end")
                    .attr("transform", "rotate(-90)")
                    .attr("y", -45)
                    .attr("x", -10)
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
                    
                    // Path
                    svg.append("path")
                        .datum(solverData)
                        .attr("fill", "none")
                        .attr("stroke", color(solver.solver))
                        .attr("stroke-width", 2)
                        .attr("d", line)
                        .attr("class", "line-path")
                        .style("filter", \`drop-shadow(0 0 4px \${color(solver.solver)})\`);
                        
                    // Dots
                    svg.selectAll(".dot-" + solver.solver)
                        .data(solverData)
                        .enter().append("circle")
                        .attr("cx", d => x(d.matrix))
                        .attr("cy", d => y(Math.max(d.time, minTime)))
                        .attr("r", 4)
                        .attr("fill", "#16161e")
                        .attr("stroke", color(solver.solver))
                        .attr("stroke-width", 2)
                        .on("mouseover", function(event, d) {
                            d3.select(this).attr("r", 6).attr("fill", color(solver.solver));
                            
                            const tooltip = document.getElementById('tooltip');
                            tooltip.style.display = 'block';
                            tooltip.style.left = (event.pageX + 15) + 'px';
                            tooltip.style.top = (event.pageY + 15) + 'px';
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

    const solverScripts = await findSolvers(rootDir);
    const allMetrics: SolverMetrics[] = [];

    for (const script of solverScripts) {
        const metrics = await runSolver(script);
        if (metrics) {
            allMetrics.push(metrics);
        }
    }

    // Save metrics.json
    await fs.writeFile(metricsFile, JSON.stringify(allMetrics, null, 2));
    console.log(`Saved metrics to ${metricsFile}`);

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

    await page.screenshot({ path: screenshotPath, fullPage: true });

    await browser.close();
    console.log(`Screenshot saved to ${screenshotPath}`);
}

main().catch(console.error);
