import os
import re
import glob
import argparse
import json
import datetime

def parse_run_file(filepath):
    with open(filepath, 'r', errors='ignore') as f:
        content = f.read()
    
    iterations = re.findall(r'Solved in Iterations=(\d+)', content)
    times = re.findall(r'Seconds to process ([\d.]+)', content)
    cpu_user = re.findall(r'([\d.]+)\s+user', content)
    cpu_sys = re.findall(r'([\d.]+)\s+sys', content)
    max_rss = re.findall(r'(\d+)\s+maximum resident set size', content)
    
    return iterations, times, cpu_user, cpu_sys, max_rss

def load_history(filepath="benchmark_history.json"):
    if os.path.exists(filepath):
        try:
            with open(filepath, 'r') as f:
                return json.load(f)
        except json.JSONDecodeError:
            return []
    return []

def save_history(history, filepath="benchmark_history.json"):
    with open(filepath, 'w') as f:
        json.dump(history, f, indent=2)

def generate_html(results, history=[], highlight=None, personalities={}, language_metadata={}):
    # Determine max number of matrices to create headers
    # Use mode (most common length) to avoid outliers skewing the count
    lengths = [len(data['times']) for lang, data in results.items() if not results[lang].get('is_placeholder')]
    if lengths:
        import statistics
        try:
            max_matrices = statistics.mode(lengths)
        except:
            # If multiple modes or error, fallback to max but capped at 10 to be safe?
            # Or just max.
            max_matrices = max(lengths)
    else:
        max_matrices = 0
    
    # Sort results by total time (fastest first)
    sorted_results = sorted(results.items(), key=lambda item: sum(map(float, item[1]['times'])) if item[1]['times'] else float('inf'))

    # Language Histories
    language_histories = {
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
    }

    html = """
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
            text-align: left;
            padding: 15px;
            color: var(--secondary);
            font-size: 0.9em;
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
        
        /* Modal */
        .modal-overlay {
            position: fixed;
            top: 0; left: 0; right: 0; bottom: 0;
            background: rgba(0,0,0,0.8);
            display: none;
            justify-content: center;
            align-items: center;
            z-index: 2000;
            backdrop-filter: blur(5px);
        }
        .modal {
            background: var(--surface);
            border: 1px solid var(--primary);
            padding: 30px;
            max-width: 500px;
            border-radius: 8px;
            box-shadow: 0 0 30px rgba(0, 255, 157, 0.2);
            position: relative;
        }
        .modal h2 {
            color: var(--primary);
            margin-top: 0;
            font-family: 'JetBrains Mono', monospace;
        }
        .modal p {
            line-height: 1.6;
        }
        .close-modal {
            position: absolute;
            top: 10px;
            right: 15px;
            color: #888;
            cursor: pointer;
            font-size: 1.5em;
        }
        .close-modal:hover {
            color: var(--danger);
        }
        
        /* Rotation for Descending Sort */
        .rotate-180 {
            transform: rotate(180deg);
            display: inline-block; /* Ensure transform works */
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
        .cell-score {
            position: absolute;
            top: 2px;
            right: 4px;
            color: #ff9900;
            font-size: 0.75em;
            font-weight: bold;
            opacity: 0.9;
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
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
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
    
    <div style="width: 90%; margin: 0 auto 40px auto; background: #16161e; padding: 20px; border-radius: 8px; border: 1px solid #2a2a35;">
        <canvas id="historyChart"></canvas>
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
"""
    for i in range(max_matrices):
        html += f"""<th>
            Matrix {i+1}
            <div class="sort-group">
                <button class="sort-btn" onclick="sortMatrix({i}, 'time', this)" title="Sort by Time">S</button>
                <button class="sort-btn" onclick="sortMatrix({i}, 'iters', this)" title="Sort by Iterations">I</button>
                <button class="sort-btn" onclick="sortMatrix({i}, 'mem', this)" title="Sort by Memory">M</button>
                <button class="sort-btn" onclick="sortMatrix({i}, 'score', this)" title="Sort by Score">Sc</button>
            </div>
        </th>"""
    # Find C baseline
    c_results = results.get('C', {})
    c_times = c_results.get('times', [])
    
    html += """<th>
        Total Time
        <div class="sort-group">
            <button class="sort-btn" onclick="sortRows('time', this)" title="Sort by Total Time">S</button>
        </div>
    </th>
    </tr></thead><tbody>"""

    for lang, data in sorted_results:
        times = data['times']
        iters = data['iters']
        mems = data['max_rss']
        
        total_time = sum(map(float, times)) if times else 0
        total_iters = sum(map(int, iters)) if iters else 0
        max_mem = max(map(int, mems)) if mems else 0
        
        # Calculate Efficiency Score (Memory MB / Seconds)
        mem_mb_total = max_mem / 1024 / 1024
        if total_time > 0:
            efficiency_score = mem_mb_total / total_time
        else:
            efficiency_score = 0

        # Suspect Logic
        # Flag if we report fewer or more matrices than expected (incomplete or over-complete run)
        is_suspect = len(times) != max_matrices
        
        # Handle placeholders for sorting
        if results[lang].get('is_placeholder'):
            sort_time = 999999
            sort_mem = -1
            sort_iters = -1
            sort_score = -1
            is_suspect = True # Placeholders are always suspect
        else:
            sort_time = total_time
            sort_mem = max_mem
            sort_iters = total_iters
            sort_score = efficiency_score
        
        row_class = "active-row" if lang == highlight else ""
        if results[lang].get('is_placeholder'):
            row_class += " placeholder-row"
        if is_suspect:
            row_class += " suspect"
        
        quote = personalities.get('Standard', {}).get(lang, "A mystery wrapped in code.")
        quote = quote.replace("'", "&apos;")
        
        # Add Efficiency to quote for tooltip
        quote += f" Efficiency: {efficiency_score:.2f} MB/s"
        
        # Get Metadata
        meta = language_metadata.get(lang, {})
        year = meta.get('date', '0000')
        display_name = "C#" if lang == "C_Sharp" else lang
        
        # Build row with data attributes for each matrix
        matrix_data_attrs = ""
        for i in range(max_matrices):
            t = float(times[i]) if i < len(times) else 999999
            it = int(iters[i]) if i < len(iters) else -1
            m = int(mems[i]) if i < len(mems) else -1
            
            # Calculate Score for this matrix (vs C)
            score = 0
            if i < len(times) and i < len(c_times) and float(c_times[i]) > 0:
                 score = t / float(c_times[i])
            elif i < len(times):
                 score = 0
            
            matrix_data_attrs += f" data-m{i}-time='{t}' data-m{i}-iters='{it}' data-m{i}-mem='{m}' data-m{i}-score='{score:.2f}'"

        html += f"<tr class='{row_class}' data-quote='{quote}' data-lang='{lang}' data-year='{year}' data-time='{sort_time}' data-mem='{sort_mem}' data-iters='{sort_iters}' data-score='{sort_score}' {matrix_data_attrs}><td class='lang-col' onclick='showLanguageDetails(\"{lang}\")'><div>{display_name}</div><div class='lang-year'>{year}</div></td>"
        
        for i in range(max_matrices):
            if i < len(times):
                t = float(times[i])
                it = iters[i] if i < len(iters) else "?"
                c_user = float(data['cpu_user'][i]) if i < len(data['cpu_user']) else 0
                c_sys = float(data['cpu_sys'][i]) if i < len(data['cpu_sys']) else 0
                mem = int(data['max_rss'][i]) if i < len(data['max_rss']) else 0
                mem_mb = mem / 1024 / 1024
                
                html += f"<td><div class='cell-content'><div class='time' title='Wall Clock Time'>{t:.5f}s</div><div class='meta'><span title='Iterations'>#{it}</span><span title='Memory'>{mem_mb:.1f}M</span></div></div></td>"
            else:
                html += "<td><span style='color: #333'>-</span></td>"
        html += f"<td class='total-time'><div style='display:flex;flex-direction:column;align-items:flex-end;'><div>{total_time:.4f}s</div><div style='font-size:0.6em;color:#5c5c66;'>{total_iters:,} iters</div></div></td></tr>"

    html += """
        </tbody></table></div>
        <script>
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
                    // Default directions: time, lang, year, score ascending; mem, iters descending
                    if (metric === 'mem' || metric === 'iters') currentSort.dir = -1;
                    else currentSort.dir = 1;
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
                        // time, mem, iters, score
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
                    // Default directions: time, score ascending; iters, mem descending
                    if (metric === 'time' || metric === 'score') currentSort.dir = 1;
                    else currentSort.dir = -1;
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
                const rows = document.querySelectorAll('tbody tr');
                const isActive = btn.classList.contains('active');
                
                if (isActive) {
                    // Reset: Show all (Imposters visible)
                    rows.forEach(row => row.style.display = '');
                    btn.classList.remove('active');
                    btn.innerText = "Hide Imposters";
                } else {
                    // Filter: Hide imposters
                    rows.forEach(row => {
                        if (row.classList.contains('placeholder-row')) {
                            row.style.display = 'none';
                        } else {
                            row.style.display = '';
                        }
                    });
                    btn.classList.add('active');
                    btn.innerText = "Show Imposters";
                }
            }
            
            // History Modal
            function showHistory(el) {
                const row = el.closest('tr');
                const lang = row.getAttribute('data-lang');
                const history = row.getAttribute('data-history');
                
                document.getElementById('modal-title').innerText = lang;
                document.getElementById('modal-content').innerText = history;
                document.getElementById('modal-overlay').style.display = 'flex';
                
                // Stop tooltip from showing
                document.getElementById('tooltip').style.display = 'none';
            }
            
            function closeModal(event) {
                if (event.target === document.getElementById('langModal') || event.target.classList.contains('modal-close')) {
                    document.getElementById('langModal').style.display = 'none';
                }
            }

            // Methodology Modal Logic
            function showMethodology() {
                document.getElementById('methodModal').style.display = 'flex';
            }

            function closeMethodology(event) {
                if (event.target === document.getElementById('methodModal') || event.target.classList.contains('modal-close')) {
                    document.getElementById('methodModal').style.display = 'none';
                }
            }

            // Tooltip logic
            const personalities = __PERSONALITIES_JSON__;
            const languageMetadata = __LANGUAGE_METADATA_JSON__;
            let currentPersonality = "Neuromancer";

            function changePersonality() {
                const selector = document.getElementById('personality-selector');
                currentPersonality = selector.value;
                
                // Update Intro Text
                const intro = document.getElementById('personality-intro');
                intro.innerText = personalityIntros[currentPersonality] || personalityIntros["Standard"];
                
                // Update Tooltips
                const rows = document.querySelectorAll('tbody tr');
                rows.forEach(row => {
                    const lang = row.getAttribute('data-lang');
                    const quote = personalities[currentPersonality][lang] || personalities[currentPersonality]['default'];
                    row.setAttribute('data-quote', quote);
                });
            }
            
            // Auto-scroll to active row
            document.addEventListener("DOMContentLoaded", function() {
                const activeRow = document.querySelector(".active-row");
                if (activeRow) {
                    activeRow.scrollIntoView({ behavior: "smooth", block: "center" });
                }
                
                changePersonality(); // Set initial intro text and tooltip quotes
                
                const tooltip = document.getElementById('tooltip');
                const rows = document.querySelectorAll('tbody tr');
                
                rows.forEach(row => {
                    row.addEventListener('mouseenter', (e) => {
                        const quote = row.getAttribute('data-quote');
                        if (quote) {
                            tooltip.innerHTML = quote;
                            tooltip.style.display = 'block';
                        }
                    });
                    
                    row.addEventListener('mousemove', (e) => {
                        tooltip.style.left = (e.clientX + 20) + 'px';
                        tooltip.style.top = (e.clientY + 20) + 'px';
                    });
                    
                    row.addEventListener('mouseleave', () => {
                        tooltip.style.display = 'none';
                    });
                });
            });
        </script>
        <script>
            // Chart.js Logic
            const historyData = __HISTORY_JSON__;
            
            if (historyData.length > 0) {
                const ctx = document.getElementById('historyChart').getContext('2d');
                
                // 1. Extract all unique languages
                const allLangs = new Set();
                historyData.forEach(entry => {
                    Object.keys(entry.results).forEach(lang => allLangs.add(lang));
                });

                // 2. Calculate Ranks for each timestamp
                // Structure: { timestamp: "...", ranks: { "C": 1, "Rust": 2, ... } }
                const rankedHistory = historyData.map(entry => {
                    // Calculate total time for each lang in this entry
                    const runTimes = [];
                    Object.entries(entry.results).forEach(([lang, res]) => {
                        if (res && res.times && res.times.length > 0) {
                            const totalTime = res.times.reduce((a, b) => parseFloat(a) + parseFloat(b), 0);
                            // Filter out placeholders or failed runs (extremely high times) if needed
                            // For now, we rank everything that has a time.
                            runTimes.push({ lang, time: totalTime });
                        }
                    });

                    // Sort by time (ascending) -> Rank 1 is fastest
                    runTimes.sort((a, b) => a.time - b.time);

                    const ranks = {};
                    runTimes.forEach((item, index) => {
                        ranks[item.lang] = index + 1;
                    });

                    return {
                        timestamp: entry.timestamp,
                        ranks: ranks
                    };
                });

                // 3. Filter for Top Contenders (from the LATEST run)
                // We don't want 50 lines. Let's take the top 10 from the most recent run.
                const latestRanks = rankedHistory[rankedHistory.length - 1].ranks;
                const topContenders = Object.entries(latestRanks)
                    .sort(([, rankA], [, rankB]) => rankA - rankB)
                    .slice(0, 15) // Top 15
                    .map(([lang]) => lang);

                // 4. Build Datasets
                const datasets = [];
                // Carnival/Neon Palette
                const colors = [
                    '#FF0055', // Neon Red
                    '#00FF9D', // Neon Green
                    '#00B8FF', // Neon Blue
                    '#FFCC00', // Neon Yellow
                    '#9D00FF', // Neon Purple
                    '#FF6600', // Neon Orange
                    '#00FFFF', // Cyan
                    '#FF00FF', // Magenta
                    '#FFFF00', // Yellow
                    '#FFFFFF'  // White
                ];

                let colorIdx = 0;
                
                topContenders.forEach(lang => {
                    const dataPoints = rankedHistory.map(entry => {
                        return entry.ranks[lang] || null; // null if it didn't run that time
                    });

                    datasets.push({
                        label: lang,
                        data: dataPoints,
                        borderColor: colors[colorIdx % colors.length],
                        backgroundColor: colors[colorIdx % colors.length],
                        borderWidth: 3,
                        pointRadius: 4,
                        pointHoverRadius: 8,
                        pointBackgroundColor: '#0d0d12', // Dark center for "mechanical" look
                        pointBorderWidth: 2,
                        fill: false,
                        stepped: 'middle', // MECHANICAL CARNIVAL MOVEMENT
                        tension: 0 // No curves
                    });
                    colorIdx++;
                });

                const labels = historyData.map(entry => {
                    const date = new Date(entry.timestamp);
                    return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
                });

                new Chart(ctx, {
                    type: 'line',
                    data: {
                        labels: labels,
                        datasets: datasets
                    },
                    options: {
                        responsive: true,
                        interaction: {
                            mode: 'index',
                            intersect: false,
                        },
                        plugins: {
                            title: {
                                display: true,
                                text: 'MECHANICAL DERBY: TOP 15 CONTENDERS',
                                color: '#e0e0e0',
                                font: { size: 18, family: 'JetBrains Mono', weight: 'bold' },
                                padding: 20
                            },
                            legend: {
                                position: 'bottom',
                                labels: { 
                                    color: '#e0e0e0', 
                                    font: { family: 'JetBrains Mono' },
                                    boxWidth: 12,
                                    usePointStyle: true
                                }
                            },
                            tooltip: {
                                backgroundColor: 'rgba(13, 13, 18, 0.9)',
                                titleColor: '#00ff9d',
                                bodyFont: { family: 'JetBrains Mono' },
                                callbacks: {
                                    label: function(context) {
                                        return `${context.dataset.label}: Rank #${context.raw}`;
                                    }
                                }
                            }
                        },
                        scales: {
                            x: {
                                ticks: { color: '#5c5c66', font: { family: 'JetBrains Mono' } },
                                grid: { color: '#2a2a35', drawBorder: false }
                            },
                            y: {
                                reverse: true, // Rank 1 at top
                                min: 1,
                                max: 16, // Give a little space at bottom
                                ticks: { 
                                    color: '#5c5c66', 
                                    stepSize: 1,
                                    font: { family: 'JetBrains Mono' },
                                    callback: function(value) { if (value % 1 === 0) return '#' + value; }
                                },
                                grid: { color: '#2a2a35', drawBorder: false },
                                title: { display: true, text: 'RANKING', color: '#5c5c66', font: { family: 'JetBrains Mono' } }
                            }
                        }
                    }
                });
            }
        </script>
    </body></html>
    """
    html = html.replace("__HISTORY_JSON__", json.dumps(history))
    html = html.replace("__PERSONALITIES_JSON__", json.dumps(personalities))
    html = html.replace("__LANGUAGE_METADATA_JSON__", json.dumps(language_metadata))
    return html

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--highlight", help="Name of the solver to highlight")
    parser.add_argument("--save-history", action="store_true", help="Save current results to history")
    args = parser.parse_args()

    results = {}
    base_dir = "AI-2025"
    
    # Iterate over all subdirectories
    for entry in os.listdir(base_dir):
        dir_path = os.path.join(base_dir, entry)
        if os.path.isdir(dir_path):
            run_file = os.path.join(dir_path, "run.txt")
            run_file = os.path.join(dir_path, "run.txt")
            is_placeholder = os.path.exists(os.path.join(dir_path, "response_lazy.sh"))
            
            iterations = []
            times = []
            cpu_user = []
            cpu_sys = []
            max_rss = []
            
            if os.path.exists(run_file):
                iterations, times, cpu_user, cpu_sys, max_rss = parse_run_file(run_file)
            
            if times or is_placeholder:
                results[entry] = {
                    'iters': iterations,
                    'times': times,
                    'cpu_user': cpu_user,
                    'cpu_sys': cpu_sys,
                    'max_rss': max_rss,
                    'is_placeholder': is_placeholder
                }
            elif entry == args.highlight:
                # If highlighted but no run.txt yet, add placeholder
                results[entry] = {
                    'iters': [],
                    'times': [],
                    'cpu_user': [],
                    'cpu_sys': [],
                    'max_rss': [],
                    'is_placeholder': True
                }
    
    quotes = {
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
        "Logo": "Turtle graphics. Teaching kids to code.",
        "Scheme": "Minimalist Lisp. Academic purity.",
        "Racket": "A programmable programming language. Lisp descendant.",
        "Groovy": "Java's dynamic cousin.",
        "Haxe": "Cross-platform toolkit. Compiles to everything.",
        "Jq": "JSON processor. It slices and dices JSON.",
        "Bc": "Arbitrary precision calculator. It does math.",
        "Basic": "Beginner's All-purpose Symbolic Instruction Code. Where we all started.",
        "Algol68": "The ancestor. It defined the block structure.",
        "APL": "A Programming Language. cryptic symbols. Array processing.",
        "Jupyter": "Interactive notebooks. Data science playground.",
        "Matrices": "The input data. The puzzle itself.",
        "Matrices_Backup": "The backup.",
        "Matrices_Filtered": "The filter."
    }

    personalities = {
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
    }

    language_metadata = {
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
    }

    history = load_history()
    if args.save_history:
        # Create a snapshot
        snapshot = {
            "timestamp": datetime.datetime.now().isoformat(),
            "results": results
        }
        history.append(snapshot)
        # Keep only last 50 entries to avoid bloat
        if len(history) > 50:
            history = history[-50:]
        save_history(history)

    html_content = generate_html(results, history, highlight=args.highlight, personalities=personalities, language_metadata=language_metadata)
    
    with open("benchmark_report.html", "w") as f:
        f.write(html_content)
    
    print(f"Report generated: {os.path.abspath('benchmark_report.html')}")

    # Snapshot Logic
    snapshot_dir = "snapshots"
    if not os.path.exists(snapshot_dir):
        os.makedirs(snapshot_dir)
    
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    snapshot_path = os.path.join(snapshot_dir, f"benchmark_report_{timestamp}.html")
    
    with open(snapshot_path, "w") as f:
        f.write(html_content)
    
    print(f"Snapshot saved: {os.path.abspath(snapshot_path)}")

if __name__ == "__main__":
    main()
