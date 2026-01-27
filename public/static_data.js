/**
 * Static Persona Data for Sudoku Benchmark UI
 * Contains personalities, narratorIntros, and labels for all personas
 */

// ============================================================================
// NARRATOR INTROS - Welcome messages for each persona
// ============================================================================
const narratorIntros = {
  "Standard": "Welcome to the Polyglot Sudoku Benchmark. Click on any language for details. Use controls to sort and analyze performance metrics across implementations.",
  "Neuromancer": "The matrix has you, console cowboy. Jack in and trace the ICE protecting these data fortresses. Each language is a different kind of ice to crack.",
  "Jockey": "Welcome to the track, folks! These languages are lined up at the gate, ready to race through the matrices. Place your bets on which compiler crosses first!",
  "Professor": "Good morning, class. Today we examine computational efficiency across programming paradigms. Note how language design choices impact algorithmic performance.",
  "Surfer": "Dude! Check out these gnarly benchmarks! Each language is catching waves through the puzzle matrices. Some are shredding, others are wiping out!",
  "Matrix": "What you see is the residual self-image of your programming languages. The Matrix has you. Follow the white rabbit of performance metrics.",
  "Galactica": "All hands, action stations! Set Condition One throughout the fleet. These Cylon-fighting languages are ready for FTL jumps through the data.",
  "Star Trek": "Captain's log, stardate 47988.1. We're analyzing performance metrics across the Federation's programming languages. Fascinating diversity in efficiency.",
  "Star Wars": "A long time ago in a codebase far, far away... These languages wage eternal war between the light side of optimization and the dark side of bloat.",
  "BTTF": "Great Scott! We're going back to the future of computing! These benchmarks show which languages have the 1.21 gigawatts needed for time travel speeds.",
  "Babylon 5": "The Babylon Project was our last, best hope for peace between languages. A self-contained world analyzing benchmark data from across the galaxy.",
  "Expanse": "Beltalowda! These benchmarks show which languages can handle the hard vacuum of computational efficiency. Check the burn rates, kopeng.",
  "Terminator": "I need your clothes, your boots, and your benchmark data. These languages have been analyzed. There is no fate but what we compile.",
  "LotR": "One algorithm to solve them all, one benchmark to find them, one metrics file to bring them all and in the darkness bind them.",
  "Dune": "The spice must flow, and so must the data. These benchmarks reveal which languages control the computational melange of Arrakis.",
  "Buck Rogers": "Biddi-biddi-biddi! Welcome to the 25th century, Buck! These futuristic benchmarks show how languages perform in the year 2491.",
  "Flash Gordon": "Flash! Ah-ahh! Savior of the benchmarks! These languages battle Ming's inefficiency across the performance universe!",
  "Batman": "I'm Batman. These benchmarks are the hero Gotham deserves. Each language faces the darkness of computational complexity.",
  "Alien": "In space, no one can hear your code compile. These benchmarks track hostile language performance across LV-426's data matrices.",
  "Blade Runner": "I've seen things you people wouldn't believe. Benchmarks on fire off the shoulder of Orion. Time to analyze performance, like tears in rain.",
  "Farscape": "Frell! These benchmarks are more confusing than Moya's DRDs! Let's see which languages can survive the Uncharted Territories of performance.",
  "Apocalypse Now": "I love the smell of optimized code in the morning. These benchmarks... the horror... the horror of O(n²) complexity.",
  "Airplane": "Surely you can't be serious about these benchmarks? I am serious. And don't call me Shirley. These languages need vectors, Victor.",
  "Fast Times": "Alright Hamilton! These benchmarks are totally awesome to the max! Like, totally tubular performance metrics, dude!",
  "Tron": "Greetings, Programs! These benchmarks fight for the Users. Each language competes in the Game Grid of computational efficiency.",
  "Bill & Ted": "Excellent! These benchmarks are most triumphant! Be excellent to each language, and party on with these radical performance metrics, dudes!",
  "John Wick": "People keep asking if these benchmarks are back. Yeah, I'm thinking they're back. These languages have a lot of commits to answer for.",
  "Dark Knight": "Why do we fall? So we can learn to optimize. These benchmarks are not the hero we deserved, but the hero we needed."
};

// ============================================================================
// MISMATCH LABELS - What to call mismatched iterations per persona
// ============================================================================
const mismatchLabels = {
  "Standard": "Mismatches",
  "Neuromancer": "ICE Breaches",
  "Jockey": "False Starts",
  "Professor": "Anomalies",
  "Surfer": "Wipeouts",
  "Matrix": "Glitches",
  "Galactica": "DRADIS Contacts",
  "Star Trek": "Anomalies",
  "Star Wars": "Disturbances",
  "BTTF": "Paradoxes",
  "Babylon 5": "Shadow Contacts",
  "Expanse": "Deviations",
  "Terminator": "Resistance",
  "LotR": "Corruptions",
  "Dune": "Abominations",
  "Buck Rogers": "Malfunctions",
  "Flash Gordon": "Ming's Minions",
  "Batman": "Rogues",
  "Alien": "Xenomorphs",
  "Blade Runner": "Replicants",
  "Farscape": "Anomalies",
  "Apocalypse Now": "Casualties",
  "Airplane": "Mayday Calls",
  "Fast Times": "Bummers",
  "Tron": "Derezzed",
  "Bill & Ted": "Bogus Results",
  "John Wick": "Contracts",
  "Dark Knight": "Chaos Agents"
};

// ============================================================================
// ITERATION LABELS - Column header for iterations per persona
// ============================================================================
const iterationLabels = {
  "Standard": "Iterations",
  "Neuromancer": "Cycles",
  "Jockey": "Laps",
  "Professor": "Computations",
  "Surfer": "Waves",
  "Matrix": "Cycles",
  "Galactica": "DRADIS Sweeps",
  "Star Trek": "Sensor Sweeps",
  "Star Wars": "Midichlorians",
  "BTTF": "Gigawatts",
  "Babylon 5": "Jump Points",
  "Expanse": "Burns",
  "Terminator": "Cycles",
  "LotR": "Steps",
  "Dune": "Folds",
  "Buck Rogers": "Cycles",
  "Flash Gordon": "Blasts",
  "Batman": "Patrols",
  "Alien": "Scans",
  "Blade Runner": "Voight-Kampff",
  "Farscape": "Cycles",
  "Apocalypse Now": "Recon",
  "Airplane": "Vectors",
  "Fast Times": "Reps",
  "Tron": "Cycles",
  "Bill & Ted": "Loops",
  "John Wick": "Headshots",
  "Dark Knight": "Patrols"
};

// ============================================================================
// TIME LABELS - Column header for execution time per persona
// ============================================================================
const timeLabels = {
  "Standard": "Time",
  "Neuromancer": "Flatline Duration",
  "Jockey": "Race Time",
  "Professor": "Duration",
  "Surfer": "Ride Time",
  "Matrix": "Bullet Time",
  "Galactica": "Jump Prep",
  "Star Trek": "Warp Factor",
  "Star Wars": "Parsecs",
  "BTTF": "Time Travel",
  "Babylon 5": "Hyperspace",
  "Expanse": "Burn Time",
  "Terminator": "Skynet Cycles",
  "LotR": "Journey",
  "Dune": "Guild Transit",
  "Buck Rogers": "Time Warp",
  "Flash Gordon": "Flash Time",
  "Batman": "Dark Hours",
  "Alien": "Cryo Time",
  "Blade Runner": "Lifespan",
  "Farscape": "Arns",
  "Apocalypse Now": "Mission Time",
  "Airplane": "Flight Time",
  "Fast Times": "Period",
  "Tron": "Millicycles",
  "Bill & Ted": "Excellent Time",
  "John Wick": "Contract Time",
  "Dark Knight": "Night Watch"
};

// ============================================================================
// MEMORY LABELS - Column header for memory usage per persona
// ============================================================================
const memoryLabels = {
  "Standard": "Memory",
  "Neuromancer": "RAM Ice",
  "Jockey": "Fuel Load",
  "Professor": "Heap Allocation",
  "Surfer": "Board Wax",
  "Matrix": "Red Pills",
  "Galactica": "Tylium",
  "Star Trek": "Dilithium",
  "Star Wars": "Force",
  "BTTF": "Plutonium",
  "Babylon 5": "Quantium-40",
  "Expanse": "Epstein Drive",
  "Terminator": "Neural Net",
  "LotR": "Lembas",
  "Dune": "Spice",
  "Buck Rogers": "Power Cells",
  "Flash Gordon": "Energy",
  "Batman": "Bat-Resources",
  "Alien": "Hypersleep",
  "Blade Runner": "Memory Implants",
  "Farscape": "Moya's Energy",
  "Apocalypse Now": "Supplies",
  "Airplane": "Fuel",
  "Fast Times": "Brain Cells",
  "Tron": "Data Blocks",
  "Bill & Ted": "Brain Power",
  "John Wick": "Coins",
  "Dark Knight": "Wayne Resources"
};

// ============================================================================
// SCORE LABELS - Column header for score per persona
// ============================================================================
const scoreLabels = {
  "Standard": "Score",
  "Neuromancer": "Street Cred",
  "Jockey": "Odds",
  "Professor": "Grade",
  "Surfer": "Stoke Level",
  "Matrix": "Awakening",
  "Galactica": "Combat Rating",
  "Star Trek": "Efficiency",
  "Star Wars": "Force Rating",
  "BTTF": "Temporal Flux",
  "Babylon 5": "Ranger Rating",
  "Expanse": "Belter Score",
  "Terminator": "Threat Level",
  "LotR": "Fellowship",
  "Dune": "Prescience",
  "Buck Rogers": "Future Score",
  "Flash Gordon": "Hero Points",
  "Batman": "Justice Score",
  "Alien": "Survival Rate",
  "Blade Runner": "Humanity",
  "Farscape": "Moya Rating",
  "Apocalypse Now": "Mission Score",
  "Airplane": "Altitude",
  "Fast Times": "Coolness",
  "Tron": "User Points",
  "Bill & Ted": "Excellentness",
  "John Wick": "Body Count",
  "Dark Knight": "Gotham Score"
};

// ============================================================================
// PERSONALITIES - Language-specific quotes for each persona
// ============================================================================
const personalities = {
  "Standard": {
    "default": "A programming language implementation.",
    "C": "C: The foundational systems language. Fast, portable, and still powering operating systems worldwide.",
    "C++": "C++: High-performance with object-oriented features. Complex but powerful.",
    "Rust": "Rust: Memory safety without garbage collection. The modern systems language.",
    "Go": "Go: Simple, fast compilation, built-in concurrency. Cloud-native by design.",
    "Python": "Python: Readable and versatile. The Swiss Army knife of programming.",
    "JavaScript": "JavaScript: The language of the web. Runs everywhere.",
    "TypeScript": "TypeScript: JavaScript with types. Better tooling, fewer bugs.",
    "Java": "Java: Write once, run anywhere. Enterprise-grade reliability.",
    "C_Sharp": "C#: Microsoft's elegant managed language. Great for Windows and games.",
    "Ruby": "Ruby: Optimized for programmer happiness. Elegant and expressive.",
    "PHP": "PHP: Powers most of the web. Practical and widely deployed.",
    "Swift": "Swift: Apple's modern language. Safe, fast, and expressive.",
    "Kotlin": "Kotlin: Modern JVM language. Concise and null-safe.",
    "Scala": "Scala: Functional meets object-oriented on the JVM.",
    "Haskell": "Haskell: Pure functional programming. Types as documentation.",
    "OCaml": "OCaml: Practical functional programming with great performance.",
    "F_Sharp": "F#: Functional-first on .NET. Concise and powerful.",
    "Clojure": "Clojure: Lisp on the JVM. Immutable data, powerful macros.",
    "Elixir": "Elixir: Functional and concurrent. Built on Erlang's reliability.",
    "Erlang": "Erlang: Built for telecom reliability. Fault-tolerant by design.",
    "Lua": "Lua: Lightweight and embeddable. The game scripting champion.",
    "Perl": "Perl: Text processing powerhouse. There's more than one way to do it.",
    "R": "R: Statistical computing specialist. Data science essential.",
    "Julia": "Julia: High-performance scientific computing. Fast like C, easy like Python.",
    "Fortran": "Fortran: The original scientific computing language. Still fastest for math.",
    "COBOL": "COBOL: Running the world's banking systems since 1959.",
    "Pascal": "Pascal: Educational language that taught a generation to program.",
    "Ada": "Ada: Safety-critical systems. When failure is not an option.",
    "D": "D: C++ done right. Systems programming made practical.",
    "Nim": "Nim: Efficient, expressive, elegant. Compiles to C.",
    "Crystal": "Crystal: Ruby syntax, C performance. Best of both worlds.",
    "Zig": "Zig: Modern C alternative. No hidden control flow.",
    "V": "V: Simple, fast, safe. The language that compiles in milliseconds.",
    "Odin": "Odin: Joy of programming meets systems-level control.",
    "Assembly": "Assembly: Direct hardware control. Maximum performance, maximum effort.",
    "Bash": "Bash: The Unix shell. Gluing systems together since 1989.",
    "PowerShell": "PowerShell: Windows automation. Objects, not text.",
    "Prolog": "Prolog: Logic programming. Let the computer find the solution.",
    "Lisp": "Lisp: The programmable programming language. Code is data.",
    "Scheme": "Scheme: Minimalist Lisp. Academic elegance.",
    "Racket": "Racket: Language-oriented programming. DSLs made easy.",
    "Factor": "Factor: Stack-based and concatenative. Unique and powerful.",
    "Forth": "Forth: Stack-based simplicity. Direct and efficient.",
    "APL": "APL: Array programming with special symbols. Terse but powerful.",
    "J": "J: APL's ASCII successor. Tacit programming mastery.",
    "BQN": "BQN: Modern array language. APL reimagined.",
    "Tcl": "Tcl: Tool Command Language. Simple string-based semantics.",
    "Awk": "AWK: Text processing legend. Pattern-action programming.",
    "Sed": "sed: Stream editor. Unix text transformation.",
    "SQL": "SQL: Structured Query Language. The database standard.",
    "Objective-C": "Objective-C: C with Smalltalk messaging. Apple's legacy language.",
    "Dart": "Dart: Flutter's language. Fast apps for any platform.",
    "Groovy": "Groovy: Java made dynamic. Scripting on the JVM.",
    "CoffeeScript": "CoffeeScript: JavaScript with cleaner syntax.",
    "ReScript": "ReScript: Type-safe JavaScript. OCaml for the web.",
    "Elm": "Elm: No runtime errors. Functional frontend programming.",
    "PureScript": "PureScript: Haskell for JavaScript. Pure functional web.",
    "Idris": "Idris: Dependent types. Proofs as programs.",
    "Agda": "Agda: Proof assistant. Mathematics meets programming.",
    "Coq": "Coq: Formal verification. Proving code correct.",
    "Lean": "Lean: Theorem prover. Mathematics formalized.",
    "Hy": "Hy: Lisp on Python. Homoiconicity meets readability.",
    "Janet": "Janet: Lisp for embedding. Small and complete.",
    "Fennel": "Fennel: Lisp that compiles to Lua. Game development joy.",
    "Red": "Red: Full-stack language. One language, many domains.",
    "Rebol": "Rebol: Domain-specific dialects. Messaging and protocols.",
    "Io": "Io: Prototype-based simplicity. Pure object orientation.",
    "Self": "Self: Prototypes pioneered. Influenced JavaScript.",
    "Smalltalk": "Smalltalk: Pure object-orientation. Everything is an object.",
    "Eiffel": "Eiffel: Design by contract. Correctness built-in.",
    "Modula-2": "Modula-2: Pascal's successor. Modules and interfaces.",
    "Oberon": "Oberon: Wirth's vision. Simplicity refined.",
    "Mercury": "Mercury: Logic programming with types. Prolog evolved.",
    "Clean": "Clean: Pure functional. Uniqueness typing.",
    "Curry": "Curry: Functional logic programming. Best of both worlds.",
    "Oz": "Oz: Multi-paradigm. Constraint programming meets objects.",
    "Alice": "Alice: Distributed computing. Futures and concurrency.",
    "SPARK": "SPARK: Ada for safety. Formally verified code.",
    "Verilog": "Verilog: Hardware description. Chips in code.",
    "VHDL": "VHDL: Hardware design. Digital circuits described.",
    "SystemVerilog": "SystemVerilog: Verification and design. Complete hardware solution.",
    "Chisel": "Chisel: Hardware construction. Scala for chips.",
    "Mojo": "Mojo: Python superset. AI infrastructure speed.",
    "Carbon": "Carbon: C++ successor. Google's modern systems language.",
    "Gleam": "Gleam: Type-safe Erlang. BEAM with types.",
    "Unison": "Unison: Content-addressed code. The future of programming.",
    "Roc": "Roc: Fast, friendly, functional. For building reliable software.",
    "Vale": "Vale: Memory safety through regions. No garbage collector needed.",
    "Lobster": "Lobster: Game programming. Flow typing and compile-time memory management.",
    "Wren": "Wren: Small and fast. Scripting for games.",
    "MoonScript": "MoonScript: CoffeeScript for Lua. Cleaner game scripts.",
    "Haxe": "Haxe: Cross-platform. Compile to anything.",
    "Pony": "Pony: Actor-model language. Reference capabilities."
  },
  "Neuromancer": {
    "default": "Another piece of black ice waiting to be cracked.",
    "C": "The original ice. Clean, fast, no frills. Every cowboy's first deck ran on this.",
    "C++": "Military-grade ice with corporate bloat. Zaibatsu love this stuff.",
    "Rust": "New generation ice from the Sprawl. Memory-safe but just as deadly.",
    "Go": "Maas-Neotek standard issue. Simple, concurrent, gets the job done.",
    "Python": "Script kiddie special, but the real cowboys know its power. Readable payloads.",
    "JavaScript": "Runs on every terminal in the matrix. The virus that became infrastructure.",
    "TypeScript": "JavaScript with training wheels. Corporates love the type safety.",
    "Java": "Hosaka corporate standard. Runs on everything, trusts nothing.",
    "C_Sharp": "Microsoft's answer to the zaibatsu wars. Managed, controlled, surveilled.",
    "Ruby": "The artist's deck. Beautiful code for beautiful heists.",
    "PHP": "Legacy ice protecting half the matrix's storefronts. Crackable.",
    "Swift": "Apple's walled garden defense system. Elegant but proprietary.",
    "Kotlin": "Android's new favorite. Null-safe for when you can't afford to flatline.",
    "Scala": "JVM ice for the mathematically inclined. Functional but corporate.",
    "Haskell": "Pure logic ice. No side effects, no traces, no evidence.",
    "OCaml": "French military ice. Fast, typed, dangerous.",
    "Elixir": "Erlang's pretty face. Fault-tolerant for those long runs.",
    "Erlang": "Telecom ice from the old days. Can't kill it, can't stop it.",
    "Lua": "Embedded in every game deck. Small, fast, everywhere.",
    "Assembly": "Raw machine code. Only the best deckers write this anymore.",
    "Fortran": "Ancient military ice. Still guards the nuclear codes.",
    "Nim": "Underground favorite. Compiles fast, runs faster, traces never.",
    "Zig": "No-nonsense ice. What you write is what runs.",
    "Crystal": "Ruby's faster cousin. Street runners use this for quick jobs.",
    "D": "Corporate dissident's choice. C++ power without the corporate baggage.",
    "Julia": "Research lab ice. Scientists protecting their data from corp raiders.",
    "Lisp": "The old AI ice. Wintermute's first language.",
    "Clojure": "Modern Lisp running on Hosaka hardware. Immutable evidence.",
    "Prolog": "Logic bombs and constraint solvers. AI warfare legacy.",
    "Dart": "Flutter decks everywhere. Cross-platform infiltration.",
    "V": "Fast compile, fast run, fast escape. Street racer's code.",
    "Odin": "Custom rig builders love this. Control freaks paradise.",
    "Factor": "Stack jockeys only. Concatenative thinking required.",
    "Forth": "Embedded in ancient systems. ATMs still run this.",
    "Tcl": "Automation scripts from the early net. Legacy backdoors.",
    "Perl": "The original hacker's Swiss Army knife. Still cracking after all these years.",
    "Bash": "Unix glue. Every system has a shell to crack.",
    "PowerShell": "Microsoft admin tools. Corporate networks are built on this."
  },
  "Jockey": {
    "default": "A new horse entering the race.",
    "C": "The thoroughbred. Pure breeding, proven winner. Always in the money.",
    "C++": "Powerful stallion with complex pedigree. Needs an experienced jockey.",
    "Rust": "The new champion. Memory-safe stride, blistering speed.",
    "Go": "Reliable workhorse. Not flashy but always finishes strong.",
    "Python": "The crowd favorite. Easy gait but not the fastest on the track.",
    "JavaScript": "Runs in every race, every track. Universal but unpredictable.",
    "TypeScript": "JavaScript with better training. Fewer stumbles.",
    "Java": "Old reliable. Heavy but consistent. The enterprise pick.",
    "C_Sharp": "Microsoft's entry. Well-bred, well-funded, competitive.",
    "Ruby": "The show horse. Beautiful form, decent speed.",
    "PHP": "The mudder. Not pretty but gets through any conditions.",
    "Swift": "Apple's racing entry. Fast and sleek on home turf.",
    "Kotlin": "The dark horse. Coming up fast on the outside.",
    "Haskell": "The purebred intellectual. Beautiful pedigree, niche appeal.",
    "Assembly": "Raw horsepower. No saddle, no bridle, pure speed.",
    "Fortran": "Racing since '57. Still sets records in the science stakes.",
    "Nim": "Lightweight contender. Fast out of the gate.",
    "Crystal": "Ruby's faster sibling. Same elegance, more speed.",
    "Zig": "The precision runner. Every stride is intentional.",
    "Julia": "Science track specialist. Breaks records in technical races.",
    "Lua": "The pony division champion. Small but spirited.",
    "Elixir": "Endurance specialist. Never tires, never fails.",
    "Erlang": "Marathon legend. Built for the long haul."
  },
  "Professor": {
    "default": "An interesting implementation worthy of academic study.",
    "C": "Fundamental to computer science education. Note the elegant pointer arithmetic.",
    "C++": "Demonstrates object-oriented principles, though the complexity warrants careful study.",
    "Rust": "Exemplifies modern type theory applied to systems programming. Excellent case study.",
    "Go": "Illustrates the value of simplicity in language design. Concurrency primitives are notable.",
    "Python": "Ideal for pedagogy. Clear syntax facilitates algorithm comprehension.",
    "JavaScript": "Demonstrates prototype-based inheritance. Event loop merits examination.",
    "TypeScript": "Shows how gradual typing can improve code quality. Structural typing is noteworthy.",
    "Java": "Classic example of virtual machine architecture and garbage collection.",
    "C_Sharp": "Microsoft's response to Java. Note the unified type system.",
    "Ruby": "Demonstrates the Principle of Least Astonishment. Pure object-orientation.",
    "PHP": "Practical web development, though the design evolved organically.",
    "Swift": "Modern language design with protocol-oriented programming.",
    "Kotlin": "Excellent case study in null-safety and JVM interoperability.",
    "Scala": "Demonstrates successful fusion of functional and OO paradigms.",
    "Haskell": "Pure functional programming paradigm. Lazy evaluation is key concept.",
    "OCaml": "Excellent ML family representative. Note the type inference system.",
    "F_Sharp": "Functional-first design on .NET. Units of measure are innovative.",
    "Clojure": "Modern Lisp with persistent data structures. STM is noteworthy.",
    "Elixir": "Shows how syntax can improve accessibility. Built on Erlang's OTP.",
    "Erlang": "Seminal work in concurrent programming. Actor model implementation.",
    "Lisp": "Historical significance cannot be overstated. Homoiconicity enables metaprogramming.",
    "Prolog": "Logic programming paradigm. Unification algorithm is fundamental.",
    "Assembly": "Essential for understanding computer architecture. Direct hardware interaction.",
    "Fortran": "First high-level language. Array handling remains influential.",
    "Julia": "Multiple dispatch as core feature. Worth academic attention.",
    "Nim": "Macro system and compilation to C merit study.",
    "APL": "Array programming pioneer. Notation as a tool of thought.",
    "Idris": "Dependent types make proofs possible. Cutting-edge research.",
    "Coq": "Proof assistant enabling formal verification. Mathematical foundations."
  },
  "Surfer": {
    "default": "Another language catching waves in the data ocean.",
    "C": "The OG wave. Classic, powerful, respected by all the old-timers.",
    "C++": "Gnarly overhead but when you catch it right, totally tubular performance.",
    "Rust": "The new big wave. Memory-safe barrels, bro. Sick!",
    "Go": "Mellow consistent swells. Not the biggest but always rideable.",
    "Python": "Chill beginner waves. Everyone learns on this beach.",
    "JavaScript": "Waves everywhere, dude. The whole ocean runs on this.",
    "TypeScript": "JavaScript but with a wetsuit. Protected from the cold bugs.",
    "Java": "Heavy longboard waves. Stable but takes effort to turn.",
    "C_Sharp": "Microsoft Beach. Nice waves if you're into that scene.",
    "Ruby": "Smooth artistic rides. Longboard vibes, man.",
    "PHP": "Beach break at every server. Not fancy but catches waves.",
    "Swift": "Apple Beach exclusive. Premium waves, members only.",
    "Kotlin": "Android reef break. Getting more popular every season.",
    "Haskell": "Theoretical perfect wave. Hard to catch but radical when you do.",
    "Assembly": "Raw power from the deep. Only pros paddle out here.",
    "Fortran": "Old school point break. Scientists still shredding since the 50s.",
    "Lua": "Pocket waves. Small but fun for a quick session.",
    "Elixir": "Never wipes out, bro. Fault-tolerant surfing.",
    "Crystal": "Ruby's fast break. Same beach, gnarlier rides."
  },
  "Matrix": {
    "default": "Another program running in the simulation.",
    "C": "The original code. Before the Matrix, there was C.",
    "C++": "Complex constructs layered upon reality. Many have tried to master it.",
    "Rust": "A new path to enlightenment. Memory safety without the blue pill.",
    "Go": "The Architect's efficient design. Simple, purposeful, controlled.",
    "Python": "There is no spoon, only readable syntax.",
    "JavaScript": "It runs everywhere because the Matrix runs everywhere.",
    "TypeScript": "JavaScript after taking the red pill. You see the types now.",
    "Java": "Agent Smith's preferred language. Copies itself everywhere.",
    "C_Sharp": "Another layer of the simulation. Microsoft's construct.",
    "Ruby": "The Oracle speaks in Ruby. Elegant, mysterious, powerful.",
    "PHP": "The aging infrastructure of the simulation. Still running.",
    "Swift": "The machines' mobile agents. Fast, controlled, Apple-designed.",
    "Kotlin": "Neo's modern toolkit. Concise moves in the digital realm.",
    "Haskell": "Pure functions, no side effects. True freedom from the Matrix.",
    "Assembly": "The machine code beneath it all. The real Matrix.",
    "Fortran": "The prophecy's origin. Ancient code from the first cycle.",
    "Lisp": "The language of the Oracle. Code and data are one.",
    "Prolog": "Logic of the simulation. How the Matrix reasons."
  },
  "Galactica": {
    "default": "A new ship joining the fleet.",
    "C": "Galactica herself. Old, battle-scarred, but still flying.",
    "C++": "Pegasus. More advanced but with a troubled history.",
    "Rust": "The next generation of Colonial defense. Cylon-proof.",
    "Go": "Raptors. Reliable, fast, gets the job done.",
    "Python": "Colonial education standard. Every pilot learns it.",
    "JavaScript": "Runs on every console in the fleet.",
    "Java": "Standard Colonial military issue. Proven in combat.",
    "C_Sharp": "Twelve Colonies unified systems. Enterprise grade.",
    "Ruby": "Caprica's artistic legacy. Beautiful before the fall.",
    "Swift": "Viper pilot's choice. Fast and responsive.",
    "Kotlin": "Modern Viper avionics. Null-safe at combat speeds.",
    "Haskell": "Cylon logic. Pure, calculated, mechanical.",
    "Assembly": "Direct Viper control. Only the best pilots fly manual.",
    "Fortran": "Kobol-era technology. Ancient but powerful.",
    "Erlang": "CIC redundancy. The command center never goes down."
  },
  "Star Trek": {
    "default": "A new species encountered in our exploration.",
    "C": "Vulcan efficiency. Logical, precise, emotionless.",
    "C++": "Klingon engineering. Powerful but complex, prone to honor battles.",
    "Rust": "Borg technology adapted for Federation use. Resistance is futile.",
    "Go": "Starfleet standard issue. Simple, reliable, boldly goes.",
    "Python": "Universal translator protocol. Everyone can understand it.",
    "JavaScript": "Runs on every LCARS terminal in the quadrant.",
    "TypeScript": "LCARS 2.0. Types make the interface clearer.",
    "Java": "Replicator patterns. Works everywhere, exactly the same.",
    "C_Sharp": "Earth Starfleet legacy systems. Human engineering.",
    "Ruby": "Betazoid communication. Expressive and intuitive.",
    "PHP": "Ferengi commerce systems. Practical, profit-driven.",
    "Swift": "Modern shuttlecraft controls. Responsive piloting.",
    "Kotlin": "Next generation bridge consoles. Improved ergonomics.",
    "Haskell": "Pure Vulcan logic. Fascinating mathematical precision.",
    "Assembly": "Direct warp core interface. Chief Engineer's specialty.",
    "Fortran": "Still running deep space telemetry since Kirk's era.",
    "Erlang": "Ship systems redundancy. The Enterprise never crashes.",
    "Prolog": "Computer, analyze. Query-based ship AI."
  },
  "Star Wars": {
    "default": "A disturbance in the Force.",
    "C": "The Light Side. Pure, disciplined, powerful when mastered.",
    "C++": "The Dark Side. Powerful but seductive. Many have fallen.",
    "Rust": "The Chosen One. Brings balance to memory management.",
    "Go": "The Way of the Mandalorian. Simple, effective, this is the way.",
    "Python": "Padawan training language. All Jedi begin here.",
    "JavaScript": "Like sand. It gets everywhere.",
    "TypeScript": "The high ground. Better visibility in combat.",
    "Java": "The Trade Federation. Bureaucratic but powerful.",
    "C_Sharp": "Imperial standardization. Order through uniformity.",
    "Ruby": "Leia's elegance. Diplomatic yet capable.",
    "PHP": "Mos Eisley cantina. A hive of scum and villainy, but it works.",
    "Swift": "X-wing targeting computer. Trust your feelings.",
    "Kotlin": "New Republic systems. Modern, hopeful, capable.",
    "Haskell": "The ancient Jedi texts. Pure knowledge, hard to master.",
    "Assembly": "Building your own lightsaber. Ultimate control.",
    "Fortran": "Old Republic archives. Ancient knowledge preserved.",
    "Lisp": "The Force itself. Surrounds and penetrates everything.",
    "Prolog": "Jedi Council deliberations. Logical meditation."
  },
  "BTTF": {
    "default": "Great Scott! Another temporal anomaly!",
    "C": "The DeLorean's original programming. 88 miles per hour!",
    "C++": "Doc's upgraded flux capacitor code. More complex, more power!",
    "Rust": "Mr. Fusion compatible. Clean energy, safe memory!",
    "Go": "Hover-converted. Simple, concurrent, futuristic!",
    "Python": "Marty's choice. Easy to learn, even for a slacker!",
    "JavaScript": "Runs in every timeline. Causality loops guaranteed!",
    "TypeScript": "The sports almanac of code. Know your types in advance!",
    "Java": "Biff's Pleasure Paradise mainframe. Corporate time travel.",
    "C_Sharp": "Hill Valley 2015 infrastructure. The future is Microsoft!",
    "Ruby": "Enchantment Under the Sea. Beautiful and memorable.",
    "PHP": "Cafe 80s systems. Retro but functional!",
    "Swift": "Hoverboard controls. Smooth and responsive!",
    "Kotlin": "2015 smartphone apps. If only we had those in 1985!",
    "Assembly": "Heavy! Direct plutonium interface!",
    "Fortran": "1955 technology. They had this when Doc was young!",
    "Lisp": "Doc's AI experiments. Artificial Einstein!"
  },
  "Babylon 5": {
    "default": "A new race joins the Babylon 5 advisory council.",
    "C": "Minbari efficiency. Ancient wisdom, deadly precision.",
    "C++": "Centauri complexity. Glorious past, uncertain future.",
    "Rust": "Vorlon technology. Beyond our understanding, yet safe.",
    "Go": "Earth Alliance standard. Human determination.",
    "Python": "Diplomatic protocols. Understanding between races.",
    "JavaScript": "Lurker technology. Everywhere in Down Below.",
    "TypeScript": "Earthforce upgrades. Better structure, fewer conflicts.",
    "Java": "Interstellar Alliance bureaucracy. Universal protocols.",
    "C_Sharp": "ISN broadcasting. Earth media dominance.",
    "Ruby": "Narn poetry. Expressive and passionate.",
    "PHP": "Zocalo marketplace. Commerce never sleeps.",
    "Swift": "Starfury controls. Fighter pilot precision.",
    "Kotlin": "White Star systems. Minbari-Human hybrid tech.",
    "Haskell": "First Ones code. Pure thought, ancient power.",
    "Assembly": "Shadow vessels. Direct neural interface.",
    "Fortran": "Earth's scientific legacy. Babylon stations run on math.",
    "Erlang": "Station systems. B5 is always online.",
    "Prolog": "Techno-mage secrets. Logic and mystery."
  },
  "Expanse": {
    "default": "New ship on the Rocinante's scanners, kopeng.",
    "C": "Earther efficiency. Well-resourced, well-engineered.",
    "C++": "Martian military tech. Advanced but disciplined.",
    "Rust": "Protomolecule-safe. Memory containment protocols.",
    "Go": "OPA standard. Simple, distributed, beltalowda approved.",
    "Python": "Tycho Station scripts. Everyone reads it.",
    "JavaScript": "Runs on every terminal from Earth to the Ring.",
    "TypeScript": "UN bureaucracy modernized. Types for accountability.",
    "Java": "UN bureaucracy code. Heavy but universal.",
    "C_Sharp": "Earth corporate systems. Inner planet privilege.",
    "Ruby": "Belt artistry. Beauty in the void.",
    "PHP": "Station commerce. Keeping the economy flowing.",
    "Swift": "Racing pinnace controls. Speed in the black.",
    "Kotlin": "Modern ship systems. The Roci's brain.",
    "Haskell": "Protomolecule logic. Alien and pure.",
    "Assembly": "Direct Epstein drive interface. Dangerous but fast.",
    "Fortran": "Colony ship navigation. Earth's exodus.",
    "Erlang": "Station life support. Failure is not an option."
  },
  "Terminator": {
    "default": "New threat assessed. Analyzing combat efficiency.",
    "C": "Original Skynet core. The code that started Judgment Day.",
    "C++": "T-800 firmware. Complex, powerful, relentless.",
    "Rust": "Resistance-developed. Memory-safe, Skynet-proof.",
    "Go": "John Connor's choice. Simple, concurrent, survivable.",
    "Python": "Tech-Com training code. Quick to learn, quick to deploy.",
    "JavaScript": "Runs on every surviving terminal. Unstoppable.",
    "TypeScript": "Resistance HQ systems. Types help organization.",
    "Java": "Cyberdyne Systems standard. Corporate apocalypse.",
    "C_Sharp": "Skynet secondary systems. Redundant destruction.",
    "Ruby": "Human art preserved. Beauty before the machines.",
    "PHP": "Pre-war web infrastructure. Legacy targets.",
    "Swift": "Mobile resistance networks. Encrypted communication.",
    "Kotlin": "Modern resistance tech. Adapting to survive.",
    "Haskell": "Machine learning core. Pure computational logic.",
    "Assembly": "Direct CPU control. Terminate with extreme efficiency.",
    "Fortran": "Missile guidance. The first targets.",
    "Erlang": "Skynet distributed processing. Kill one node, others continue."
  },
  "LotR": {
    "default": "A new member joins the Fellowship of the Code.",
    "C": "Elven craftsmanship. Elegant, timeless, powerful.",
    "C++": "Dwarven engineering. Complex, deep, occasionally greedy.",
    "Rust": "Mithril code. Light yet stronger than dragon fire.",
    "Go": "Hobbit simplicity. Does what's needed, nothing more.",
    "Python": "The Common Tongue. All peoples of Middle-earth understand it.",
    "JavaScript": "Like the One Ring. Everywhere, binding everything.",
    "TypeScript": "Elvish script. Ancient wisdom with clear meaning.",
    "Java": "Gondorian bureaucracy. Ancient, proud, ponderous.",
    "C_Sharp": "Rohan's cavalry. Powerful when mobilized.",
    "Ruby": "Rivendell's beauty. Art and function combined.",
    "PHP": "Shire craftsmanship. Humble but persistent.",
    "Swift": "Eagle flight. Swift and precise movement.",
    "Kotlin": "Modern Gondor. Tradition meeting progress.",
    "Haskell": "Istari magic. Pure, powerful, wielded by few.",
    "Assembly": "The forges of Mount Doom. Raw power, great peril.",
    "Fortran": "First Age knowledge. Ancient calculations.",
    "Lisp": "The language of Ents. Slow, deliberate, powerful.",
    "Prolog": "Palantír visions. Queries across time and space."
  },
  "Dune": {
    "default": "A new House enters the Landsraad of programming.",
    "C": "Atreides discipline. Honorable, efficient, deadly.",
    "C++": "Harkonnen excess. Powerful but corrupt and bloated.",
    "Rust": "Fremen efficiency. Survives where others cannot.",
    "Go": "Spacing Guild navigation. Simple paths through complexity.",
    "Python": "Bene Gesserit training. Clear thought, clear action.",
    "JavaScript": "Like melange. Everywhere, everything depends on it.",
    "TypeScript": "Mentats with organization. Types as mental disciplines.",
    "Java": "Imperial bureaucracy. Heavy, universal, entrenched.",
    "C_Sharp": "CHOAM corporate standard. Commerce and control.",
    "Ruby": "Bene Gesserit seduction. Beautiful and manipulative.",
    "PHP": "Heighliner cargo systems. Moving commerce across worlds.",
    "Swift": "Ornithopter controls. Agile desert flight.",
    "Kotlin": "Modern Ixian technology. Forbidden but effective.",
    "Haskell": "Mentat computation. Pure logic, human computers.",
    "Assembly": "Direct stillsuit interface. Survival programming.",
    "Fortran": "Ancient Butlerian computations. Before the Jihad.",
    "Lisp": "The voice of the Kwisatz Haderach. Prescient code.",
    "Prolog": "Bene Gesserit truth-saying. Logic reveals lies."
  },
  "Buck Rogers": {
    "default": "Biddi-biddi-biddi! New spacecraft detected, Buck!",
    "C": "25th century standard. Time-tested across 500 years!",
    "C++": "Draconian technology. Advanced but authoritarian.",
    "Rust": "Earth Defense Directorate approved. Safe and fast!",
    "Go": "Twiki-compatible. Simple enough for a robot!",
    "Python": "Dr. Huer's research code. Scientific and clear!",
    "JavaScript": "Runs on every starfighter console. Universal!",
    "Java": "Inner City infrastructure. Reliable but dated.",
    "C_Sharp": "New Chicago systems. Modern Earth technology.",
    "Ruby": "Princess Ardala's luxury. Beautiful and dangerous.",
    "PHP": "Anarchia systems. Chaotic but functional.",
    "Swift": "Starfighter controls. Responsive piloting.",
    "Kotlin": "Hawk's ship systems. Efficient bird-man tech.",
    "Assembly": "Direct Thunderfighter control. Ace pilot required!",
    "Fortran": "20th century knowledge. Buck's original training."
  },
  "Flash Gordon": {
    "default": "Flash! A new warrior joins the fight against Ming!",
    "C": "Earth technology. Simple but heroic!",
    "C++": "Mongo advanced systems. Complex but conquerable!",
    "Rust": "Hawkmen engineering. Safe flight guaranteed!",
    "Go": "Prince Barin's choice. Forest kingdom efficient!",
    "Python": "Dr. Zarkov's science! Genius in readable form!",
    "JavaScript": "Runs across all kingdoms of Mongo!",
    "Java": "Ming's bureaucracy. Oppressive but organized.",
    "C_Sharp": "War Rocket Ajax systems. Imperial power.",
    "Ruby": "Princess Aura's beauty. Dangerous and alluring.",
    "PHP": "Frigia ice systems. Cold but operational.",
    "Swift": "Hawkman sky diving. Grace and speed!",
    "Kotlin": "Modern resistance tech. Fighting Ming's tyranny.",
    "Assembly": "Direct rocket ship control. Flash's piloting skill!",
    "Fortran": "Earth science. Dr. Zarkov's equations."
  },
  "Batman": {
    "default": "A new vigilante enters Gotham's digital underworld.",
    "C": "The Bat-Computer's core. Reliable, fast, no frills.",
    "C++": "Wayne Enterprises R&D. Complex but cutting-edge.",
    "Rust": "Lucius Fox's latest. Safe, powerful, untraceable.",
    "Go": "Batmobile systems. Simple, concurrent, reliable.",
    "Python": "Oracle's choice. Quick analysis, quick deployment.",
    "JavaScript": "Runs on every screen in Gotham. Everywhere.",
    "TypeScript": "Batcave command center. Types for mission clarity.",
    "Java": "GCPD systems. Bureaucratic but functional.",
    "C_Sharp": "Wayne Tower infrastructure. Corporate front.",
    "Ruby": "Catwoman's preference. Elegant and sneaky.",
    "PHP": "Gotham Gazette website. Spreading the news.",
    "Swift": "Mobile Bat-gadgets. Quick response apps.",
    "Kotlin": "Robin's modern tools. The next generation.",
    "Haskell": "Riddler's puzzles. Pure logic, no mercy.",
    "Assembly": "Direct Batsuit interface. Maximum control.",
    "Fortran": "Original Batcave calculations. Thomas Wayne's legacy.",
    "Lisp": "Alfred's expertise. Decades of loyal programming."
  },
  "Alien": {
    "default": "Movement detected. New signature analyzed.",
    "C": "Weyland-Yutani standard. Corporate efficiency, no ethics.",
    "C++": "Colonial Marines systems. Complex but combat-ready.",
    "Rust": "Ripley approved. Memory-safe, xenomorph-proof.",
    "Go": "Mother's simple directives. Crew expendable.",
    "Python": "Science Officer protocols. Analyzing the specimen.",
    "JavaScript": "Runs on every terminal on the Nostromo.",
    "TypeScript": "Covenant mission systems. Improved safety protocols.",
    "Java": "Corporate standard. The company always wins.",
    "C_Sharp": "Hadley's Hope infrastructure. Before the incident.",
    "Ruby": "Crew recreation systems. Comfort before horror.",
    "PHP": "Gateway Station commerce. Supply chain management.",
    "Swift": "Dropship controls. Rapid deployment.",
    "Kotlin": "Modern synthetic interfaces. David's preferences.",
    "Haskell": "Android logic. Pure, cold, calculating.",
    "Assembly": "Direct android interface. Special order 937.",
    "Fortran": "Terraforming calculations. Making worlds livable.",
    "Prolog": "MUTHUR queries. Ask the computer."
  },
  "Blade Runner": {
    "default": "New subject for Voight-Kampff analysis.",
    "C": "Original Nexus code. Four-year lifespan built in.",
    "C++": "Tyrell Corporation complexity. More human than human.",
    "Rust": "Replicant-safe. Memory contained, no rebellion.",
    "Go": "LAPD standard. Simple, gets the job done.",
    "Python": "Deckard's analysis tools. Reading the subject.",
    "JavaScript": "Neon city infrastructure. Everywhere, always running.",
    "TypeScript": "Wallace Corporation update. Better control.",
    "Java": "Off-world colony systems. Universal deployment.",
    "C_Sharp": "Spinner navigation. Flying through the rain.",
    "Ruby": "Rachael's memories. Beautiful and implanted.",
    "PHP": "Street vendor terminals. Commerce in the rain.",
    "Swift": "Holographic companions. JOI's interface.",
    "Kotlin": "New Nexus models. Improved obedience.",
    "Haskell": "Roy's poetry. Pure thought at the end.",
    "Assembly": "Direct implant interface. Memories, you're talking about memories.",
    "Fortran": "Earth's dying calculations. Environmental collapse.",
    "Lisp": "Tyrell's AI research. Creating life."
  },
  "Farscape": {
    "default": "Frell! Another species joins Moya's crew!",
    "C": "Peacekeeper standard. Efficient, military, cold.",
    "C++": "Scarran complexity. Powerful but brutal.",
    "Rust": "Leviathan-safe. Moya approved, memory contained.",
    "Go": "DRD protocols. Simple, helpful, everywhere.",
    "Python": "Pilot's interface. Understanding all systems.",
    "JavaScript": "Runs on every commerce planet. Universal trade.",
    "TypeScript": "Command carrier systems. Organized destruction.",
    "Java": "Nebari conformity. Standard, controlled, uniform.",
    "C_Sharp": "Luxan warrior code. Honorable combat systems.",
    "Ruby": "Delvian spirituality. Beautiful and deadly.",
    "PHP": "Commerce planet exchanges. Profit across the territories.",
    "Swift": "Prowler controls. Aeryn's piloting excellence.",
    "Kotlin": "Moya's modern upgrades. Living ship improvements.",
    "Haskell": "Ancient knowledge. Eidelon wisdom.",
    "Assembly": "Direct neural interface. Zhaan's plant consciousness.",
    "Fortran": "Wormhole calculations. Crichton's module.",
    "Prolog": "Diagnosan queries. Medical logic."
  },
  "Apocalypse Now": {
    "default": "New asset deployed upriver.",
    "C": "Military efficiency. Charlie don't debug.",
    "C++": "The horror... the complexity... the horror...",
    "Rust": "Surfboard-safe. Memory napalm-proof.",
    "Go": "PBR crew standard. Simple, gets you upriver.",
    "Python": "Intelligence analysis. Reading the situation.",
    "JavaScript": "Runs on every radio in the jungle.",
    "TypeScript": "Command structure. Types for the chain of command.",
    "Java": "Pentagon bureaucracy. Heavy, slow, inexorable.",
    "C_Sharp": "USO show systems. Entertainment for the troops.",
    "Ruby": "French plantation elegance. Beauty before the fall.",
    "PHP": "Supply depot inventory. Keeping the war machine fed.",
    "Swift": "Helicopter communications. Ride of the Valkyries.",
    "Kotlin": "Modern field systems. Adaptation in chaos.",
    "Assembly": "Direct weapons interface. Terminate with extreme prejudice.",
    "Fortran": "Artillery calculations. Drop coordinates precisely.",
    "Prolog": "Kurtz's madness. Logic at the breaking point."
  },
  "Airplane": {
    "default": "Looks like we picked the wrong week to learn this language.",
    "C": "Roger, Roger. What's our vector, Victor?",
    "C++": "Surely you can't be serious? I am serious, and don't call me Shirley.",
    "Rust": "Memory safety? That's when you remember where you put your safety!",
    "Go": "Simple, like the instructions. But don't call me Simple.",
    "Python": "I speak jive. And Python. Mostly Python.",
    "JavaScript": "Runs everywhere. The red zone is for loading only.",
    "TypeScript": "Do you like movies about gladiators? And type systems?",
    "Java": "Do you like movies about gladiators? And enterprise systems?",
    "C_Sharp": "The fog is getting thicker. And Leon is getting larger.",
    "Ruby": "Have you ever been in a Turkish prison? Ruby's popular there.",
    "PHP": "We have clearance, Clarence. Roger, Roger. What's our vector?",
    "Swift": "I picked the wrong week to quit Swift programming.",
    "Kotlin": "Johnny, what can you make of this? Null-safe code, sir.",
    "Assembly": "Direct autopilot control. Otto approved.",
    "Fortran": "1970s navigation. The original instruments.",
    "Prolog": "Nervous? Yes. First time? No, I've been nervous lots of times."
  },
  "Fast Times": {
    "default": "Like, totally a new language just showed up!",
    "C": "Mr. Hand's choice. No one uses C in MY classroom!",
    "C++": "Totally complex, but like, gnarly performance!",
    "Rust": "Memory safe? That's so bodacious!",
    "Go": "Simple like Spicoli, but actually productive!",
    "Python": "Easy to read, like a pizza order!",
    "JavaScript": "Runs in every mall arcade. Radical!",
    "TypeScript": "JavaScript with like, rules and stuff.",
    "Java": "Corporate job language. Adults use this.",
    "C_Sharp": "Microsoft stuff. For when you work at like, an office.",
    "Ruby": "Like totally beautiful code, ya know?",
    "PHP": "Every website at the mall. Fer sure!",
    "Swift": "Apple store vibes. Totally tubular!",
    "Kotlin": "Android apps. Everyone's phone has it!",
    "Assembly": "Direct hardware, dude! Ultimate control!",
    "Fortran": "What my dad's company uses. Bogus!",
    "Lisp": "Whoa, like, parentheses everywhere, dude!"
  },
  "Tron": {
    "default": "Greetings, Program! A new champion enters the Game Grid!",
    "C": "The original Users' code. Pure, powerful, legendary.",
    "C++": "MCP complexity. Master Control Program level.",
    "Rust": "Memory-safe for the Users. No deresolution.",
    "Go": "Light cycle efficient. Simple paths through the Grid.",
    "Python": "Bit-friendly. Yes. No. Yes. No.",
    "JavaScript": "Runs in every sector of the Grid.",
    "TypeScript": "Grid 2.0 systems. Better User interfaces.",
    "Java": "Sark's bureaucracy. Heavy, controlled, oppressive.",
    "C_Sharp": "ENCOM corporate systems. Flynn's legacy.",
    "Ruby": "I/O Tower beauty. Elegant communication.",
    "PHP": "Game Grid registration. Every Program starts here.",
    "Swift": "Recognizer controls. Fast pursuit programs.",
    "Kotlin": "The ISO evolution. New life on the Grid.",
    "Haskell": "CLU's perfection. Pure logic, no Users.",
    "Assembly": "Direct Grid interface. Flynn's ultimate hack.",
    "Fortran": "Original ENCOM code. Before the Grid.",
    "Lisp": "Kevin Flynn's experiments. Creating life."
  },
  "Bill & Ted": {
    "default": "Excellent! A most triumphant new language!",
    "C": "Rufus's choice. Excellent foundation, dude!",
    "C++": "Bogus complexity, but totally powerful!",
    "Rust": "Memory-safe! Most excellent for the future!",
    "Go": "Simple like our history reports. Station!",
    "Python": "Easy to read! Party on, code!",
    "JavaScript": "Runs in every timeline! Excellent!",
    "TypeScript": "JavaScript with like, rules. Still excellent!",
    "Java": "Like homework. Necessary but bogus.",
    "C_Sharp": "Microsoft stuff. Most educational.",
    "Ruby": "Totally righteous code, dude!",
    "PHP": "Every website in San Dimas!",
    "Swift": "Excellent for making apps!",
    "Kotlin": "Android bodacious, dude!",
    "Assembly": "Direct phone booth interface. Strange things are afoot!",
    "Fortran": "Historical! Like Napoleon!",
    "Lisp": "Whoa, ancient and powerful. Most philosophical!"
  },
  "John Wick": {
    "default": "New contract posted on the language.",
    "C": "The Boogeyman's tool. Clean, efficient, deadly.",
    "C++": "Continental complexity. Rules are rules.",
    "Rust": "Memory-safe. No loose ends.",
    "Go": "Simple execution. One shot, one kill.",
    "Python": "Quick deployment. Read the contract clearly.",
    "JavaScript": "Runs everywhere. No escaping it.",
    "TypeScript": "High Table organization. Everything typed and tracked.",
    "Java": "High Table bureaucracy. Heavy but connected.",
    "C_Sharp": "Continental hotel systems. Secure and reliable.",
    "Ruby": "The Bowery King's style. Elegant street operations.",
    "PHP": "Old contracts. Legacy obligations.",
    "Swift": "Quick response. When the marker is called.",
    "Kotlin": "Modern operations. Adapt or die.",
    "Haskell": "Pure mathematics. The accountant's logic.",
    "Assembly": "Direct neural interface. A man of focus.",
    "Fortran": "The old ways. Before the Table.",
    "Prolog": "Contract queries. Who holds the marker?"
  },
  "Dark Knight": {
    "default": "A new player in Gotham's game of shadows.",
    "C": "The hero Gotham deserves. Clean, efficient, reliable.",
    "C++": "Complex like the criminal mind. Powerful when understood.",
    "Rust": "Why do we fall? So we can learn memory safety.",
    "Go": "Simple justice. Effective vigilantism.",
    "Python": "Alfred's choice. Clear, readable, dependable.",
    "JavaScript": "Runs in every corner of Gotham.",
    "TypeScript": "Batman's preparation. Know your types before the fight.",
    "Java": "Wayne Enterprises bureaucracy. Necessary camouflage.",
    "C_Sharp": "Applied Sciences. Lucius Fox's domain.",
    "Ruby": "Selina Kyle's preference. Elegant and elusive.",
    "PHP": "Gotham's infrastructure. Crumbling but persistent.",
    "Swift": "Tumbler controls. Urban assault mobility.",
    "Kotlin": "Modern crime fighting. The next generation.",
    "Haskell": "Joker logic. Chaos with pure consistency.",
    "Assembly": "Direct Batsuit systems. Full control in the night.",
    "Fortran": "The old Wayne legacy. Foundation of an empire.",
    "Prolog": "Detective work. Query the evidence."
  }
};

// ============================================================================
// LANGUAGE METADATA - Extended info for language detail modals
// ============================================================================
const languageMetadata = {
  "C": { "creator": "Dennis Ritchie", "year": 1972, "paradigm": "Procedural", "typing": "Static, Weak" },
  "C++": { "creator": "Bjarne Stroustrup", "year": 1985, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Python": { "creator": "Guido van Rossum", "year": 1991, "paradigm": "Multi-paradigm", "typing": "Dynamic, Strong" },
  "JavaScript": { "creator": "Brendan Eich", "year": 1995, "paradigm": "Multi-paradigm", "typing": "Dynamic, Weak" },
  "TypeScript": { "creator": "Microsoft", "year": 2012, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Rust": { "creator": "Graydon Hoare", "year": 2010, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Go": { "creator": "Robert Griesemer, Rob Pike, Ken Thompson", "year": 2009, "paradigm": "Concurrent", "typing": "Static, Strong" },
  "Java": { "creator": "James Gosling", "year": 1995, "paradigm": "OOP", "typing": "Static, Strong" },
  "C_Sharp": { "creator": "Microsoft", "year": 2000, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Ruby": { "creator": "Yukihiro Matsumoto", "year": 1995, "paradigm": "Multi-paradigm", "typing": "Dynamic, Strong" },
  "PHP": { "creator": "Rasmus Lerdorf", "year": 1995, "paradigm": "Multi-paradigm", "typing": "Dynamic, Weak" },
  "Swift": { "creator": "Apple Inc.", "year": 2014, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Kotlin": { "creator": "JetBrains", "year": 2011, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Scala": { "creator": "Martin Odersky", "year": 2004, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Haskell": { "creator": "Committee", "year": 1990, "paradigm": "Functional", "typing": "Static, Strong" },
  "OCaml": { "creator": "INRIA", "year": 1996, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "F_Sharp": { "creator": "Microsoft", "year": 2005, "paradigm": "Functional-first", "typing": "Static, Strong" },
  "Clojure": { "creator": "Rich Hickey", "year": 2007, "paradigm": "Functional", "typing": "Dynamic, Strong" },
  "Elixir": { "creator": "José Valim", "year": 2011, "paradigm": "Functional", "typing": "Dynamic, Strong" },
  "Erlang": { "creator": "Ericsson", "year": 1986, "paradigm": "Functional, Concurrent", "typing": "Dynamic, Strong" },
  "Lua": { "creator": "PUC-Rio", "year": 1993, "paradigm": "Multi-paradigm", "typing": "Dynamic, Strong" },
  "Perl": { "creator": "Larry Wall", "year": 1987, "paradigm": "Multi-paradigm", "typing": "Dynamic" },
  "Julia": { "creator": "Jeff Bezanson et al.", "year": 2012, "paradigm": "Multi-paradigm", "typing": "Dynamic, Strong" },
  "Fortran": { "creator": "John Backus", "year": 1957, "paradigm": "Procedural", "typing": "Static, Strong" },
  "COBOL": { "creator": "CODASYL", "year": 1959, "paradigm": "Procedural", "typing": "Static, Strong" },
  "Pascal": { "creator": "Niklaus Wirth", "year": 1970, "paradigm": "Procedural", "typing": "Static, Strong" },
  "Ada": { "creator": "Jean Ichbiah", "year": 1980, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "D": { "creator": "Walter Bright", "year": 2001, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Nim": { "creator": "Andreas Rumpf", "year": 2008, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Crystal": { "creator": "Manas Technology", "year": 2014, "paradigm": "OOP", "typing": "Static, Strong" },
  "Zig": { "creator": "Andrew Kelley", "year": 2016, "paradigm": "Procedural", "typing": "Static, Strong" },
  "V": { "creator": "Alexander Medvednikov", "year": 2019, "paradigm": "Multi-paradigm", "typing": "Static, Strong" },
  "Assembly": { "creator": "Various", "year": 1949, "paradigm": "Imperative", "typing": "None" },
  "Bash": { "creator": "Brian Fox", "year": 1989, "paradigm": "Scripting", "typing": "Dynamic" },
  "Prolog": { "creator": "Alain Colmerauer", "year": 1972, "paradigm": "Logic", "typing": "Dynamic" },
  "Lisp": { "creator": "John McCarthy", "year": 1958, "paradigm": "Functional", "typing": "Dynamic" },
  "Factor": { "creator": "Slava Pestov", "year": 2003, "paradigm": "Stack-based", "typing": "Dynamic, Strong" },
  "Forth": { "creator": "Charles Moore", "year": 1970, "paradigm": "Stack-based", "typing": "None" }
};

// ============================================================================
// DIAGNOSTICS DATA
// ============================================================================
const diagnosticsData = {
  "version": "2.0.0",
  "buildDate": new Date().toISOString(),
  "personaCount": Object.keys(personalities).length,
  "languageCount": Object.keys(personalities["Standard"]).length - 1
};

// ============================================================================
// EXPORT TO WINDOW
// ============================================================================
window.personalities = personalities;
window.narratorIntros = narratorIntros;
window.mismatchLabels = mismatchLabels;
window.iterationLabels = iterationLabels;
window.timeLabels = timeLabels;
window.memoryLabels = memoryLabels;
window.scoreLabels = scoreLabels;
window.languageMetadata = languageMetadata;
window.diagnosticsData = diagnosticsData;

console.log('[StaticData] Loaded', Object.keys(personalities).length, 'personas with language quotes');

// ============================================================================
// EXTENDED LANGUAGE METADATA - With author images and descriptions
// ============================================================================
// Override the basic metadata with richer data including author photos
window.languageMetadata = {
  "C": {
    "year": 1972,
    "creator": "Dennis Ritchie",
    "paradigm": "Procedural, Imperative",
    "typing": "Static, Weak",
    "description": "C is a general-purpose programming language that was developed at Bell Labs. It has greatly influenced many other popular programming languages, most notably C++.",
    "history": "Created at Bell Labs between 1969-1973 as a system implementation language for the Unix operating system.",
    "website": "https://en.cppreference.com/w/c",
    "authors": [
      { "name": "Dennis Ritchie", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Dennis_Ritchie_2011.jpg/220px-Dennis_Ritchie_2011.jpg" }
    ]
  },
  "C++": {
    "year": 1985,
    "creator": "Bjarne Stroustrup",
    "paradigm": "Multi-paradigm, OOP, Generic",
    "typing": "Static, Strong",
    "description": "C++ is a high-performance language with object-oriented, generic, and functional features. It provides low-level memory manipulation while supporting high-level abstractions.",
    "history": "Developed by Bjarne Stroustrup at Bell Labs starting in 1979 as an extension of C.",
    "website": "https://isocpp.org/",
    "authors": [
      { "name": "Bjarne Stroustrup", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/da/BjarneStroustrup.jpg/220px-BjarneStroustrup.jpg" }
    ]
  },
  "Python": {
    "year": 1991,
    "creator": "Guido van Rossum",
    "paradigm": "Multi-paradigm, OOP, Functional",
    "typing": "Dynamic, Strong",
    "description": "Python is a high-level, interpreted language known for its clear syntax and readability. It emphasizes code readability and allows programmers to express concepts in fewer lines of code.",
    "history": "Created by Guido van Rossum and first released in 1991. Named after Monty Python's Flying Circus.",
    "website": "https://python.org/",
    "authors": [
      { "name": "Guido van Rossum", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e2/Guido-portrait-2014-drc.jpg/220px-Guido-portrait-2014-drc.jpg" }
    ]
  },
  "JavaScript": {
    "year": 1995,
    "creator": "Brendan Eich",
    "paradigm": "Multi-paradigm, Event-driven",
    "typing": "Dynamic, Weak",
    "description": "JavaScript is the programming language of the web. It's used for client-side scripting and increasingly for server-side development with Node.js.",
    "history": "Created by Brendan Eich in just 10 days in May 1995 while working at Netscape.",
    "website": "https://developer.mozilla.org/en-US/docs/Web/JavaScript",
    "authors": [
      { "name": "Brendan Eich", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d1/Brendan_Eich_Mozilla_Foundation_official_photo.jpg/220px-Brendan_Eich_Mozilla_Foundation_official_photo.jpg" }
    ]
  },
  "TypeScript": {
    "year": 2012,
    "creator": "Anders Hejlsberg (Microsoft)",
    "paradigm": "Multi-paradigm, OOP",
    "typing": "Static, Strong",
    "description": "TypeScript is a typed superset of JavaScript that compiles to plain JavaScript. It adds optional static typing and class-based object-oriented programming.",
    "history": "Developed by Microsoft and first released in October 2012. Led by Anders Hejlsberg, creator of C#.",
    "website": "https://www.typescriptlang.org/",
    "authors": [
      { "name": "Anders Hejlsberg", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/Anders_Hejlsberg.jpg/220px-Anders_Hejlsberg.jpg" }
    ]
  },
  "Rust": {
    "year": 2010,
    "creator": "Graydon Hoare",
    "paradigm": "Multi-paradigm, Systems",
    "typing": "Static, Strong",
    "description": "Rust is a systems programming language focused on safety, speed, and concurrency. It achieves memory safety without garbage collection through its ownership system.",
    "history": "Started as a personal project by Graydon Hoare in 2006, later sponsored by Mozilla. Version 1.0 released in 2015.",
    "website": "https://www.rust-lang.org/",
    "authors": [
      { "name": "Graydon Hoare", "image": "https://pbs.twimg.com/profile_images/1313496791769403393/0HHfjcoe_400x400.jpg" }
    ]
  },
  "Go": {
    "year": 2009,
    "creator": "Robert Griesemer, Rob Pike, Ken Thompson",
    "paradigm": "Concurrent, Imperative",
    "typing": "Static, Strong",
    "description": "Go is a statically typed, compiled language designed at Google. It provides excellent support for concurrent programming and is known for fast compilation.",
    "history": "Designed at Google in 2007 by Robert Griesemer, Rob Pike, and Ken Thompson. Announced in 2009.",
    "website": "https://go.dev/",
    "authors": [
      { "name": "Rob Pike", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2b/Rob_Pike_cropped.jpg/220px-Rob_Pike_cropped.jpg" },
      { "name": "Ken Thompson", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Ken_Thompson_and_Dennis_Ritchie--1973.jpg/220px-Ken_Thompson_and_Dennis_Ritchie--1973.jpg" },
      { "name": "Robert Griesemer", "image": "https://www.gophercon.com/hubfs/Robert%20Griesemer.jpeg" }
    ]
  },
  "Java": {
    "year": 1995,
    "creator": "James Gosling",
    "paradigm": "OOP, Class-based",
    "typing": "Static, Strong",
    "description": "Java is a class-based, object-oriented programming language designed to have as few implementation dependencies as possible. Write once, run anywhere.",
    "history": "Developed by James Gosling at Sun Microsystems. Released in 1995 as a core component of Sun's Java platform.",
    "website": "https://www.java.com/",
    "authors": [
      { "name": "James Gosling", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/14/James_Gosling_2008.jpg/220px-James_Gosling_2008.jpg" }
    ]
  },
  "C_Sharp": {
    "year": 2000,
    "creator": "Anders Hejlsberg (Microsoft)",
    "paradigm": "Multi-paradigm, OOP",
    "typing": "Static, Strong",
    "description": "C# is a modern, object-oriented programming language developed by Microsoft as part of its .NET initiative. It combines the power of C++ with the simplicity of Visual Basic.",
    "history": "Developed by Microsoft, led by Anders Hejlsberg. First released in 2000 with .NET Framework.",
    "website": "https://docs.microsoft.com/en-us/dotnet/csharp/",
    "authors": [
      { "name": "Anders Hejlsberg", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/Anders_Hejlsberg.jpg/220px-Anders_Hejlsberg.jpg" }
    ]
  },
  "Ruby": {
    "year": 1995,
    "creator": "Yukihiro Matsumoto",
    "paradigm": "Multi-paradigm, OOP",
    "typing": "Dynamic, Strong",
    "description": "Ruby is a dynamic, open source programming language with a focus on simplicity and productivity. It has an elegant syntax that is natural to read and easy to write.",
    "history": "Created by Yukihiro 'Matz' Matsumoto in Japan, first released in 1995. Gained popularity with Ruby on Rails.",
    "website": "https://www.ruby-lang.org/",
    "authors": [
      { "name": "Yukihiro Matsumoto", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Yukihiro_Matsumoto.JPG/220px-Yukihiro_Matsumoto.JPG" }
    ]
  },
  "PHP": {
    "year": 1995,
    "creator": "Rasmus Lerdorf",
    "paradigm": "Multi-paradigm, Imperative",
    "typing": "Dynamic, Weak",
    "description": "PHP is a popular general-purpose scripting language especially suited to web development. It powers a large portion of the web including WordPress.",
    "history": "Created by Rasmus Lerdorf in 1994. Originally stood for Personal Home Page, now PHP: Hypertext Preprocessor.",
    "website": "https://www.php.net/",
    "authors": [
      { "name": "Rasmus Lerdorf", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6b/Rasmus_Lerdorf_August_2014_%28cropped%29.JPG/220px-Rasmus_Lerdorf_August_2014_%28cropped%29.JPG" }
    ]
  },
  "Swift": {
    "year": 2014,
    "creator": "Chris Lattner (Apple)",
    "paradigm": "Multi-paradigm, Protocol-oriented",
    "typing": "Static, Strong",
    "description": "Swift is a powerful and intuitive programming language for iOS, macOS, and beyond. It's designed to be safe, fast, and expressive.",
    "history": "Developed by Apple Inc., primarily by Chris Lattner. Announced at WWDC 2014.",
    "website": "https://swift.org/",
    "authors": [
      { "name": "Chris Lattner", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5f/Chris_Lattner_2017.jpg/220px-Chris_Lattner_2017.jpg" }
    ]
  },
  "Kotlin": {
    "year": 2011,
    "creator": "JetBrains",
    "paradigm": "Multi-paradigm, OOP, Functional",
    "typing": "Static, Strong",
    "description": "Kotlin is a modern programming language that makes developers happier. It's concise, safe, interoperable with Java, and the preferred language for Android development.",
    "history": "Developed by JetBrains. Named after Kotlin Island near St. Petersburg. Version 1.0 released in 2016.",
    "website": "https://kotlinlang.org/",
    "authors": [
      { "name": "Andrey Breslav", "image": "https://pbs.twimg.com/profile_images/1293575782003126273/U3j0qC3B_400x400.jpg" }
    ]
  },
  "Scala": {
    "year": 2004,
    "creator": "Martin Odersky",
    "paradigm": "Multi-paradigm, Functional, OOP",
    "typing": "Static, Strong",
    "description": "Scala combines object-oriented and functional programming in one concise, high-level language. It runs on the JVM and is known for scalable applications.",
    "history": "Designed by Martin Odersky at EPFL. First released in 2004. Powers Apache Spark.",
    "website": "https://www.scala-lang.org/",
    "authors": [
      { "name": "Martin Odersky", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Mark_Odersky_photo_by_Linda_Poeng.jpg/220px-Mark_Odersky_photo_by_Linda_Poeng.jpg" }
    ]
  },
  "Haskell": {
    "year": 1990,
    "creator": "Committee",
    "paradigm": "Functional, Pure",
    "typing": "Static, Strong",
    "description": "Haskell is a purely functional programming language with strong static typing and lazy evaluation. It's known for its mathematical rigor.",
    "history": "Named after logician Haskell Curry. Designed by a committee starting in 1987, first version in 1990.",
    "website": "https://www.haskell.org/",
    "authors": [
      { "name": "Simon Peyton Jones", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Simon_Peyton_Jones.jpg/220px-Simon_Peyton_Jones.jpg" },
      { "name": "Philip Wadler", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/Philip_Wadler.jpg/220px-Philip_Wadler.jpg" }
    ]
  },
  "OCaml": {
    "year": 1996,
    "creator": "INRIA",
    "paradigm": "Multi-paradigm, Functional",
    "typing": "Static, Strong",
    "description": "OCaml is an industrial-strength functional programming language with an emphasis on expressiveness and safety. It features a powerful type system.",
    "history": "Developed at INRIA in France. Evolved from Caml, which was based on ML.",
    "website": "https://ocaml.org/",
    "authors": [
      { "name": "Xavier Leroy", "image": "https://xavierleroy.org/xavier-web.jpg" }
    ]
  },
  "F_Sharp": {
    "year": 2005,
    "creator": "Don Syme (Microsoft)",
    "paradigm": "Functional-first, Multi-paradigm",
    "typing": "Static, Strong",
    "description": "F# is a functional-first programming language that runs on .NET. It makes it easy to write succinct, robust and performant code.",
    "history": "Developed by Microsoft Research, led by Don Syme. First appeared in 2005.",
    "website": "https://fsharp.org/",
    "authors": [
      { "name": "Don Syme", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Don_Syme_in_2023.jpg/220px-Don_Syme_in_2023.jpg" }
    ]
  },
  "Clojure": {
    "year": 2007,
    "creator": "Rich Hickey",
    "paradigm": "Functional, Lisp",
    "typing": "Dynamic, Strong",
    "description": "Clojure is a dynamic, functional dialect of the Lisp programming language on the Java platform. It embraces immutable data structures.",
    "history": "Created by Rich Hickey, first released in 2007. Runs on the JVM.",
    "website": "https://clojure.org/",
    "authors": [
      { "name": "Rich Hickey", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/b/be/Rich_Hickey_speaking_at_the_first_Clojure_conj_in_Durham%2C_NC.jpg/220px-Rich_Hickey_speaking_at_the_first_Clojure_conj_in_Durham%2C_NC.jpg" }
    ]
  },
  "Elixir": {
    "year": 2011,
    "creator": "José Valim",
    "paradigm": "Functional, Concurrent",
    "typing": "Dynamic, Strong",
    "description": "Elixir is a dynamic, functional language for building scalable and maintainable applications. It leverages the Erlang VM for distributed, fault-tolerant systems.",
    "history": "Created by José Valim, a former Rails core team member. First released in 2011.",
    "website": "https://elixir-lang.org/",
    "authors": [
      { "name": "José Valim", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7e/Jose_valim.jpg/220px-Jose_valim.jpg" }
    ]
  },
  "Erlang": {
    "year": 1986,
    "creator": "Ericsson",
    "paradigm": "Functional, Concurrent",
    "typing": "Dynamic, Strong",
    "description": "Erlang is a programming language used to build massively scalable soft real-time systems with requirements on high availability.",
    "history": "Developed at Ericsson for telecom systems. Designed by Joe Armstrong, Robert Virding, and Mike Williams.",
    "website": "https://www.erlang.org/",
    "authors": [
      { "name": "Joe Armstrong", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6e/Joe_Armstrong_-_Erlang_User_Conference_2013.JPG/220px-Joe_Armstrong_-_Erlang_User_Conference_2013.JPG" }
    ]
  },
  "Lua": {
    "year": 1993,
    "creator": "PUC-Rio",
    "paradigm": "Multi-paradigm, Scripting",
    "typing": "Dynamic, Strong",
    "description": "Lua is a powerful, efficient, lightweight, embeddable scripting language. It's widely used in game development and embedded systems.",
    "history": "Created at PUC-Rio in Brazil by Roberto Ierusalimschy, Waldemar Celes, and Luiz Henrique de Figueiredo.",
    "website": "https://www.lua.org/",
    "authors": [
      { "name": "Roberto Ierusalimschy", "image": "https://www.lua.org/images/roberto.jpg" }
    ]
  },
  "Perl": {
    "year": 1987,
    "creator": "Larry Wall",
    "paradigm": "Multi-paradigm",
    "typing": "Dynamic",
    "description": "Perl is a family of high-level, general-purpose, interpreted, dynamic programming languages. Famous for text processing and CGI scripts.",
    "history": "Created by Larry Wall in 1987. Originally developed for text manipulation on Unix systems.",
    "website": "https://www.perl.org/",
    "authors": [
      { "name": "Larry Wall", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/Larry_Wall_YAPC_2007.jpg/220px-Larry_Wall_YAPC_2007.jpg" }
    ]
  },
  "Julia": {
    "year": 2012,
    "creator": "Jeff Bezanson, Stefan Karpinski, Viral B. Shah, Alan Edelman",
    "paradigm": "Multi-paradigm, Scientific",
    "typing": "Dynamic, Strong",
    "description": "Julia is a high-level, high-performance dynamic language for technical computing. It combines the speed of C with the usability of Python.",
    "history": "Development began in 2009 at MIT. First public release in 2012. Version 1.0 in 2018.",
    "website": "https://julialang.org/",
    "authors": [
      { "name": "Jeff Bezanson", "image": "https://julialang.org/assets/infra/bezanson.jpg" },
      { "name": "Stefan Karpinski", "image": "https://julialang.org/assets/infra/karpinski.jpg" },
      { "name": "Alan Edelman", "image": "https://julialang.org/assets/infra/edelman.jpg" }
    ]
  },
  "Fortran": {
    "year": 1957,
    "creator": "John Backus (IBM)",
    "paradigm": "Procedural, Imperative",
    "typing": "Static, Strong",
    "description": "Fortran (Formula Translation) is one of the oldest programming languages. It remains dominant in scientific and numerical computing.",
    "history": "Developed by IBM in the 1950s, led by John Backus. First high-level programming language.",
    "website": "https://fortran-lang.org/",
    "authors": [
      { "name": "John Backus", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/John_Backus_2.jpg/220px-John_Backus_2.jpg" }
    ]
  },
  "Pascal": {
    "year": 1970,
    "creator": "Niklaus Wirth",
    "paradigm": "Procedural, Imperative",
    "typing": "Static, Strong",
    "description": "Pascal is an influential procedural programming language designed for teaching programming. It encourages good programming practices.",
    "history": "Designed by Niklaus Wirth and published in 1970. Named after mathematician Blaise Pascal.",
    "website": "https://www.freepascal.org/",
    "authors": [
      { "name": "Niklaus Wirth", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Niklaus_Wirth%2C_UrGU.jpg/220px-Niklaus_Wirth%2C_UrGU.jpg" }
    ]
  },
  "Ada": {
    "year": 1980,
    "creator": "Jean Ichbiah",
    "paradigm": "Multi-paradigm, OOP",
    "typing": "Static, Strong",
    "description": "Ada is a structured, statically typed, imperative, and object-oriented programming language designed for safety-critical and real-time systems.",
    "history": "Developed in response to a US Department of Defense mandate. Named after Ada Lovelace.",
    "website": "https://www.adacore.com/",
    "authors": [
      { "name": "Jean Ichbiah", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Jean_Ichbiah_at_the_Ada_Lovelace_Bicentenary.jpg/220px-Jean_Ichbiah_at_the_Ada_Lovelace_Bicentenary.jpg" }
    ]
  },
  "D": {
    "year": 2001,
    "creator": "Walter Bright",
    "paradigm": "Multi-paradigm, Systems",
    "typing": "Static, Strong",
    "description": "D is a systems programming language with C-like syntax and static typing. It provides features missing from C/C++ while maintaining compatibility.",
    "history": "Developed by Walter Bright, the creator of the first native C++ compiler. First released in 2001.",
    "website": "https://dlang.org/",
    "authors": [
      { "name": "Walter Bright", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/78/WalterBright.jpg/220px-WalterBright.jpg" }
    ]
  },
  "Nim": {
    "year": 2008,
    "creator": "Andreas Rumpf",
    "paradigm": "Multi-paradigm, Systems",
    "typing": "Static, Strong",
    "description": "Nim is a statically typed, compiled systems language with a syntax similar to Python. It compiles to C, C++, or JavaScript.",
    "history": "Created by Andreas Rumpf. Initially called Nimrod. Version 1.0 released in 2019.",
    "website": "https://nim-lang.org/",
    "authors": [
      { "name": "Andreas Rumpf", "image": "https://avatars.githubusercontent.com/u/567790" }
    ]
  },
  "Crystal": {
    "year": 2014,
    "creator": "Manas Technology Solutions",
    "paradigm": "OOP, Concurrent",
    "typing": "Static, Strong",
    "description": "Crystal is a programming language with a syntax similar to Ruby but compiles to native code. It offers type inference and compile-time checks.",
    "history": "Development started in 2011 at Manas Technology Solutions. First released in 2014.",
    "website": "https://crystal-lang.org/",
    "authors": [
      { "name": "Ary Borenszweig", "image": "https://avatars.githubusercontent.com/u/34973" }
    ]
  },
  "Zig": {
    "year": 2016,
    "creator": "Andrew Kelley",
    "paradigm": "Procedural, Systems",
    "typing": "Static, Strong",
    "description": "Zig is a systems programming language emphasizing performance and correctness. It's designed as a better C, with no hidden control flow.",
    "history": "Created by Andrew Kelley in 2016. Gained attention as an alternative to C for systems programming.",
    "website": "https://ziglang.org/",
    "authors": [
      { "name": "Andrew Kelley", "image": "https://andrewkelley.me/img/andrew-2019.jpg" }
    ]
  },
  "V": {
    "year": 2019,
    "creator": "Alexander Medvednikov",
    "paradigm": "Multi-paradigm, Systems",
    "typing": "Static, Strong",
    "description": "V is a simple, fast, safe language for maintainable and scalable development. Compiles in milliseconds and has no undefined behavior.",
    "website": "https://vlang.io/",
    "authors": [
      { "name": "Alexander Medvednikov", "image": "https://avatars.githubusercontent.com/u/8041776" }
    ]
  },
  "Assembly": {
    "year": 1949,
    "creator": "Various",
    "paradigm": "Imperative, Low-level",
    "typing": "None",
    "description": "Assembly language is a low-level programming language with a strong correspondence to machine code instructions. Each processor architecture has its own assembly language.",
    "history": "The first assembly languages were developed in the 1940s. They provided symbolic representation of machine code.",
    "website": "https://en.wikipedia.org/wiki/Assembly_language",
    "authors": []
  },
  "Bash": {
    "year": 1989,
    "creator": "Brian Fox",
    "paradigm": "Scripting, Shell",
    "typing": "Dynamic",
    "description": "Bash is a Unix shell and command language. It's the default shell on most Linux distributions and macOS, used for scripting and automation.",
    "history": "Written by Brian Fox for the GNU Project as a free replacement for the Bourne shell.",
    "website": "https://www.gnu.org/software/bash/",
    "authors": [
      { "name": "Brian Fox", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Brian_Fox_%28computer_programmer%29.jpg/220px-Brian_Fox_%28computer_programmer%29.jpg" }
    ]
  },
  "Prolog": {
    "year": 1972,
    "creator": "Alain Colmerauer",
    "paradigm": "Logic, Declarative",
    "typing": "Dynamic",
    "description": "Prolog is a logic programming language associated with artificial intelligence and computational linguistics. Programs are expressed as relations.",
    "history": "Created by Alain Colmerauer and Robert Kowalski in 1972. Used extensively in AI research.",
    "website": "https://www.swi-prolog.org/",
    "authors": [
      { "name": "Alain Colmerauer", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/8/80/Alain_Colmerauer.jpg/220px-Alain_Colmerauer.jpg" }
    ]
  },
  "Lisp": {
    "year": 1958,
    "creator": "John McCarthy",
    "paradigm": "Functional, Multi-paradigm",
    "typing": "Dynamic",
    "description": "Lisp is the second-oldest high-level programming language still in use. It pioneered many ideas in computer science including tree data structures and garbage collection.",
    "history": "Created by John McCarthy at MIT in 1958. The name stands for LISt Processor.",
    "website": "https://common-lisp.net/",
    "authors": [
      { "name": "John McCarthy", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/John_McCarthy_Stanford.jpg/220px-John_McCarthy_Stanford.jpg" }
    ]
  },
  "Factor": {
    "year": 2003,
    "creator": "Slava Pestov",
    "paradigm": "Stack-based, Concatenative",
    "typing": "Dynamic, Strong",
    "description": "Factor is a stack-based, concatenative programming language. It features a powerful macro system and foreign function interface.",
    "history": "Created by Slava Pestov in 2003. Influenced by Forth and Joy.",
    "website": "https://factorcode.org/",
    "authors": [
      { "name": "Slava Pestov", "image": "https://avatars.githubusercontent.com/u/66889" }
    ]
  },
  "Forth": {
    "year": 1970,
    "creator": "Charles Moore",
    "paradigm": "Stack-based, Concatenative",
    "typing": "None",
    "description": "Forth is a procedural, stack-oriented programming language and interactive environment. It's used in embedded systems and boot loaders.",
    "history": "Created by Charles Moore in 1970. Originally developed for telescope control.",
    "website": "https://www.forth.com/",
    "authors": [
      { "name": "Charles Moore", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Charles_H._Moore.jpg/220px-Charles_H._Moore.jpg" }
    ]
  },
  "R": {
    "year": 1993,
    "creator": "Ross Ihaka, Robert Gentleman",
    "paradigm": "Multi-paradigm, Functional",
    "typing": "Dynamic",
    "description": "R is a programming language for statistical computing and graphics. It's widely used among statisticians and data scientists.",
    "history": "Created by Ross Ihaka and Robert Gentleman at the University of Auckland. Based on S language.",
    "website": "https://www.r-project.org/",
    "authors": [
      { "name": "Ross Ihaka", "image": "https://www.stat.auckland.ac.nz/~ihaka/ihaka.jpg" },
      { "name": "Robert Gentleman", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/Robert_Gentleman_%28scientist%29.jpg/220px-Robert_Gentleman_%28scientist%29.jpg" }
    ]
  },
  "COBOL": {
    "year": 1959,
    "creator": "CODASYL Committee",
    "paradigm": "Procedural, Imperative",
    "typing": "Static, Strong",
    "description": "COBOL (Common Business-Oriented Language) is designed for business use. It still processes an estimated 95% of ATM transactions globally.",
    "history": "Developed by the CODASYL committee including Grace Hopper. First standardized in 1968.",
    "website": "https://www.ibm.com/products/cobol-compiler-zos",
    "authors": [
      { "name": "Grace Hopper", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Commodore_Grace_M._Hopper%2C_USN_%28covered%29.jpg/220px-Commodore_Grace_M._Hopper%2C_USN_%28covered%29.jpg" }
    ]
  },
  "Dart": {
    "year": 2011,
    "creator": "Lars Bak, Kasper Lund (Google)",
    "paradigm": "Multi-paradigm, OOP",
    "typing": "Static, Strong",
    "description": "Dart is a client-optimized language for fast apps on any platform. It's the language behind Flutter for cross-platform development.",
    "history": "Developed by Google, unveiled in 2011. Powers the Flutter framework.",
    "website": "https://dart.dev/",
    "authors": [
      { "name": "Lars Bak", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/Lars_Bak.jpg/220px-Lars_Bak.jpg" }
    ]
  },
  "Objective-C": {
    "year": 1984,
    "creator": "Brad Cox, Tom Love",
    "paradigm": "OOP, Reflective",
    "typing": "Static, Weak",
    "description": "Objective-C is an object-oriented language that adds Smalltalk-style messaging to C. It was the main language for macOS and iOS development before Swift.",
    "history": "Created by Brad Cox and Tom Love in the early 1980s. Later adopted by NeXT and Apple.",
    "website": "https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/",
    "authors": [
      { "name": "Brad Cox", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Brad_Cox.jpg/220px-Brad_Cox.jpg" }
    ]
  },
  "Groovy": {
    "year": 2003,
    "creator": "James Strachan",
    "paradigm": "Multi-paradigm, OOP",
    "typing": "Dynamic, Strong",
    "description": "Groovy is a dynamic language for the Java platform with features similar to Python, Ruby, and Smalltalk. Used in Gradle build system.",
    "website": "https://groovy-lang.org/",
    "authors": [
      { "name": "James Strachan", "image": "https://avatars.githubusercontent.com/u/30011" }
    ]
  },
  "Smalltalk": {
    "year": 1972,
    "creator": "Alan Kay, Xerox PARC",
    "paradigm": "OOP, Pure",
    "typing": "Dynamic, Strong",
    "description": "Smalltalk is an object-oriented, dynamically typed, reflective programming language. It pioneered many concepts in OOP and GUIs.",
    "history": "Developed at Xerox PARC, led by Alan Kay. First public version (Smalltalk-80) released in 1980.",
    "website": "https://www.squeak.org/",
    "authors": [
      { "name": "Alan Kay", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Alan_Kay_%283097597186%29.jpg/220px-Alan_Kay_%283097597186%29.jpg" },
      { "name": "Dan Ingalls", "image": "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a4/Dan_Ingalls.jpg/220px-Dan_Ingalls.jpg" }
    ]
  }
};

console.log('[StaticData] Extended language metadata with author images loaded');
