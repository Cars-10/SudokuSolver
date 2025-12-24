// PersonaMetadata.ts - Persona-specific UI text and themes
// Separated from LanguagesMetadata.ts to avoid conflation with language data
// --- Methodology Texts (explains scoring per persona) ---
export const methodologyTexts = {
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
// --- UI Label Mappings (changes based on persona) ---
export const mismatchLabels = {
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
export const iterationLabels = {
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
export const timeLabels = {
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
export const memoryLabels = {
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
export const scoreLabels = {
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
// --- Narrator Intros (intro text shown per persona) ---
export const narratorIntros = {
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
// --- Personalities (language descriptions per persona) ---
export const personalities = {
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
