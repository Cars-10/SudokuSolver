import * as fs from 'fs/promises';
import * as path from 'path';
import { glob } from 'glob';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');

interface LangConfig {
    ext: string;
    setup?: string;
    run: string;
}

const CONFIGS: Record<string, LangConfig> = {
    'C': { ext: 'Sudoku.c', setup: 'gcc -O3 -o Sudoku Sudoku.c', run: './Sudoku' },
    'C++': { ext: 'Sudoku.cpp', setup: 'g++ -O3 -o Sudoku Sudoku.cpp', run: './Sudoku' },
    'Rust': { ext: 'Sudoku.rs', setup: 'rustc -C opt-level=3 Sudoku.rs', run: './Sudoku' },
    'Go': { ext: 'Sudoku.go', setup: 'go build -o Sudoku Sudoku.go', run: './Sudoku' },
    'Swift': { ext: 'Sudoku.swift', setup: 'swiftc -O Sudoku.swift', run: './Sudoku' },
    'D': { ext: 'Sudoku.d', setup: 'dmd -O -release -inline -of=Sudoku Sudoku.d', run: './Sudoku' },
    'Nim': { ext: 'Sudoku.nim', setup: 'nim c -d:danger -d:release --out:Sudoku Sudoku.nim', run: './Sudoku' },
    'Crystal': { ext: 'Sudoku.cr', setup: 'crystal build --release -o Sudoku Sudoku.cr', run: './Sudoku' },
    'Java': { ext: 'Sudoku.java', setup: 'javac Sudoku.java', run: 'java Sudoku' },
    'Scala': { ext: 'Sudoku.scala', setup: 'scalac Sudoku.scala', run: 'scala Sudoku' },
    'Kotlin': { ext: 'Sudoku.kt', setup: 'kotlinc Sudoku.kt -include-runtime -d Sudoku.jar', run: 'java -jar Sudoku.jar' },
    'Python': { ext: 'Sudoku.py', run: 'python3 Sudoku.py' },
    'Ruby': { ext: 'Sudoku.rb', run: 'ruby Sudoku.rb' },
    'PHP': { ext: 'Sudoku.php', run: 'php Sudoku.php' },
    'Perl': { ext: 'Sudoku.pl', run: 'perl Sudoku.pl' },
    'Lua': { ext: 'Sudoku.lua', run: 'lua Sudoku.lua' },
    'JavaScript': { ext: 'Sudoku.js', run: 'node Sudoku.js' },
    'TypeScript': { ext: 'Sudoku.ts', run: 'npx ts-node Sudoku.ts' },
    'Awk': { ext: 'Sudoku.awk', run: 'awk -f Sudoku.awk' },
    'Bash': { ext: 'Sudoku.sh', run: 'bash Sudoku.sh' },
    'R': { ext: 'Sudoku.R', run: 'Rscript Sudoku.R' },
    'Julia': { ext: 'Sudoku.jl', run: 'julia Sudoku.jl' },
    'Elixir': { ext: 'Sudoku.exs', run: 'elixir Sudoku.exs' },
    'Tcl': { ext: 'Sudoku.tcl', run: 'tclsh Sudoku.tcl' },
    'Assembly': { ext: 'Sudoku.asm', setup: 'nasm -f macho64 Sudoku.asm && ld -macosx_version_min 10.7.0 -o Sudoku Sudoku.o -lSystem', run: './Sudoku' }, // macOS specific assumption
};

async function generate() {
    console.log('Generating execution scripts...');
    const langDirs = await glob(path.join(rootDir, 'CleanedUp', 'Languages', '*'));

    for (const dir of langDirs) {
        const langName = path.basename(dir);
        let config = CONFIGS[langName];

        if (!config) {
            // Try to auto-detect by file extension if possible, or skip
            console.warn(`No config for ${langName}, checking files...`);
            // heuristic detection could go here, skipping for now to rely on explicit config
            continue;
        }

        const scriptPath = path.join(dir, 'setupAndRunMe.sh');

        // Define content
        const setupBlock = config.setup ? `
# Compile/Setup
if [ -f "${config.ext}" ]; then
    ${config.setup}
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
else
    report_env_error "${config.ext} not found"
fi` : `
# No compilation needed
if [ ! -f "${config.ext}" ]; then
    report_env_error "${config.ext} not found"
fi`;

        const content = `#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"

# Function to report environment error
report_env_error() {
    echo "[" > "$METRICS_FILE"
    echo "  {" >> "$METRICS_FILE"
    echo "    \\"solver\\": \\"${langName}\\", " >> "$METRICS_FILE"
    echo "    \\"status\\": \\"error\\", " >> "$METRICS_FILE"
    echo "    \\"output\\": \\"Environment Not Ready: $1\\"" >> "$METRICS_FILE"
    echo "  }" >> "$METRICS_FILE"
    echo "]" >> "$METRICS_FILE"
    cat "$METRICS_FILE"
    exit 1
}

# --- SETUP PHASE ---
${setupBlock}

# --- EXECUTION PHASE ---
echo "[" > "$METRICS_FILE"

first=true
MATRICES="$@"

if [ -z "$MATRICES" ]; then
    MATRICES="../../../Matrices/*.matrix"
fi

for matrix in $MATRICES; do
    if [ "$first" = true ]; then
        first=false
    else
        echo "," >> "$METRICS_FILE"
    fi

    matrix_name=$(basename "$matrix")
    
    tmp_out=$(mktemp)
    tmp_err=$(mktemp)
    
    # Python timestamp for precision
    start_time=$(python3 -c 'import time; print(time.time())')
    
    # Detect OS for time command
    OS="$(uname)"
    if [ "$OS" = "Darwin" ]; then
        /usr/bin/time -l ${config.run} "$matrix" > "$tmp_out" 2> "$tmp_err"
        memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
        cpu_user=$(grep "user" "$tmp_err" | awk '{print $3}')
        cpu_sys=$(grep "sys" "$tmp_err" | awk '{print $5}')
    else
        /usr/bin/time -v ${config.run} "$matrix" > "$tmp_out" 2> "$tmp_err"
        memory=$(grep "Maximum resident set size" "$tmp_err" | awk -F': ' '{print $2}')
        # Convert KB to Bytes
        memory=$((memory * 1024))
        cpu_user=$(grep "User time" "$tmp_err" | awk -F': ' '{print $2}')
        cpu_sys=$(grep "System time" "$tmp_err" | awk -F': ' '{print $2}')
    fi
    
    end_time=$(python3 -c 'import time; print(time.time())')
    duration=$(awk -v start=$start_time -v end=$end_time 'BEGIN { printf "%.6f", end - start }')

    # Validation
    iterations=$(grep -a "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2 | tr -d '[:space:]')
    if [ -z "$iterations" ]; then
        iterations=0
        status="fail"
    else
        status="pass"
    fi
     
    # Result
    echo "  {" >> "$METRICS_FILE"
    echo "    \\"matrix\\": \\"$matrix_name\\"," >> "$METRICS_FILE"
    echo "    \\"time\\": $duration," >> "$METRICS_FILE"
    echo "    \\"iterations\\": $iterations," >> "$METRICS_FILE"
    echo "    \\"memory\\": $memory," >> "$METRICS_FILE"
    echo "    \\"cpu_user\\": $cpu_user," >> "$METRICS_FILE"
    echo "    \\"cpu_sys\\": $cpu_sys," >> "$METRICS_FILE"
    echo "    \\"status\\": \\"$status\\"," >> "$METRICS_FILE"
    
    # Safe JSON output capture
    json_output=$(cat "$tmp_out" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')
    echo "    \\"output\\": $json_output" >> "$METRICS_FILE"
    echo -n "  }" >> "$METRICS_FILE"
    
    rm "$tmp_out" "$tmp_err"
done

echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"
cat "$METRICS_FILE"
`;
        await fs.writeFile(scriptPath, content);
        await fs.chmod(scriptPath, 0o755);
        console.log(`Generated ${scriptPath}`);
    }
}

generate().catch(console.error);
