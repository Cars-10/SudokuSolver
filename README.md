# Multi-Language Sudoku Solver Benchmark Suite

A comprehensive cross-language performance benchmarking project featuring **85+ programming language implementations** of an identical Sudoku solver algorithm. This project provides objective, reproducible performance comparisons across the vast landscape of programming languages.

## Table of Contents

- [Overview](#overview)
- [Project History](#project-history)
- [Features](#features)
- [Quick Start](#quick-start)
- [Architecture](#architecture)
- [Usage](#usage)
- [Adding a New Language](#adding-a-new-language)
- [Algorithm Specification](#algorithm-specification)
- [Performance Metrics](#performance-metrics)
- [Contributing](#contributing)
- [License](#license)

## Overview

This project implements an identical brute-force backtracking Sudoku solver across 85+ programming languages to enable **fair, apples-to-apples performance comparisons**. Every implementation follows the exact same algorithm specification, ensuring that performance differences reflect the languages themselves, not algorithmic variations.

### Key Principles

1. **Algorithm Consistency** - Every language uses the identical backtracking algorithm
2. **Iteration Counting** - The iteration count serves as an algorithmic fingerprint (656 iterations for Matrix 1)
3. **No Optimizations** - No MRV heuristics, constraint propagation, or smart search strategies
4. **Reproducible Results** - Deterministic algorithm with consistent ordering guarantees identical results
5. **Comprehensive Metrics** - Time, memory, CPU usage, I/O, context switches, and more

## Project History

### Origins (Early Commits)

The project began as a simple C implementation of a Sudoku solver:

- **First commit** (55851b0): Basic C Sudoku solver
- **Evolution**: Added timing functions, command-line input, matrix formatting
- **Standardization**: Created identical output format across implementations

### Growth to Multi-Language Suite

What started as a single-language project evolved into a comprehensive benchmark suite:

1. **Phase 1**: Core languages (C, C++, Python, Java, Go, Rust)
2. **Phase 2**: Expansion to popular languages (JavaScript, Ruby, PHP, Swift, Kotlin)
3. **Phase 3**: Systems languages (Fortran, Ada, D, Nim, Crystal, Zig)
4. **Phase 4**: Functional languages (Haskell, OCaml, F#, Elixir, Erlang, Clojure)
5. **Phase 5**: Scripting languages (Lua, Perl, Tcl, Awk, sed)
6. **Phase 6**: Esoteric and specialized languages (Prolog, COBOL, Forth, APL, Rexx)

### Modern Infrastructure (Recent Development)

- **Web Interface**: Interactive benchmark runner with real-time execution
- **Docker Support**: Containerized environment with all 85+ toolchains pre-installed
- **Report Generation**: Automated HTML report generation with visualizations
- **Metadata System**: Language personalities, compiler variants, and custom configurations
- **Validation Framework**: Automated iteration count validation against reference

## Features

### 🚀 Core Capabilities

- **85+ Language Implementations**: From Assembly to Zig, covering compiled, interpreted, JVM, functional, and esoteric languages
- **Identical Algorithm**: Every implementation follows the exact same backtracking specification
- **Automated Benchmarking**: Run single languages, specific matrices, or full benchmark suites
- **Docker Integration**: Pre-configured container with all language toolchains installed
- **Interactive Web UI**: Run benchmarks, view results, and compare languages in real-time

### 📊 Comprehensive Metrics

Each benchmark run captures:

- **Execution Time**: Wall clock time in milliseconds
- **Iteration Count**: Algorithm fingerprint (must match reference)
- **Memory Usage**: Peak memory consumption in bytes
- **CPU Metrics**: User time, system time, total CPU usage
- **OS Metrics**: Page faults (major/minor), context switches (voluntary/involuntary)
- **I/O Statistics**: Input/output operations count
- **Exit Status**: Success, timeout, error, or environment error

### 📈 Advanced Reporting

- **HTML Report Generation**: Beautiful, interactive reports with sorting and filtering
- **Performance Visualization**: Charts comparing execution time across languages
- **Compiler Variants**: Test different optimization levels (O0, O2, O3, Ofast)
- **Historical Tracking**: Compare performance across multiple benchmark runs
- **Language Metadata**: Custom descriptions, logos, personality quotes, and notes

### 🔧 Developer Experience

- **Shared Build System**: Common `common.sh` library for consistent benchmark execution
- **Automatic Validation**: Iteration count checking against C reference implementation
- **Toolchain Detection**: Automatic compiler/interpreter version detection
- **Timeout Handling**: Configurable timeouts prevent infinite loops
- **Error Reporting**: Detailed error messages for debugging failed runs
- **Session State**: Lock/unlock languages, track pending benchmarks

### 🐳 Docker Environment

- **Pre-installed Toolchains**: All 85+ languages ready to use
- **Consistent Environment**: Eliminates "works on my machine" issues
- **Volume Mounting**: Live code updates without rebuilding container
- **Port Mapping**: Access web interface at `localhost:9001`
- **Server Control Scripts**: Easy start/stop/status management

### 🌐 Web Interface Features

- **Language Selection**: Choose any language from the dropdown
- **Matrix Selection**: Run individual matrices (1-6) or full suite
- **Real-time Output**: See solver output as it executes
- **Run Type Selection**: Toggle between Local and Docker execution
- **Metrics Display**: View detailed performance metrics for each run
- **Report Generation**: Generate updated HTML reports on-demand
- **Source Code Viewer**: Inspect solver implementations directly in the UI
- **Metadata Editor**: Edit language descriptions, upload logos, customize display

## Quick Start

### Prerequisites

- **macOS/Linux**: Required for shell scripts
- **Docker** (recommended): For consistent environment
- **OR** Language-specific toolchains installed locally

### Running Your First Benchmark

#### Option 1: Using Docker (Recommended)

```bash
# Start the Docker container
./scripts/server_control.sh start

# Access the web interface
open http://localhost:9001

# Run a benchmark via command line
docker-compose exec app bash
cd /app/Algorithms/BruteForce/C && ./runMe.sh
```

#### Option 2: Local Execution

```bash
# Install required tools (macOS)
brew install gnu-time coreutils

# Run a single language
cd Algorithms/BruteForce/C
./runMe.sh                                    # Run all matrices
./runMe.sh ../../Matrices/1.matrix            # Run specific matrix

# Run multiple languages
./runBenchmarks.sh C Python Go                # Specific languages
./runBenchmarks.sh --all                      # All languages
./runBenchmarks.sh --pending                  # Pending languages only

# Generate HTML report
cd Metrics
npx ts-node generate_report_only.ts
open ../index.html
```

### Starting the Web Server

```bash
# Local server (port 9002)
cd server
npm install
npm start

# Docker server (port 9001)
docker-compose up -d
```

## Architecture

### Directory Structure

```
SudokuSolver/
├── Algorithms/            # Algorithm implementations
│   ├── BruteForce/        # Brute-force language implementations
│   ├── DLX/               # Dancing Links implementations
│   ├── common.sh          # Shared benchmark functions
│   └── metadata.json      # Language metadata
│   ├── C/
│   │   ├── Sudoku.c       # Solver implementation
│   │   ├── runMe.sh       # Benchmark script
│   │   ├── metrics.json   # Generated results
│   │   └── README.md      # Language-specific notes
│   ├── Python/
│   ├── JavaScript/
│   └── [82+ more languages...]
│
├── Matrices/              # Test puzzles
│   ├── 1.matrix          # Easiest (656 iterations)
│   ├── 2.matrix          # Easy (439,269 iterations)
│   ├── 3.matrix          # Medium (98,847 iterations)
│   ├── 4.matrix          # Medium (9,085 iterations)
│   ├── 5.matrix          # Hard (445,778 iterations)
│   └── 6.matrix          # Very Hard
│
├── Metrics/               # Report generation
│   ├── HTMLGenerator.ts   # Main report generator
│   ├── gather_metrics.ts  # Metrics aggregation
│   ├── types.ts           # TypeScript interfaces
│   ├── LanguagesMetadata.ts  # Language personalities
│   └── generate_report_only.ts  # Standalone report generator
│
├── server/                # Web interface backend
│   ├── index.js          # Express server
│   ├── logo_processor.js # Logo handling
│   └── public/           # Frontend assets
│
├── scripts/               # Utility scripts
│   ├── server_control.sh # Docker management
│   └── ralph/            # Automated development tools
│
├── benchmark_config.json  # Language configuration
├── session_state.json     # UI state persistence
├── docker-compose.yml     # Docker configuration
└── index.html          # Generated benchmark report
```

### Data Flow

1. **Execution**: `runMe.sh` → Solver → Output
2. **Metrics Capture**: Python wrapper → `metrics.json`
3. **Aggregation**: `generate_report_only.ts` → Collect all `metrics.json` files
4. **Report Generation**: `HTMLGenerator.ts` → `index.html`
5. **Web Serving**: Express server → Browser

### Language Implementation Pattern

Every language follows this structure:

```bash
Algorithms/BruteForce/{Language}/
├── Sudoku.{ext}      # Solver (e.g., Sudoku.py, Sudoku.java)
├── runMe.sh          # Benchmark runner
├── metrics.json      # Results (generated)
└── README.md         # Notes (optional)
```

**runMe.sh Template:**

```bash
#!/bin/bash
cd "$(dirname "$0")"

# Configuration
LANGUAGE="LanguageName"
SOLVER_BINARY="./solver_executable"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../common.sh

# Define compile function (if needed)
compile() {
    check_toolchain compiler_name
    # Compilation commands here
}

# Run benchmark
main "$@"
```

## Usage

### Running Benchmarks

#### Single Language

```bash
cd Algorithms/BruteForce/Python
./runMe.sh                           # All matrices
./runMe.sh ../../Matrices/1.matrix   # Specific matrix
VARIANT=O3 ./runMe.sh                # With compiler variant
```

#### Multiple Languages

```bash
./runBenchmarks.sh C Python Go       # Specific languages
./runBenchmarks.sh --all             # All languages
./runBenchmarks.sh --pending         # Only pending languages
./runBenchmarks.sh --status          # Show benchmark status
```

#### Using Docker

```bash
# Interactive shell
docker-compose exec app bash
cd /app/Algorithms/BruteForce/Go && ./runMe.sh

# Single command
docker-compose exec app bash -c "cd /app/Algorithms/BruteForce/Rust && ./runMe.sh"
```

### Generating Reports

```bash
# Generate main report
cd Metrics
npx ts-node generate_report_only.ts

# View report
open ../index.html

# Generate and serve
./runBenchmarks.sh --report
```

### Web Interface

```bash
# Start server
cd server && npm start
# OR
docker-compose up -d

# Access at:
# Docker: http://localhost:9001
# Local:  http://localhost:9002
```

**Features:**
- Select language and matrix from dropdowns
- Click "Run" to execute benchmark
- View real-time output
- Generate updated reports
- Browse language source code
- Edit metadata and upload logos

### Validation

```bash
# Validate iteration counts
cd Metrics
node validate_run.js Algorithms/BruteForce/C/metrics.json

# Validate output format
node validate_run.js --format Languages/C/metrics.json

# Complete validation
node validate_run.js --all Languages/C/metrics.json
```

## Adding a New Language

### 1. Create Language Directory

```bash
mkdir Algorithms/BruteForce/NewLanguage
cd Algorithms/BruteForce/NewLanguage
```

### 2. Implement Solver

Create `Sudoku.{ext}` following the [Algorithm Specification](#algorithm-specification).

**Critical Requirements:**
- Row-major search order (row 0-8, col 0-8)
- Candidate values 1-9 in ascending order
- Increment iteration counter BEFORE validity check
- On EVERY attempt (even invalid ones)

### 3. Create runMe.sh

```bash
#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="NewLanguage"
SOLVER_BINARY="./Sudoku"  # or "python3 Sudoku.py" for interpreted
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../common.sh

compile() {
    check_toolchain compiler_name
    compiler -o Sudoku Sudoku.ext
}

main "$@"
```

### 4. Validate

```bash
chmod +x runMe.sh
./runMe.sh ../../Matrices/1.matrix
```

**Expected output:**
```
Solved in Iterations=656
```

If iterations don't match 656, your algorithm is incorrect.

### 5. Run Full Suite

```bash
./runMe.sh  # Runs all matrices
```

### 6. Update Configuration

Add to `benchmark_config.json`:

```json
{
  "languages": {
    "NewLanguage": {
      "status": "complete",
      "matrices": ["1.matrix", "2.matrix", "3.matrix", "4.matrix", "5.matrix"]
    }
  }
}
```

## Algorithm Specification

All implementations MUST match this exact specification:

### Search Strategy

1. **Find Empty Cell**: Row-major order (row 0→8, then col 0→8)
2. **Try Candidates**: Values 1→9 in ascending order
3. **Validate**: Check row, column, and 3×3 box constraints
4. **Backtrack**: If no valid candidates, return false
5. **Recurse**: If placement valid, continue to next cell

### Iteration Counting

```
iterations = 0

function solve(board):
    iterations++  # ← BEFORE validity check, on EVERY attempt

    cell = find_next_empty(board)
    if no empty cell:
        return true  # Solved

    for value in [1, 2, 3, 4, 5, 6, 7, 8, 9]:
        if is_valid(board, cell, value):
            board[cell] = value
            if solve(board):
                return true
            board[cell] = 0  # Backtrack

    return false
```

### Reference Iteration Counts

| Matrix | Iterations | Difficulty |
|--------|-----------|------------|
| 1      | 656       | Easiest    |
| 2      | 439,269   | Easy       |
| 3      | 98,847    | Medium     |
| 4      | 9,085     | Medium     |
| 5      | 445,778   | Hard       |
| 6      | Variable  | Very Hard  |

**If your iteration count doesn't match, your algorithm is incorrect.**

### Output Format

```
../Matrices/N.matrix
<9 rows of input puzzle>

Puzzle:
<9 rows formatted with spaces>

Puzzle:
<9 rows of solved puzzle>

Solved in Iterations=NNN

Seconds to process X.XXX
```

## Performance Metrics

### 🏆 Scoring System (Composite Score)

The project uses a **Weighted Geometric Mean** to calculate a single "Composite Score" (Ψ) for each language. This method is an industry standard (used by SPEC and Geekbench) as it prevents outliers from dominating the results and rewards balanced performance.

#### The Formula
Score = (Time_Ratio ^ 0.8) * (Memory_Ratio ^ 0.2)

Where:
- **Time_Ratio**: The ratio of the language's execution time to the C baseline (Timeₗₐₙg / Time꜀).
- **Memory_Ratio**: The ratio of the language's peak memory to the C baseline (Memₗₐₙg / Mem꜀).
- **Weights**: 80% Time, 20% Memory.

#### Default Weights
By default, the system emphasizes execution time over memory usage:
- **Time Weight (wₜ)**: 0.8 (80%)
- **Memory Weight (wₘ)**: 0.2 (20%)

#### Admin UI & Configurability
Scoring weights are **persistent and configurable**. Developers can adjust these weights in real-time through the **Methodology Modal** in the web interface. Changes are saved to `benchmark_config.json` and trigger an automated report regeneration to reflect the new rankings.

#### Interpretation
- **Ψ = 1.0**: Parity with C (the baseline).
- **Ψ < 1.0**: Outperforms C (lower is better).
- **Ψ > 1.0**: Underperforms C.

### 📊 What We Measure

Each benchmark run captures:

- **Execution Time**: Wall clock execution time (milliseconds).
- **Iteration Count**: Algorithm fingerprint (must match reference).
- **Memory**: Peak resident set size (bytes).
- **CPU User Time**: Time spent in user mode (milliseconds).
- **CPU System Time**: Time spent in kernel mode (milliseconds).
- **Page Faults Major**: Faults requiring disk I/O.
- **Page Faults Minor**: Faults satisfied from cache.
- **Context Switches Voluntary**: Process yielded CPU.
- **Context Switches Involuntary**: Process preempted.
- **I/O Operations**: Input and output operation counts.

### 🛠️ Compiler Variants

Test different optimization levels:

```bash
VARIANT=O0 ./runMe.sh      # No optimization
VARIANT=O2 ./runMe.sh      # Moderate optimization
VARIANT=O3 ./runMe.sh      # Aggressive optimization
VARIANT=Ofast ./runMe.sh   # Fastest (may break standards)
```

### Typical Performance (Matrix 5)

| Language | Time (ms) | Relative to C |
|----------|-----------|---------------|
| C (O3)   | ~50       | 1.0x          |
| Rust     | ~55       | 1.1x          |
| C++      | ~60       | 1.2x          |
| Go       | ~120      | 2.4x          |
| Java     | ~150      | 3.0x          |
| Python   | ~15,000   | 300x          |

## Contributing

### Ways to Contribute

1. **Add New Languages**: Implement the solver in a missing language
2. **Improve Documentation**: Clarify instructions, add examples
3. **Enhance Web UI**: Improve the frontend experience
4. **Add Visualizations**: Create new charts and comparisons
5. **Fix Bugs**: Report and fix issues
6. **Optimize Infrastructure**: Improve build times, Docker configuration

### Contribution Guidelines

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-language`)
3. Follow the [Algorithm Specification](#algorithm-specification)
4. Validate iteration counts match reference
5. Test your changes locally
6. Commit with descriptive messages
7. Push to your fork
8. Open a Pull Request

### Code Style

- **Shell Scripts**: Follow existing `runMe.sh` patterns
- **Solver Code**: Match language idioms while preserving algorithm
- **Documentation**: Clear, concise, with examples
- **Commits**: Descriptive messages explaining the "why"

## Technical Details

### Dependencies

**Server:**
- Node.js 18+
- Express, CORS, Multer
- Sharp (image processing)

**Metrics:**
- TypeScript 5+
- ts-node
- glob (file matching)

**System Tools (macOS):**
```bash
brew install gnu-time coreutils
```

**System Tools (Linux):**
```bash
# Usually pre-installed
/usr/bin/time
timeout
```

### Special Character Handling

Languages with special characters use underscores in directory names:

- `C#` → `Algorithms/BruteForce/C_Sharp/` (LANGUAGE="C_Sharp")
- `F#` → `Algorithms/BruteForce/F_Sharp/` (LANGUAGE="F_Sharp")
- `C++` → `Algorithms/BruteForce/C++/` (keeps special chars)

The `LANGUAGE` variable in `runMe.sh` must match the directory name exactly.

### Docker Details

**Image**: `sudoku-benchmark`
- Based on Ubuntu with all language toolchains
- Mounts project directories as volumes
- Runs server on port 9001
- Persistent storage for results

**Volume Mounts:**
- `./Languages:/app/Languages`
- `./Matrices:/app/Matrices`
- `./Metrics:/app/Metrics`
- `./server:/app/server`

## License

This project is open source. See individual language implementations for their specific licenses.

## Acknowledgments

- Inspired by the classic "Computer Language Benchmarks Game"
- Built with contributions from the programming language community
- Powered by the diverse ecosystem of programming languages

---

**Project Statistics:**
- 85+ Programming Languages
- 6 Test Matrices
- 500,000+ Total Benchmark Iterations
- Consistent Algorithm Across All Languages
- 100% Reproducible Results

**Have questions or suggestions?** Open an issue or contribute on GitHub!
