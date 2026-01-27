# Adding a New Language to the Sudoku Benchmark Suite

This guide explains how to add a new language implementation to the benchmark suite, how the test system works, and what metrics are captured.

## Table of Contents

1. [Overview](#overview)
2. [Directory Structure](#directory-structure)
3. [The Algorithm Specification](#the-algorithm-specification)
4. [Creating a New Language Implementation](#creating-a-new-language-implementation)
5. [The runMe.sh Script](#the-runmesh-script)
6. [How the Test Suite Works](#how-the-test-suite-works)
7. [Metrics Capture](#metrics-capture)
8. [Validation](#validation)
9. [Troubleshooting](#troubleshooting)

---

## Overview

The Sudoku Benchmark Suite compares performance across 88+ programming languages using an **identical brute-force backtracking algorithm**. The iteration count serves as an "algorithm fingerprint" - if your implementation produces the same iteration count as the C reference, your algorithm is correct.

### Key Principles

- **Algorithm purity**: All implementations must follow the exact same algorithm
- **Fair comparison**: No language-specific optimizations allowed
- **Automated validation**: Iteration counts are verified against reference values
- **Comprehensive metrics**: Time, memory, CPU usage, and more are captured

---

## Directory Structure

Each language implementation lives in its own directory:

```
Algorithms/
├── BruteForce/           # Standard backtracking algorithm
│   ├── C/                # Reference implementation
│   │   ├── Sudoku.c      # Solver source code
│   │   ├── runMe.sh      # Benchmark runner script
│   │   └── metrics.json  # Generated benchmark results
│   ├── Python/
│   │   ├── Sudoku.py
│   │   ├── runMe.sh
│   │   └── metrics.json
│   └── {YourLanguage}/
│       ├── Sudoku.{ext}
│       ├── runMe.sh
│       └── metrics.json
├── DLX/                  # Dancing Links algorithm
└── common.sh             # Shared benchmark functions
```

### Special Characters in Directory Names

Languages with special characters use underscores:

| Language | Directory Name | LANGUAGE variable |
|----------|---------------|-------------------|
| C#       | `C_Sharp/`    | `C_Sharp`         |
| F#       | `F_Sharp/`    | `F_Sharp`         |
| C++      | `C++/`        | `C++`             |

**Important**: The `LANGUAGE` variable in `runMe.sh` must match the directory name exactly.

---

## The Algorithm Specification

Your implementation **must exactly match** this algorithm to produce correct iteration counts:

### Brute-Force Backtracking Rules

1. **Search order**: Row-major (row 0→8, then column 0→8)
2. **Candidate order**: Try values 1 through 9 in ascending order
3. **Iteration counting**: Increment the counter **BEFORE** checking validity, on **EVERY** attempt
4. **No optimizations**: No MRV (Minimum Remaining Values), no constraint propagation

### Reference Iteration Counts

| Matrix | Iterations   | Difficulty |
|--------|-------------|------------|
| 1      | 656         | Easy       |
| 2      | 439,269     | Medium     |
| 3      | 98,847      | Medium     |
| 4      | 9,085       | Easy       |
| 5      | 445,778     | Medium     |
| 6      | 622,577,597 | Hard       |

**If your iteration counts don't match, your algorithm is incorrect.**

### Pseudocode

```
function solve():
    # Find first empty cell (row-major order)
    for row = 0 to 8:
        for col = 0 to 8:
            if puzzle[row][col] == 0:
                goto try_values

    # No empty cell = solved
    return SUCCESS

try_values:
    for val = 1 to 9:
        iterations++           # COUNT BEFORE validity check
        if isValid(row, col, val):
            puzzle[row][col] = val
            if solve() == SUCCESS:
                return SUCCESS
            puzzle[row][col] = 0   # Backtrack

    return FAILURE
```

### Output Format

Your solver must produce output in this exact format:

```
{matrix_path}
{9 lines of puzzle digits, space-separated}

Puzzle:
{initial puzzle - 9 lines}

Puzzle:
{solved puzzle - 9 lines}

Solved in Iterations={count}

Seconds to process {time}
```

---

## Creating a New Language Implementation

### Step 1: Create the Directory

```bash
mkdir Algorithms/BruteForce/YourLanguage
cd Algorithms/BruteForce/YourLanguage
```

### Step 2: Implement the Solver

Create `Sudoku.{ext}` following the algorithm specification. Use the C implementation as your reference:

```bash
# View reference implementation
cat ../C/Sudoku.c
```

Key implementation points:

1. **Read matrix file** from command-line argument
2. **Parse 9x9 grid** (space-separated integers, 0 = empty)
3. **Implement `isValid(row, col, val)`** checking row, column, and 3x3 box
4. **Implement `solve()`** with exact iteration counting
5. **Output in required format**

### Step 3: Create runMe.sh

Create the benchmark runner script:

```bash
#!/bin/bash
# Algorithms/BruteForce/YourLanguage/runMe.sh

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="YourLanguage"              # Must match directory name
SOLVER_BINARY="./Sudoku"             # Or "interpreter Sudoku.ext"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes default

# Source shared functions
source ../../common.sh

# ============================================================================
# COMPILATION (if needed)
# ============================================================================
compile() {
    # Check toolchain availability
    check_toolchain your_compiler

    # Check source file exists
    if [ ! -f "Sudoku.ext" ]; then
        report_env_error "Sudoku.ext not found"
    fi

    # Compile (for compiled languages)
    echo "Compiling YourLanguage solver..."
    your_compiler -o Sudoku Sudoku.ext

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
```

### Step 4: Validate Your Implementation

```bash
# Run on the simplest matrix first
./runMe.sh ../../../Matrices/1.matrix

# Expected output should show: Solved in Iterations=656
```

### Step 5: Run Full Benchmark

```bash
# Run all matrices
./runMe.sh

# Results written to metrics.json
```

---

## The runMe.sh Script

### For Compiled Languages

```bash
#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Rust"
SOLVER_BINARY="./sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain rustc

    if [ ! -f "Sudoku.rs" ]; then
        report_env_error "Sudoku.rs not found"
    fi

    echo "Compiling Rust solver..."
    rustc -O -o sudoku Sudoku.rs

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

main "$@"
```

### For Interpreted Languages

```bash
#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Ruby"
SOLVER_BINARY="ruby Sudoku.rb"      # Note: interpreter + script
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain ruby

    if [ ! -f "Sudoku.rb" ]; then
        report_env_error "Sudoku.rb not found"
    fi

    # Syntax check
    ruby -c Sudoku.rb > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "Sudoku.rb has syntax errors"
    fi

    echo "Ruby solver ready"
}

main "$@"
```

### For JVM Languages

```bash
#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Java"
SOLVER_BINARY="java Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain javac
    check_toolchain java

    if [ ! -f "Sudoku.java" ]; then
        report_env_error "Sudoku.java not found"
    fi

    echo "Compiling Java solver..."
    javac Sudoku.java

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

main "$@"
```

---

## How the Test Suite Works

### Execution Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                     runBenchmarks.sh                            │
│  (Master orchestrator - runs languages in parallel)             │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                  Language runMe.sh                              │
│  1. Sources common.sh                                           │
│  2. Calls compile() if needed                                   │
│  3. Calls main() which runs run_benchmarks()                    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     common.sh                                   │
│  run_benchmarks():                                              │
│    - Iterates over matrix files                                 │
│    - Calls run_matrix() for each                                │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   run_matrix()                                  │
│  1. Executes solver via Python wrapper (for precise timing)     │
│  2. Captures: time, memory, CPU, page faults, I/O, etc.         │
│  3. Validates solution correctness                              │
│  4. Validates iteration count against reference                 │
│  5. Outputs JSON result                                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    metrics.json                                 │
│  Results merged into per-language metrics file                  │
└─────────────────────────────────────────────────────────────────┘
```

### Running Benchmarks

```bash
# Run a single language
./runBenchmarks.sh Python

# Run all pending (incomplete) languages
./runBenchmarks.sh --pending

# Run all languages
./runBenchmarks.sh --all

# Show status
./runBenchmarks.sh --status

# Generate HTML report
./runBenchmarks.sh --report

# Filter by algorithm type
ALGO_FILTER=BF ./runBenchmarks.sh --all      # BruteForce only
ALGO_FILTER=DLX ./runBenchmarks.sh --all     # Dancing Links only
```

---

## Metrics Capture

### What Gets Measured

Each benchmark run captures comprehensive metrics via `resource.getrusage()`:

| Metric | Description | Unit |
|--------|-------------|------|
| `time` | Wall-clock execution time | milliseconds |
| `iterations` | Algorithm iteration count | count |
| `memory` | Peak memory usage (max RSS) | bytes |
| `cpu_user` | User CPU time | milliseconds |
| `cpu_sys` | System CPU time | milliseconds |
| `page_faults_major` | Major page faults (disk I/O) | count |
| `page_faults_minor` | Minor page faults (memory) | count |
| `context_switches_voluntary` | Voluntary context switches | count |
| `context_switches_involuntary` | Involuntary context switches | count |
| `io_inputs` | Block input operations | count |
| `io_outputs` | Block output operations | count |
| `status` | `success`, `timeout`, or `error` | string |

### metrics.json Format

```json
[
  {
    "solver": "Python",
    "runType": "automated",
    "timestamp": "2024-01-15T10:30:00Z",
    "results": [
      {
        "matrix": "1",
        "time": 45.5,
        "iterations": 656,
        "memory": 10485760,
        "cpu_user": 42.3,
        "cpu_sys": 2.1,
        "page_faults_major": 0,
        "page_faults_minor": 2500,
        "context_switches_voluntary": 5,
        "context_switches_involuntary": 12,
        "io_inputs": 0,
        "io_outputs": 0,
        "status": "success",
        "output": "..."
      },
      // ... more matrices
    ]
  }
]
```

### How Metrics Are Captured

The `common.sh` script uses a Python wrapper to ensure accurate, cross-platform metrics:

```python
# Simplified version of the metrics capture
import subprocess, time, resource, platform

start = time.perf_counter()
process = subprocess.run(cmd, ...)
end = time.perf_counter()

duration_ms = (end - start) * 1000
usage = resource.getrusage(resource.RUSAGE_CHILDREN)

# Normalize max RSS (macOS reports bytes, Linux reports KB)
max_rss = usage.ru_maxrss
if platform.system() == 'Darwin':
    max_rss = max_rss / 1024  # Convert to KB
```

---

## Validation

### Automatic Validation

Every benchmark run validates:

1. **Solution correctness**: The solved puzzle satisfies all Sudoku constraints
2. **Iteration count**: Matches the reference value for that matrix

### Validation Failures

Failures are logged to `benchmark_issues.json`:

```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "language": "MyLanguage",
  "algorithm": "BruteForce",
  "matrix": "1",
  "failure_type": "iteration_mismatch",
  "severity": "CRITICAL",
  "message": "Expected 656 iterations, got 657",
  "expected_iterations": 656,
  "actual_iterations": 657
}
```

### Common Iteration Count Issues

| Problem | Cause | Fix |
|---------|-------|-----|
| Count too high | Counting after validity check | Move `count++` before `isValid()` |
| Count too low | Not counting failed attempts | Count every attempt, not just valid ones |
| Count varies | Wrong search order | Use row-major: row 0→8, then col 0→8 |
| Off by small amount | Boundary conditions | Check loop ranges carefully |

---

## Troubleshooting

### "Compilation failed"

- Check that the compiler/interpreter is installed
- Verify the toolchain name in `check_toolchain`
- Check for syntax errors in your source file

### "Iteration count mismatch"

- Review the algorithm specification
- Count iterations BEFORE validity check
- Ensure row-major search order (row then column)
- Ensure candidate order 1→9

### "Solution invalid"

- Check your `isValid()` function
- Verify row, column, and 3x3 box constraints
- Ensure backtracking properly resets cells

### "Timeout"

- Matrix 6 can take 10+ minutes for slow languages
- Increase `TIMEOUT_SECONDS` if needed
- Check for infinite loops in your solver

### Running in Docker

The Docker container has all 88+ language toolchains pre-installed:

```bash
# Start container
docker-compose up -d

# Run benchmarks inside
docker-compose exec app bash
cd /app/Algorithms/BruteForce/YourLanguage
./runMe.sh
```

---

## Checklist for New Languages

- [ ] Created `Algorithms/BruteForce/{Language}/` directory
- [ ] Implemented `Sudoku.{ext}` following algorithm spec
- [ ] Created `runMe.sh` with correct `LANGUAGE` variable
- [ ] Verified iteration count matches reference (656 for matrix 1)
- [ ] Ran full benchmark suite (`./runMe.sh`)
- [ ] Checked `metrics.json` was generated
- [ ] No entries in `benchmark_issues.json`

---

## Getting Help

- Check existing implementations for reference patterns
- The C implementation (`Algorithms/BruteForce/C/Sudoku.c`) is the canonical reference
- For interpreted languages, check Python's implementation
- For JVM languages, check Java's implementation
