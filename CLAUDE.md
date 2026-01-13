# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Multi-language Sudoku solver benchmark suite with 88+ language implementations. All implementations use an identical brute-force backtracking algorithm to enable fair cross-language performance comparison. The iteration count serves as the algorithm's fingerprint - if iterations match the C reference, the algorithm is correct.

## Common Commands

### Run Benchmarks
```bash
# Single language
./runBenchmarks.sh C
./runBenchmarks.sh Python

# All pending languages
./runBenchmarks.sh --pending

# All languages
./runBenchmarks.sh --all

# Show status
./runBenchmarks.sh --status

# Generate HTML report
./runBenchmarks.sh --report
```

### Run a Single Language Directly
```bash
cd Algorithms/BruteForce/C
./runMe.sh                                    # Run all matrices
./runMe.sh ../../Matrices/1.matrix            # Run specific matrix
VARIANT=Ofast ./runMe.sh                      # Run with compiler variant
```

### Docker (Recommended)
The `sudoku-benchmark` Docker image contains all 88+ language toolchains pre-installed.

```bash
# Build and start container
docker-compose up -d

# Or use the control script
./scripts/server_control.sh start             # Start container
./scripts/server_control.sh stop              # Stop container
./scripts/server_control.sh logs              # View logs
./scripts/server_control.sh status            # Check status

# Run benchmarks inside container
docker-compose exec app bash
cd /app/Algorithms/BruteForce/C && ./runMe.sh
```

The container mounts the project directories, so benchmark results persist to host.

### Start Server Locally (without Docker)
```bash
cd server && npm install && npm start         # Serves report at localhost:9001
```

### Generate Report Only
```bash
cd Metrics && npx ts-node generate_report_only.ts
```

## Architecture

### Directory Structure
- `Algorithms/BruteForce/` - 88+ brute-force language implementations, each with `Sudoku.*` solver and `runMe.sh`
- `Algorithms/common.sh` - Shared benchmark functions sourced by all `runMe.sh` scripts
- `Algorithms/DLX/` - Dancing Links algorithm implementations
- `Matrices/` - Test puzzles (1-6.matrix), 1.matrix is simplest (656 iterations)
- `Metrics/` - TypeScript report generation (`HTMLGenerator.ts`, `gather_metrics.ts`)
- `server/` - Express server serving benchmark report and runner UI

### Special Character Handling in Directory Names
Languages with special characters in their names use underscores in directory names:
- `C#` → `Algorithms/BruteForce/C_Sharp/` (LANGUAGE="C_Sharp" in runMe.sh)
- `F#` → `Algorithms/BruteForce/F_Sharp/` (LANGUAGE="F_Sharp" in runMe.sh)
- `C++` → `Algorithms/BruteForce/C++/` (this one keeps the special chars)

**Important**: The `LANGUAGE` variable in `runMe.sh` must match the directory name exactly for metrics to be properly associated.

### Language Implementation Pattern
Each language follows this structure:
```
Algorithms/BruteForce/{Language}/
├── Sudoku.{ext}      # Solver implementation
├── runMe.sh          # Benchmark runner (sources common.sh)
├── metrics.json      # Generated benchmark results
└── README.md         # Language-specific notes
```

The `runMe.sh` pattern:
1. Set `LANGUAGE`, `SOLVER_BINARY`, `METRICS_FILE`, `TIMEOUT_SECONDS`
2. Source `../../common.sh`
3. Define `compile()` function if needed
4. Call `main "$@"`

### Metrics Pipeline
1. `runMe.sh` executes solver, captures timing/memory via Python subprocess wrapper in `common.sh`
2. Results written to `Algorithms/BruteForce/{Language}/metrics.json` (or `Algorithms/DLX/{Language}/metrics.json` for DLX implementations)
3. `generate_report_only.ts` aggregates all `metrics.json` files
4. `HTMLGenerator.ts` produces `_report.html` with interactive visualizations

### Key TypeScript Files
- `Metrics/types.ts` - `SolverMetrics` and `MetricResult` interfaces
- `Metrics/HTMLGenerator.ts` - Main report generation
- `Metrics/LanguagesMetadata.ts` - Language personalities, quotes, compiler info
- `Metrics/HistoryManager.ts` - Benchmark history tracking

## Algorithm Specification

All implementations must exactly match the C reference algorithm:

### Requirements
- **Search order**: Row-major (row 0-8, then col 0-8)
- **Candidate order**: 1 through 9 in ascending order
- **Iteration counting**: Increment BEFORE validity check, on EVERY attempt
- **No optimizations**: No MRV, no constraint propagation

### Reference Iteration Counts
| Matrix | Iterations |
|--------|-----------|
| 1      | 656       |
| 2      | 439,269   |
| 3      | 98,847    |
| 4      | 9,085     |
| 5      | 445,778   |

If iteration counts don't match, the algorithm is incorrect.

### Output Format
```
../Matrices/N.matrix
<9 rows of puzzle digits, space-separated>

Puzzle:
<initial puzzle>

Puzzle:
<solved puzzle>

Solved in Iterations=NNN

Seconds to process X.XXX
```

## Adding a New Language

1. Create `Algorithms/BruteForce/{Language}/Sudoku.{ext}` following the algorithm spec
2. Create `Algorithms/BruteForce/{Language}/runMe.sh`:
```bash
#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="NewLang"
SOLVER_BINARY="./Sudoku"  # or "python3 Sudoku.py" for interpreted
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain compiler_name
    # compilation commands
}

main "$@"
```
3. Validate: `./runMe.sh ../../Matrices/1.matrix` should show 656 iterations
4. Run full suite: `./runMe.sh`

## Configuration

### benchmark_config.json
Controls which languages/matrices to run:
- `completed[]` - Languages with completed benchmarks
- `languages.{name}.matrices[]` - Which matrices to run per language
- `languages.{name}.status` - "complete" or pending

### Environment
- **Docker (recommended)**: All toolchains pre-installed in `sudoku-benchmark` image
- **Local macOS**: Requires `brew install gnu-time coreutils` for `gtime` and `gtimeout`
- **Local**: Each language needs its compiler/interpreter in PATH

### Docker Server Notes
- The Docker container mounts `./server:/app/server` so server code changes are picked up on restart
- **NEVER restart the Docker container unnecessarily** - if server API changes are needed, the container will pick them up automatically via the volume mount
- If changes to `server/index.js` aren't reflected, verify the volume mount is working: `docker-compose exec app cat /app/server/index.js | head -20`
- Local server runs on port 9002, Docker server runs on port 9001
