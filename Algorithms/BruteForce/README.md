# Language Implementation Pattern

This directory contains implementations of the brute-force Sudoku solver across multiple programming languages. Each language follows a modular script architecture to reduce duplication and ensure consistency.

## Modular Architecture

### common.sh - Shared Function Library

The `Algorithms/common.sh` file provides reusable functions for:
- **Environment detection** - OS detection, time command selection
- **Error handling** - `report_env_error()` for environment failures
- **Metrics capture** - Timing, memory, CPU measurement
- **Output parsing** - `extract_iterations()` from solver output
- **JSON generation** - Standard metrics.json format
- **Matrix execution** - `run_matrix()` with timeout handling
- **Main orchestration** - `run_benchmarks()` wrapper

### Language-Specific Scripts

Each language has its own `runMe.sh` (or `setupAndRunMe.sh`) that:
1. Sets language-specific configuration variables
2. Sources `common.sh` to load shared functions
3. Defines compilation steps (if needed)
4. Calls `run_benchmarks()` to execute

## Creating a New Language Implementation

### Step 1: Configure Variables

```bash
#!/bin/bash
# Algorithms/BruteForce/YourLanguage/runMe.sh

cd "$(dirname "$0")"

# Set language-specific configuration
LANGUAGE="YourLanguage"
SOLVER_BINARY="./Sudoku"  # Or ./solver, ./sudoku.exe, etc.
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions
source ../../common.sh
```

### Step 2: Define Compilation (if needed)

For compiled languages, override the `compile()` function:

```bash
compile() {
    # Check compiler is available
    check_toolchain gcc  # or javac, rustc, etc.

    # Check source file exists
    if [ ! -f "Sudoku.c" ]; then
        report_env_error "Sudoku.c not found"
    fi

    # Compile
    echo "Compiling C solver..."
    gcc -O3 -o Sudoku Sudoku.c

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful"
}
```

For interpreted languages, no compilation needed - just check interpreter:

```bash
# Check Python is available
check_toolchain python3

# Set SOLVER_BINARY to interpreter + script
SOLVER_BINARY="python3 sudoku.py"
```

### Step 3: Optional - Compiler Variants

For languages with multiple compilation options:

```bash
get_compile_flags() {
    local variant="${1:-default}"
    case "$variant" in
        O0) echo "-O0" ;;
        O2) echo "-O2" ;;
        O3|default) echo "-O3" ;;
        Ofast) echo "-Ofast" ;;
        *) echo "-O3" ;;
    esac
}

compile() {
    local flags=$(get_compile_flags "${VARIANT:-default}")
    gcc $flags -o Sudoku Sudoku.c || report_env_error "Compilation failed"
}
```

### Step 4: Call Main

```bash
# Execute benchmarks
main "$@"
```

## Complete Example: C Implementation

```bash
#!/bin/bash
# Algorithms/BruteForce/C/runMe.sh

cd "$(dirname "$0")"

# Language configuration
LANGUAGE="C"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../../common.sh

# Compilation
compile() {
    check_toolchain gcc

    if [ ! -f "Sudoku.c" ]; then
        report_env_error "Sudoku.c not found"
    fi

    echo "Compiling C solver..."
    gcc -O3 -o Sudoku Sudoku.c

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    echo "Compilation successful"
}

# Run
main "$@"
```

## Metrics Output Format

All language scripts generate `metrics.json` in this format:

```json
[
  {
    "solver": "C",
    "runType": "automated",
    "timestamp": "2025-12-16T20:00:00Z",
    "results": [
      {
        "matrix": "1",
        "time": 0.001234,
        "iterations": 656,
        "memory": 1024,
        "cpu_user": 0.001,
        "cpu_sys": 0.0002,
        "status": "success",
        "output": "../Matrices/1.matrix\n\nPuzzle:\n..."
      }
    ]
  }
]
```

### Status Values

- `success` - Matrix solved within timeout
- `timeout` - Execution exceeded 5 minutes
- `error` - Runtime error or crash
- `env_error` - Missing compiler/dependencies

## Error Handling

Use `report_env_error()` for environment issues:

```bash
# Check compiler exists
if ! command -v gcc &> /dev/null; then
    report_env_error "gcc compiler not found"
fi

# Check source file exists
if [ ! -f "Sudoku.c" ]; then
    report_env_error "Sudoku.c not found"
fi

# Check compilation succeeded
gcc -O3 -o Sudoku Sudoku.c || report_env_error "Compilation failed"
```

This generates a proper error metrics.json and exits gracefully.

## Running Benchmarks

### All Matrices (default)

```bash
./runMe.sh
```

Runs all matrices from `../../../Matrices/*.matrix`

### Specific Matrices

```bash
./runMe.sh ../../../Matrices/1.matrix ../../../Matrices/2.matrix
```

### With Variant (if supported)

```bash
VARIANT=Ofast ./runMe.sh
```

## Output

The script generates:
- **metrics.json** - Machine-readable benchmark results
- **Console output** - Progress messages to stderr
- **Exit code** - 0 on success, 1 on error

## Best Practices

1. **Always source common.sh** - Don't duplicate timing/metrics logic
2. **Use check_toolchain()** - Validate dependencies before running
3. **Call report_env_error()** - For environment failures (not runtime errors)
4. **Test with Matrix 1 first** - Smallest matrix (656 iterations), fast validation
5. **Document variants** - If your language supports optimization flags
6. **Match C output exactly** - See Algorithms/BruteForce/C/ALGORITHM.md for format requirements

## Validation

After implementing a language, validate with:

```bash
# Run benchmarks
cd Algorithms/BruteForce/YourLanguage
./runMe.sh ../../../Matrices/1.matrix

# Check metrics generated
cat metrics.json

# Validate against reference
node /app/Metrics/validate_run.js /app/Algorithms/BruteForce/YourLanguage/metrics.json
```

Expected: Iteration count matches C reference (656 for Matrix 1), output format matches exactly.

## Troubleshooting

### "common.sh: No such file or directory"

Make sure `source ../../common.sh` uses correct relative path from language directory.

### "TIME_CMD: command not found"

The `detect_time_cmd()` function is called by `run_benchmarks()`. Don't call time commands directly.

### "Metrics file has wrong format"

Check that you're calling `run_benchmarks()`, not implementing metrics generation manually.

### "Iteration count doesn't match"

Your algorithm differs from C reference. See Algorithms/BruteForce/C/ALGORITHM.md for exact algorithm requirements.

## Reference Implementation

See `Algorithms/BruteForce/C/runMe.sh` for the authoritative reference implementation that all other languages should follow.
