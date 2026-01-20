# Codebase Structure

## Directory Layout

### `/Languages`
The core of the repository.
- `common.sh`: The shared "kernel" for benchmarking.
- `metadata.json`: Registry of all supported languages.
- `[Language]/`: Per-language implementation.
  - `runMe.sh`: Entry point script.
  - `Sudoku.[ext]`: Source code.
  - `metrics.json`: Performance data.

### `/Metrics`
TypeScript/Node.js tooling for data analysis.
- `HTMLGenerator.ts`: Compiles results into `benchmark_report.html`.
- `server/`: Express/Node server for UI.

### `/Matrices`
Input datasets.
- `*.matrix`: 9x9 Sudoku puzzles (0 = empty).

### Root Scripts
- `runMeGlobal.sh`: Main CLI entry point.
- `runAllBenchmarks.sh`: Bulk execution wrapper.

### Project Management
- `.planning/`: GSD workflow files (`PROJECT.md`, `ROADMAP.md`).
- `.gemini/`: Agent memory and tools.
