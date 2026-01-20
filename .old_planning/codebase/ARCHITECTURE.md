# Architecture

## High-Level Design
The system is a **Polyglot Benchmarking Harness**. It executes identical algorithms across different runtime environments to measure raw performance overhead.

### Data Flow
1. **Input:** `runMeGlobal.sh` accepts Language and Matrix arguments.
2. **Execution:** Invokes `Languages/[Lang]/runMe.sh`.
3. **Algorithm:** The solver reads `Matrices/[N].matrix`, solves via **Recursive Backtracking**, and prints results to `stdout`.
4. **Capture:** `Languages/common.sh` wraps execution, capturing start/end times and valid output.
5. **Persistence:** Metrics are written to `Languages/[Lang]/metrics.json`.
6. **Aggregation:** `Metrics/` scripts aggregate JSON files into a consolidated report.

## Design Patterns
- **Template Method:** All languages implement the exact same logic; `common.sh` provides the "template" for execution.
- **Strategy Pattern:** Each `Language/` directory is a strategy for solving the problem.
- **Pipes and Filters:** Unix-style composition of input files, execution, and output processing.

## Key Constraints
- **Algorithmic Purity:** Implementations must use brute-force backtracking. No heuristics (DLX/Algorithm X) allowed in the main benchmark.
- **IO Isolation:** Timing excludes compilation and interpreter startup where possible (handled by `common.sh`).
