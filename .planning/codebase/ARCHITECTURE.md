# Architecture

## High-Level Flow
1. **Execution**: `runMeGlobal.sh` or `runBenchmarks.sh` orchestrates solver runs.
2. **Timing**: Solvers are timed using `/usr/bin/time` or internal logic via `common.sh`.
3. **Data Collection**: `gather_metrics.ts` scans language directories for `metrics.json`.
4. **Synthesis**: `HTMLGenerator.ts` compiles metrics, metadata, and history into `_report.html`.
5. **Presentation**: A single-file HTML dashboard with embedded CSS/JS.

## Component Boundaries
- **Solvers**: Isolated processes that read `.matrix` files and output standard format strings.
- **Orchestrator**: Bash scripts managing the lifecycle of solver execution.
- **Metrics Engine**: Decoupled from solvers, processes JSON artifacts.
- **Content Server**: Optional local service for logo management and UI interactions.

## Data Flow
- `Matrices/*.matrix` -> `Solver (Process)` -> `metrics.json`
- `metrics.json` + `Algorithms/metadata.json` + `benchmark_history.db` -> `HTMLGenerator` -> `_report.html`
