# Architecture

**Analysis Date:** 2025-12-24

## Pattern Overview

**Overall:** Monolithic Layered Application with Polyglot Solver System

**Key Characteristics:**
- Multi-language Sudoku solver benchmark system
- REST API serving matrix runner UI and reports
- TypeScript-based metrics processing and visualization
- 80+ language solver implementations with standardized interface
- File-based state persistence (JSON, SQLite)

## Layers

**API Layer:**
- Purpose: HTTP endpoints for benchmark execution and UI serving
- Contains: Express routes, file upload handlers, process execution
- Location: `server/index.js`
- Depends on: File system, child_process, Metrics module
- Used by: Web clients, CLI tools

**Service Layer:**
- Purpose: Business logic for metrics, reports, and history
- Contains: HTML generation, language metadata, solver execution
- Location: `Metrics/*.ts`
- Depends on: File system, Puppeteer, SQLite
- Used by: API layer, CLI scripts

**Solver Layer:**
- Purpose: Language-specific Sudoku solver implementations
- Contains: 80+ language folders with solver code and scripts
- Location: `Languages/*/`
- Depends on: Language-specific compilers/interpreters
- Used by: Service layer via shell execution

**Utility Layer:**
- Purpose: Shared bash functions for all solvers
- Contains: Timing, error handling, output parsing
- Location: `Languages/common.sh`
- Depends on: System tools (gtime, awk, grep)
- Used by: All solver scripts

## Data Flow

**Benchmark Execution Flow:**

1. Client sends `POST /api/run` with language + matrix
2. Server (`server/index.js`) constructs shell command
3. Shell executes `Languages/[Lang]/runMe.sh ../../../Matrices/N.matrix`
4. Language script sources `Languages/common.sh`, compiles if needed
5. Solver runs, outputs `Solved in Iterations=NNN`
6. `common.sh` captures: time, memory, iterations, CPU
7. Results written to `Languages/[Lang]/metrics.json`
8. Server returns execution status + stdout/stderr
9. Optional: `POST /api/generate-report` triggers full report
10. `generate_report_only.ts` aggregates all language metrics
11. `HTMLGenerator.ts` creates interactive report with D3.js
12. Output saved to `_report.html`, served at `GET /`

**State Management:**
- File-based: All state in JSON files and SQLite
- No persistent in-memory state
- Each benchmark execution is independent

## Key Abstractions

**Type Definitions** (`Metrics/types.ts`):
- `SolverMetrics`: Wrapper for execution results with solver name and timestamp
- `MetricResult`: Individual benchmark result (time, memory, iterations, status)

**Core Modules:**
- `HTMLGenerator.ts` (2001 lines): Transforms metrics into interactive HTML reports
- `LanguagesMetadata.ts` (840 lines): Master language registry with flavor text
- `PersonaMetadata.ts` (688 lines): Narrative personas for report generation
- `SolverRunner.ts` (153 lines): Executes solvers via shell/Docker
- `HistoryManager.ts` (38 lines): Appends records to benchmark history

**Service Pattern:**
- Singleton-like modules (imported, not instantiated)
- Export async functions: `runSolver()`, `generateHtml()`
- Promise-based return types with explicit typing

## Entry Points

**Server Entry:**
- Location: `server/index.js`
- Triggers: HTTP requests to port 9001
- Responsibilities: Route handling, file serving, process execution

**Report Generator:**
- Location: `Metrics/generate_report_only.ts`
- Triggers: CLI execution or POST /api/generate-report
- Responsibilities: Aggregate metrics, generate HTML, capture screenshots

**Matrix Runner UI:**
- Location: `server/public/index.html`
- Triggers: Browser navigation to /runner
- Responsibilities: Interactive matrix loading and solver execution

**Benchmark CLI:**
- Location: `runBenchmarks.sh`
- Triggers: Shell execution
- Responsibilities: Batch benchmark execution across languages

## Error Handling

**Strategy:** Exception-based with per-layer catch and transform

**Patterns:**
- Solvers use structured status codes: `success`, `timeout`, `error`, `env_error`
- `common.sh` provides `report_env_error()` for environment issues
- Server wraps exec() in try-catch, returns JSON error responses
- Timeout handling: 180s limit via `--timeout` flag

## Cross-Cutting Concerns

**Logging:**
- Console.log for output (extensive usage)
- No log level control or structured logging
- Puppeteer: Screenshots captured for report visualization

**Validation:**
- Reference-based: Compare iterations against C baseline
- Format validation: Output structure matching
- No schema validation on JSON parsing

**File Operations:**
- fs-extra for enhanced file I/O
- glob for file pattern matching
- Atomic writes for metrics files

---

*Architecture analysis: 2025-12-24*
*Update when major patterns change*
