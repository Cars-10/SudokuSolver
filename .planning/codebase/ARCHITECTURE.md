# Architecture

**Analysis Date:** 2025-12-24

## Pattern Overview

**Overall:** Layered Monolith with Modular Service Components

**Key Characteristics:**
- Three-tier architecture (Presentation, Service, Data)
- Event-driven benchmark execution via HTTP API
- Data-centric design with JSON file persistence
- Hybrid sync/async execution model

## Layers

**Presentation Layer:**
- Purpose: Browser UI and HTTP API endpoints
- Contains: Matrix Runner SPA, REST endpoints, static file serving
- Location: `server/index.js`, `server/public/`
- Depends on: Service layer for benchmark execution and report generation
- Used by: Browser clients

**Service Layer:**
- Purpose: Core business logic for benchmarks and reports
- Contains: Solver execution, metrics aggregation, HTML report generation
- Location: `Metrics/*.ts` (SolverRunner, HTMLGenerator, HistoryManager, RepositoryAnalyzer)
- Depends on: Data layer for persistence, child processes for solver execution
- Used by: Presentation layer via CLI or HTTP API

**Data Layer:**
- Purpose: Persistence and configuration
- Contains: Benchmark metrics, language metadata, solver implementations
- Location: `Languages/*/metrics.json`, `benchmark_history.json`, `benchmark_config.json`
- Depends on: File system, SQLite (optional via better-sqlite3)
- Used by: Service layer

## Data Flow

**Solver Execution (via API):**

1. User clicks "Run" in /runner UI
2. `POST /api/run {language, matrix}` sent to server
3. `server/index.js:111` validates language directory exists
4. Determines script: `./runMe.sh` or `./setupAndRunMe.sh`
5. `exec()` runs script in `Languages/{Language}/` directory
6. Script generates `metrics.json` in language directory
7. Server merges with existing metrics.json
8. Appends to `benchmark_history.json`
9. Returns `{success, stdout, stderr, exitCode}` to client

**Report Generation:**

1. `POST /api/generate-report` triggers generation
2. `tsx Metrics/generate_report_only.ts` executed via child process
3. Loads `benchmark_history.json` and scans `Languages/*/metrics.json`
4. Aggregates all metrics with reference outputs from `Matrices/`
5. `HTMLGenerator.generateHtml()` creates report with D3.js visualizations
6. `ScreenshotUtils.captureScreenshot()` captures PNG via Puppeteer
7. Writes `_report.html` and screenshot to root
8. Returns success response

**State Management:**
- File-based: All state in `.json` files and `benchmark_history.json`
- Session persistence: `session_state.json` for UI state
- No in-memory state between requests

## Key Abstractions

**SolverRunner:**
- Purpose: Execute language-specific solvers, capture metrics
- Location: `Metrics/SolverRunner.ts`
- Pattern: Async function with timeout handling
- Methods: `runSolver()` (local), `runDockerSolver()` (container)

**HTMLGenerator:**
- Purpose: Generate comprehensive HTML reports with D3.js
- Location: `Metrics/HTMLGenerator.ts` (2000+ lines)
- Pattern: Large synchronous function with template generation
- Output: Self-contained HTML with embedded data and visualizations

**HistoryManager:**
- Purpose: Track benchmark run history (append-only log)
- Location: `Metrics/HistoryManager.ts`
- Pattern: Read-modify-write file operations
- Methods: `appendRecord()`, `getHistory()`

**RepositoryAnalyzer:**
- Purpose: Discover solvers and read reference outputs
- Location: `Metrics/RepositoryAnalyzer.ts`
- Pattern: File system traversal with glob patterns
- Methods: `findSolvers()`, `readReferenceOutputs()`

## Entry Points

**Web Server Entry:**
- Location: `server/index.js`
- Triggers: `node server/index.js` or `npm start`
- Responsibilities: HTTP API, static file serving, benchmark orchestration

**CLI Entry (Report Generation):**
- Location: `Metrics/generate_report_only.ts`
- Triggers: `tsx Metrics/generate_report_only.ts`
- Responsibilities: Aggregate metrics, generate HTML report

**CLI Entry (Suite Runner):**
- Location: `Metrics/run_suite.ts`
- Triggers: `tsx Metrics/run_suite.ts [language] [matrices]`
- Responsibilities: Multi-language benchmark orchestration

**Per-Language Entry:**
- Location: `Languages/{Language}/setupAndRunMe.sh`
- Triggers: Called by SolverRunner
- Responsibilities: Execute solver, output metrics.json

## Error Handling

**Strategy:** Exception catching at boundaries with status codes

**Patterns:**
- Services throw errors with descriptive messages
- Express handlers catch errors, return JSON responses with error field
- Solver execution uses status codes: `'env_error'`, `'error'`, success states
- Fallback mechanisms: JavaScript fallback when TypeScript generation fails

## Cross-Cutting Concerns

**Logging:**
- Console.log for normal output
- Console.error for errors
- No structured logging framework

**Validation:**
- Minimal input validation in Express handlers
- Type interfaces define expected structures (`types.ts`)
- Reference output comparison for solver correctness

**File Operations:**
- fs-extra for enhanced file system operations
- Mix of sync and async patterns (technical debt)
- Atomic writes not implemented (race condition risk)

**Screenshot Capture:**
- Puppeteer with headless Chrome
- Detects Chrome/Chromium installation path
- Captures at 1920x1080 resolution

---

*Architecture analysis: 2025-12-24*
*Update when major patterns change*
