# Architecture

**Analysis Date:** 2026-01-07

## Pattern Overview

**Overall:** Polyglot Benchmark Runner with Analytics Platform

**Key Characteristics:**
- Self-contained monolith with clear layered separation
- 88+ language solver implementations in isolated directories
- File-based state (JSON + SQLite) rather than external databases
- Minimal framework dependencies - pragmatic vanilla approach
- Docker-ready for consistent execution environment

## Layers

**Execution Layer (`Languages/`):**
- Purpose: Contains solver implementations for 88+ programming languages
- Contains: `setupAndRunMe.sh` entry scripts, solver source code, `metrics.json` output
- Depends on: Language runtimes (compilers/interpreters)
- Used by: Server layer via child_process.exec()

**Metrics Collection Layer (`Metrics/`):**
- Purpose: Aggregate, analyze, and persist benchmark data
- Contains: TypeScript modules for data processing, SQLite database, report generation
- Depends on: File system (JSON files), SQLite database
- Used by: Server API endpoints, CLI tools

**Server/API Layer (`server/`):**
- Purpose: REST API and web UI serving
- Contains: Express.js server, public assets, logo processing
- Depends on: Metrics layer for report generation, Execution layer for running solvers
- Used by: Web browsers, CLI tools

**Storage Layer:**
- Purpose: Persistent data storage
- Contains: JSON files (metrics, history, state), SQLite database
- Depends on: File system
- Used by: All other layers

**Frontend Layer (`server/public/`):**
- Purpose: Interactive UI for benchmark execution
- Contains: Vanilla HTML/CSS/JS, matrix runner interface
- Depends on: Server API endpoints
- Used by: End users via browser

## Data Flow

**Benchmark Execution:**

1. User clicks "Run" in UI (`server/public/script.js`)
2. POST /api/run sent to server (`server/index.js`)
3. Server locates `Languages/{lang}/setupAndRunMe.sh`
4. Script executed via `child_process.exec()`
5. Solver writes `metrics.json` in its directory
6. Server reads and returns metrics to UI
7. Metrics appended to `benchmark_history.json`

**Report Generation:**

1. User triggers report via UI or CLI
2. POST /api/generate-report → `server/index.js`
3. Server invokes `tsx Metrics/generate_report_only.ts`
4. Report generator loads `benchmark_history.json`
5. `HTMLGenerator.ts` produces `_report.html`
6. Optional: Puppeteer captures screenshot
7. Report served via GET /

**Database Sync:**

1. Run `tsx Metrics/sync_db.ts`
2. Deletes existing `benchmarks.db`
3. Initializes fresh schema from `schema.sql`
4. Globs all `Languages/*/metrics.json` files
5. Flattens and inserts records into SQLite
6. Database ready for queries

**State Management:**
- File-based: All state in `.json` files
- No in-memory state between requests
- Session state persisted in `session_state.json`

## Key Abstractions

**SolverRunner (`Metrics/SolverRunner.ts`):**
- Purpose: Execute solver scripts and capture metrics
- Methods: `runSolver()`, `runDockerSolver()`
- Pattern: Async function with timeout handling (180s)

**HTMLGenerator (`Metrics/HTMLGenerator.ts`):**
- Purpose: Generate self-contained HTML benchmark reports
- Methods: `generateHtml()`
- Pattern: String template generation with embedded D3.js

**HistoryManager (`Metrics/HistoryManager.ts`):**
- Purpose: Persist benchmark history across runs
- Methods: `appendRecord()`, `getHistory()`
- Pattern: Singleton-like file manager

**RepositoryAnalyzer (`Metrics/RepositoryAnalyzer.ts`):**
- Purpose: Discover available solvers and reference outputs
- Methods: `findSolvers()`, `readReferenceOutputs()`
- Pattern: Glob-based file discovery

**Database Utilities (`Metrics/db_utils.js`):**
- Purpose: SQLite CRUD operations
- Methods: `getDatabase()`, `insertRun()`, `queryRuns()`
- Pattern: Direct SQL via better-sqlite3

## Entry Points

**Web Server:**
- Location: `server/index.js`
- Triggers: `node server/index.js` or docker-compose
- Responsibilities: Serve UI, handle API requests, orchestrate execution
- Port: 9001 (default)

**Report Generation:**
- Location: `Metrics/generate_report_only.ts`
- Triggers: `tsx Metrics/generate_report_only.ts` or POST /api/generate-report
- Responsibilities: Load metrics, generate HTML report with D3 charts

**Benchmark Suite:**
- Location: `Metrics/run_suite.ts`
- Triggers: `ts-node Metrics/run_suite.ts [language] [matrices]`
- Responsibilities: Run selective benchmarks from CLI

**Database Sync:**
- Location: `Metrics/sync_db.ts`
- Triggers: `tsx Metrics/sync_db.ts`
- Responsibilities: Synchronize JSON metrics to SQLite

## Error Handling

**Strategy:** Log errors and continue, with fallback paths

**Patterns:**
- Server catches exec errors, logs to stderr, returns error response
- Report generation has fallback: TypeScript → JavaScript generator
- Solver timeouts return `status: 'timeout'` in metrics
- Database operations use try/catch with connection cleanup

## Cross-Cutting Concerns

**Logging:**
- Console.log/console.error throughout
- Emoji markers for visual clarity: `🔄`, `✅`, `❌`
- No structured logging framework

**Validation:**
- Minimal input validation on API endpoints
- Zod not used despite being a dependency
- Metrics JSON parsed without schema validation

**Configuration:**
- `.env` file for host/port
- `benchmark_config.json` for run settings
- `logos/Tailoring.json` for image processing rules

---

*Architecture analysis: 2026-01-07*
*Update when major patterns change*
