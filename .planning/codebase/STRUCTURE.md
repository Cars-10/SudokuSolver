# Codebase Structure

**Analysis Date:** 2026-01-07

## Directory Layout

```
SudokuSolver/
├── server/                    # Express.js web server
│   ├── public/               # Static UI assets
│   ├── uploads/              # Temporary file uploads
│   └── Dockerfile            # Container definition
├── Metrics/                   # Core metrics & reporting
│   └── node_modules/         # Module dependencies
├── Languages/                 # 88+ solver implementations
│   ├── C/                    # Each language has its own dir
│   ├── Python/
│   ├── Rust/
│   └── ... (85+ more)
├── Matrices/                  # Test puzzle inputs
│   └── reference_output/     # Expected solutions
├── logos/                     # Language logo cache
├── screenshots/               # Report screenshots
├── scripts/                   # Utility shell scripts
├── Tools/                     # Helper utilities
├── .planning/                 # Project planning docs
│   └── codebase/             # This analysis
└── .claude/                   # Claude AI integration
```

## Directory Purposes

**server/**
- Purpose: REST API server and web UI
- Contains: Express.js server code, static assets, Dockerfile
- Key files:
  - `index.js` - Main API server (port 9001)
  - `logo_processor.js` - Image processing utilities
  - `public/index.html` - Matrix runner UI
  - `public/script.js` - Client-side logic
  - `public/style.css` - Neon-themed styling

**Metrics/**
- Purpose: Benchmark data collection, processing, and reporting
- Contains: TypeScript/JavaScript modules, SQLite database, schema
- Key files:
  - `gather_metrics.ts` - Main metrics aggregator
  - `generate_report_only.ts` - Report generation entry point
  - `HTMLGenerator.ts` - HTML report builder (1996 lines)
  - `SolverRunner.ts` - Solver execution handler
  - `RepositoryAnalyzer.ts` - Solver discovery
  - `HistoryManager.ts` - Benchmark history persistence
  - `db_utils.js` - SQLite database operations
  - `sync_db.ts` - JSON to SQLite sync
  - `types.ts` - TypeScript interfaces
  - `benchmarks.db` - SQLite database
  - `schema.sql` - Database schema definition

**Languages/**
- Purpose: Solver implementations for 88+ programming languages
- Contains: One subdirectory per language
- Per-language structure:
  - `setupAndRunMe.sh` - Setup and execution script
  - `Sudoku.{ext}` - Solver source code
  - `metrics.json` - Benchmark results
  - `Media/` - Optional media files
  - `Dockerfile` - Optional container definition

**Matrices/**
- Purpose: Sudoku test puzzle inputs
- Contains: `.matrix` files with puzzle data
- Key files:
  - `1.matrix` through `6.matrix` - Test puzzles
  - `reference_output/` - Expected solutions
  - `ReferenceForAllMatrixRun.txt` - Combined reference

**logos/**
- Purpose: Cached language logo images
- Contains: PNG images for each language
- Key files:
  - `{language}.png` - Processed logo images
  - `Tailoring.json` - Image transformation rules

**scripts/**
- Purpose: Utility shell scripts
- Contains: Helper scripts for common operations
- Key files:
  - `run_benchmark.sh` - Benchmark runner wrapper
  - `generate_report.sh` - Report generation wrapper
  - `server_control.sh` - Server start/stop

## Key File Locations

**Entry Points:**
- `server/index.js` - Web server entry (port 9001)
- `Metrics/generate_report_only.ts` - Report generation
- `Metrics/run_suite.ts` - CLI benchmark runner
- `Metrics/sync_db.ts` - Database sync

**Configuration:**
- `.env` - Environment variables (WEBHOST, WEBPORT)
- `package.json` - Root dependencies
- `Metrics/package.json` - Metrics module dependencies
- `Metrics/tsconfig.json` - TypeScript configuration
- `docker-compose.yml` - Container orchestration
- `benchmark_config.json` - Benchmark settings
- `logos/Tailoring.json` - Image processing rules

**Core Logic:**
- `Metrics/HTMLGenerator.ts` - Report generation (1996 lines)
- `Metrics/SolverRunner.ts` - Solver execution
- `Metrics/db_utils.js` - Database operations
- `server/logo_processor.js` - Image processing

**Data Files:**
- `benchmark_history.json` - All historical runs
- `session_state.json` - UI state persistence
- `Metrics/benchmarks.db` - SQLite database
- `Languages/metadata.json` - Global language metadata

**Generated Output:**
- `_report.html` - Main benchmark report
- `_report_screenshot.html` - Screenshot version
- `screenshots/benchmark_*.png` - Report screenshots

**Documentation:**
- `MANIFESTO.md` - Project philosophy & standards
- `GEMINI.md` - Supplementary documentation

## Naming Conventions

**Files:**
- PascalCase.ts: TypeScript modules (`HTMLGenerator.ts`, `SolverRunner.ts`)
- lowercase.js: JavaScript files (`db_utils.js`, `report_client.js`)
- snake_case.ts: Multi-word scripts (`sync_db.ts`, `run_suite.ts`)
- setupAndRunMe.sh: Language entry scripts (camelCase)
- *.matrix: Test puzzle files
- _report.html: Generated output (underscore prefix)

**Directories:**
- PascalCase: Language directories (`Languages/C_Sharp/`, `Languages/Python/`)
- lowercase: Infrastructure dirs (`server/`, `scripts/`, `logos/`)
- dot-prefix: Hidden/config dirs (`.planning/`, `.claude/`)

**Special Patterns:**
- `metrics.json`: Standard output per language
- `{Language}/Media/`: Per-language media uploads
- `benchmark_*.json/html`: Benchmark-related files

## Where to Add New Code

**New Language Solver:**
- Implementation: `Languages/{LanguageName}/`
- Required: `setupAndRunMe.sh` or `runMe.sh`
- Output: `metrics.json` in same directory
- Optional: `Dockerfile` for containerized execution

**New API Endpoint:**
- Implementation: `server/index.js`
- Pattern: Add `app.get/post('/api/endpoint', handler)`

**New Metrics Module:**
- Implementation: `Metrics/{ModuleName}.ts`
- Types: Add to `Metrics/types.ts`
- Exports: Named exports preferred

**New Report Feature:**
- Implementation: `Metrics/HTMLGenerator.ts`
- Client-side: `Metrics/report_client.js`

**Utility Scripts:**
- Implementation: `scripts/{script-name}.sh`
- Or root level for main workflows

## Special Directories

**.planning/**
- Purpose: Project planning and analysis docs
- Source: GSD workflow outputs
- Committed: Yes

**screenshots/**
- Purpose: Captured report screenshots
- Source: Puppeteer automation
- Committed: Selectively (some auto-generated)

**server/uploads/**
- Purpose: Temporary file upload storage
- Source: multer middleware
- Committed: No (gitignored)

**node_modules/**
- Purpose: npm dependencies
- Source: npm install
- Committed: No (gitignored)

---

*Structure analysis: 2026-01-07*
*Update when directory structure changes*
