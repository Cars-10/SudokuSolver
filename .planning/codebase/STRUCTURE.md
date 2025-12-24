# Codebase Structure

**Analysis Date:** 2025-12-24

## Directory Layout

```
SudokuSolver/
├── server/                     # Express.js HTTP backend
│   ├── index.js                # Main server entry (port 9001)
│   ├── package.json            # Server dependencies
│   ├── logo_processor.js       # Logo fetching/processing
│   ├── Dockerfile              # Container build
│   ├── public/                 # Matrix Runner UI
│   │   ├── index.html          # Matrix selection UI
│   │   ├── script.js           # Client-side API calls
│   │   └── style.css           # Glassmorphic styling
│   └── uploads/                # User-uploaded media
│
├── Metrics/                    # Benchmark aggregation (TypeScript)
│   ├── generate_report_only.ts # Report generation entry
│   ├── HTMLGenerator.ts        # HTML report with D3.js (2000+ lines)
│   ├── SolverRunner.ts         # Execute solvers, capture metrics
│   ├── HistoryManager.ts       # Benchmark history persistence
│   ├── RepositoryAnalyzer.ts   # Discover solvers, read references
│   ├── ScreenshotUtils.ts      # Puppeteer screenshot capture
│   ├── LanguagesMetadata.ts    # Language definitions (80+ languages)
│   ├── PersonaMetadata.ts      # Personalities, methodology, narration
│   ├── types.ts                # TypeScript interfaces
│   ├── gather_metrics.ts       # Re-exports all services
│   ├── run_suite.ts            # Multi-language benchmark CLI
│   ├── db_utils.js             # SQLite database utilities
│   ├── validation.js           # Validation utilities
│   ├── package.json            # Metrics dependencies
│   └── tsconfig.json           # TypeScript configuration
│
├── Languages/                  # 80+ language solver implementations
│   ├── C/                      # Baseline solver
│   │   ├── setupAndRunMe.sh    # Execution script
│   │   ├── Sudoku.c            # Solver source
│   │   └── metrics.json        # Per-run results
│   ├── Rust/
│   ├── Python/
│   ├── JavaScript/
│   ├── TypeScript/
│   ├── ... (76+ more)
│   ├── metadata.json           # Global language metadata
│   └── common.sh               # Shared shell utilities
│
├── Matrices/                   # Test puzzles
│   ├── 1.matrix                # Easy puzzle
│   ├── 2.matrix
│   ├── 3.matrix
│   ├── 4.matrix
│   ├── 5.matrix
│   ├── 6.matrix                # Hard puzzle
│   ├── ReferenceForAllMatrixRun.txt  # Expected solutions
│   └── reference_iterations.json     # Expected iteration counts
│
├── logos/                      # Language logos (48+)
│   ├── c.png, cpp.png, ...     # Logo images
│   └── Tailoring.json          # Logo customization rules
│
├── scripts/                    # Utility bash scripts
│   ├── run_benchmark.sh
│   ├── generate_report.sh
│   └── server_control.sh
│
├── Tools/                      # Development utilities
│   ├── migrate_metadata.ts
│   └── scan_backticks.py
│
├── screenshots/                # Generated report screenshots
├── .planning/                  # GSD project planning
│   └── codebase/               # This documentation
│
├── _report.html                # Main benchmark report (generated)
├── benchmark_history.json      # Complete benchmark history
├── benchmark_config.json       # Baseline & matrix configuration
├── session_state.json          # UI session persistence
├── metrics.json                # Root metrics (C baseline)
├── package.json                # Root project dependencies
├── docker-compose.yml          # Docker orchestration
├── MANIFESTO.md                # Project philosophy
└── .env                        # Environment configuration
```

## Directory Purposes

**server/**
- Purpose: HTTP API backend and static file serving
- Contains: Express server, logo processing, Matrix Runner UI
- Key files: `index.js` (main entry), `logo_processor.js` (image transformations)
- Subdirectories: `public/` (SPA), `uploads/` (user files)

**Metrics/**
- Purpose: Benchmark aggregation, report generation, metrics processing
- Contains: TypeScript services, utilities, type definitions
- Key files: `HTMLGenerator.ts` (report), `SolverRunner.ts` (execution), `types.ts` (interfaces)
- Dependencies: Puppeteer, better-sqlite3, fs-extra, glob

**Languages/**
- Purpose: 80+ programming language solver implementations
- Contains: Per-language directories with solver source and execution scripts
- Key files: `setupAndRunMe.sh` (entry), `metrics.json` (results), solver source
- Organization: One directory per language (PascalCase names)

**Matrices/**
- Purpose: Test puzzles and reference outputs
- Contains: Matrix files (1-6), expected solutions
- Key files: `ReferenceForAllMatrixRun.txt`, `reference_iterations.json`

**logos/**
- Purpose: Language visual identifiers
- Contains: PNG logos, customization rules
- Key files: `Tailoring.json` (color transformations per language)

## Key File Locations

**Entry Points:**
- `server/index.js` - HTTP API server (port 9001)
- `Metrics/generate_report_only.ts` - Report generation CLI
- `Metrics/run_suite.ts` - Multi-language benchmark CLI
- `Languages/{Lang}/setupAndRunMe.sh` - Per-language execution

**Configuration:**
- `Metrics/tsconfig.json` - TypeScript settings
- `benchmark_config.json` - Benchmark parameters
- `.env` - Environment variables (WEBHOST, WEBPORT)
- `docker-compose.yml` - Container orchestration

**Core Logic:**
- `Metrics/SolverRunner.ts` - Solver execution with timeout
- `Metrics/HTMLGenerator.ts` - Report generation (2000+ lines)
- `Metrics/HistoryManager.ts` - History persistence
- `Metrics/LanguagesMetadata.ts` - Language definitions

**Testing:**
- `check_invalid.mjs` - Metrics validation script
- `Metrics/validation.js` - Validation utilities
- No test framework configured

**Documentation:**
- `MANIFESTO.md` - Project philosophy
- `GEMINI.md` - AI documentation
- `.planning/` - GSD project planning

## Naming Conventions

**Files:**
- PascalCase for TypeScript services: `HTMLGenerator.ts`, `SolverRunner.ts`
- snake_case for scripts/utilities: `db_utils.js`, `gather_metrics.ts`
- lowercase for data files: `metrics.json`, `benchmark_config.json`
- UPPERCASE for docs: `MANIFESTO.md`, `README.md`

**Directories:**
- PascalCase for languages: `C`, `C++`, `JavaScript`, `TypeScript`
- lowercase for infrastructure: `server`, `scripts`, `logos`
- Capitalized for main components: `Metrics`, `Languages`, `Matrices`

**Special Patterns:**
- `*.ts` for TypeScript source
- `*.js` for compiled/JavaScript
- `setupAndRunMe.sh` for language entry scripts
- `metrics.json` for per-language results

## Where to Add New Code

**New Language Solver:**
- Implementation: `Languages/{LanguageName}/`
- Entry script: `Languages/{LanguageName}/setupAndRunMe.sh`
- Metadata: Update `Languages/metadata.json`
- Logo: Add to `logos/{languagename}.png`

**New Service/Utility:**
- TypeScript service: `Metrics/{ServiceName}.ts`
- Re-export from: `Metrics/gather_metrics.ts`
- Types: Add interfaces to `Metrics/types.ts`

**New API Endpoint:**
- Handler: Add to `server/index.js`
- Pattern: `app.get/post('/api/{endpoint}', handler)`

**New Report Feature:**
- Logic: Modify `Metrics/HTMLGenerator.ts`
- Metadata: Update `Metrics/PersonaMetadata.ts` or `LanguagesMetadata.ts`

## Special Directories

**Languages/**
- Purpose: Solver implementations (80+ languages)
- Source: User-maintained solver code
- Committed: Yes (source of truth for all solvers)
- Note: Each subdirectory is independent with own metrics.json

**Matrices/**
- Purpose: Test puzzles (immutable reference data)
- Source: Pre-defined sudoku puzzles
- Committed: Yes (reference data)

**screenshots/**
- Purpose: Generated benchmark screenshots
- Source: Auto-generated by Puppeteer
- Committed: Selective (final reports only)

**node_modules/**
- Purpose: npm dependencies
- Source: Auto-generated by npm install
- Committed: No (.gitignore)

---

*Structure analysis: 2025-12-24*
*Update when directory structure changes*
