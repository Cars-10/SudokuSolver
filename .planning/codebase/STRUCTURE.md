# Codebase Structure

**Analysis Date:** 2025-12-24

## Directory Layout

```
SudokuSolver/
├── server/                 # Express.js HTTP API
│   ├── index.js            # Main server (499 lines)
│   ├── logo_processor.js   # Logo processing utility
│   ├── package.json        # Server dependencies
│   ├── public/             # Matrix runner UI
│   │   ├── index.html      # Runner interface
│   │   ├── script.js       # UI logic
│   │   └── style.css       # Styling
│   └── uploads/            # Uploaded media storage
├── Metrics/                # Report generation (4283 lines TS)
│   ├── HTMLGenerator.ts    # Report HTML (2001 lines)
│   ├── LanguagesMetadata.ts # Language registry (840 lines)
│   ├── PersonaMetadata.ts  # Narrative personas (688 lines)
│   ├── generate_report_only.ts # Main generator
│   ├── SolverRunner.ts     # Solver execution
│   ├── HistoryManager.ts   # History persistence
│   ├── ScreenshotUtils.ts  # Puppeteer screenshots
│   ├── types.ts            # TypeScript interfaces
│   └── package.json        # Metrics dependencies
├── Languages/              # 80+ language implementations
│   ├── common.sh           # Shared bash functions
│   ├── C/                  # Reference implementation
│   ├── C++/
│   ├── Rust/
│   ├── Python/
│   ├── JavaScript/
│   ├── TypeScript/
│   └── [80+ more languages]
├── Matrices/               # Test inputs
│   ├── 1.matrix - 6.matrix
│   └── reference_output/
├── scripts/                # Utility scripts
├── logos/                  # Language logo assets
├── screenshots/            # Report screenshots
├── benchmark_config.json   # Tier/matrix config
├── benchmark_history.json  # Historical runs
├── docker-compose.yml      # Docker orchestration
├── runBenchmarks.sh        # Master runner
└── package.json            # Root dependencies
```

## Directory Purposes

**server/**
- Purpose: HTTP API and static file serving
- Contains: Express routes, file handlers, logo processor
- Key files: `index.js` (main server), `public/index.html` (runner UI)
- Subdirectories: `public/` (UI), `uploads/` (media storage)

**Metrics/**
- Purpose: Benchmark aggregation, report generation, visualization
- Contains: TypeScript modules for HTML generation and data processing
- Key files: `HTMLGenerator.ts` (report builder), `generate_report_only.ts` (entry point)
- Note: Contains both `.ts` source and compiled `.js` files

**Languages/**
- Purpose: Multi-language Sudoku solver implementations
- Contains: 80+ language folders with solver code and scripts
- Key files: `common.sh` (shared utilities), each folder has `runMe.sh`/`setupAndRunMe.sh`
- Pattern: Each language is self-contained with `Sudoku.*`, `metrics.json`, `metadata.json`

**Matrices/**
- Purpose: Benchmark test inputs (Sudoku puzzles)
- Contains: 6 matrix files of varying difficulty
- Key files: `*.matrix` (puzzle inputs), `reference_output/` (expected solutions)

## Key File Locations

**Entry Points:**
- `server/index.js` - HTTP API server (port 9001)
- `Metrics/generate_report_only.ts` - Report generation CLI
- `runBenchmarks.sh` - Batch benchmark runner

**Configuration:**
- `package.json` - Root npm config
- `benchmark_config.json` - Language tiers, matrix thresholds
- `.env` - Server host/port configuration
- `docker-compose.yml` - Container orchestration

**Core Logic:**
- `server/index.js:152` - Benchmark execution
- `Metrics/HTMLGenerator.ts` - Report visualization
- `Metrics/SolverRunner.ts` - Solver process management
- `Languages/common.sh` - Shared bash utilities

**Testing:**
- `Metrics/validation.js` - Iteration validation
- `Metrics/validate_run.js` - CLI test runner
- No traditional test framework

**Documentation:**
- `Languages/README.md` - Language implementation guide
- `CLAUDE.md` - Claude Code instructions

## Naming Conventions

**Files:**
- PascalCase.ts: Class-like modules (`SolverRunner.ts`, `HTMLGenerator.ts`)
- snake_case.ts: Script-style files (`run_suite.ts`, `gather_metrics.ts`)
- kebab-case.sh: Shell scripts (`runMe.sh`, `setupAndRunMe.sh`)
- UPPERCASE.md: Important docs (`README.md`, `CLAUDE.md`)

**Directories:**
- PascalCase: Language folders (`C++`, `JavaScript`, `C_Sharp`)
- lowercase: Utility folders (`server`, `scripts`, `logos`)
- Underscores: Language names with symbols (`C_Sharp`, `F_Sharp`)

**Special Patterns:**
- `setupAndRunMe.sh`: Full setup + benchmark (compiles if needed)
- `runMe.sh`: Benchmark only (assumes compiled)
- `metrics.json`: Per-language execution results
- `metadata.json`: Per-language custom metadata

## Where to Add New Code

**New Language Solver:**
- Create: `Languages/[LanguageName]/`
- Add: `Sudoku.*` (solver), `runMe.sh` or `setupAndRunMe.sh`
- Optionally: `metadata.json`, `Media/` folder for screenshots
- Register in `Metrics/LanguagesMetadata.ts`

**New API Endpoint:**
- Location: `server/index.js`
- Pattern: `app.get/post('/api/[route]', handler)`
- Add route handling with proper error responses

**New Report Feature:**
- Location: `Metrics/HTMLGenerator.ts`
- Add method to generate HTML section
- Update `generateHtml()` to include new section

**New Metrics Processing:**
- Location: `Metrics/` directory
- Create new `.ts` file following existing patterns
- Export functions, import in `generate_report_only.ts`

## Special Directories

**.planning/**
- Purpose: Project planning documents (GSD system)
- Source: Created by /gsd commands
- Committed: Yes

**logos/**
- Purpose: Language visual identifiers (PNG/SVG)
- Served: Static at `/logos/*`
- Processing: `server/logo_processor.js`

**screenshots/**
- Purpose: Report visualization captures
- Generated: By Puppeteer via `ScreenshotUtils.ts`
- Used: Documentation, history tracking

---

*Structure analysis: 2025-12-24*
*Update when directory structure changes*
