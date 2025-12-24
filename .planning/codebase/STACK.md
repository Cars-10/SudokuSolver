# Technology Stack

**Analysis Date:** 2025-12-24

## Languages

**Primary:**
- TypeScript 5.3.2 - All metrics aggregation, report generation, and service code (`Metrics/*.ts`)
- JavaScript (Node.js) - Server backend and legacy utilities (`server/index.js`, `Metrics/*.js`)

**Secondary:**
- Shell (Bash) - Build scripts, solver execution (`runBenchmarks.sh`, `Languages/*/setupAndRunMe.sh`)
- HTML/CSS/JavaScript - Static UI for Matrix Runner (`server/public/`)
- 80+ Programming Languages - Solver implementations (`Languages/`)

## Runtime

**Environment:**
- Node.js 18+ (LTS) - Primary runtime for server and metrics
- ES2022 module system - TypeScript target (`Metrics/tsconfig.json`)
- Docker (Ubuntu 24.04) - Polyglot container with 80+ language toolchains (`server/Dockerfile`)

**Package Manager:**
- npm - Primary package manager
- Lockfiles present:
  - `package-lock.json` - Root dependencies
  - `Metrics/package-lock.json` - Metrics module
  - `server/package-lock.json` - Server module

## Frameworks

**Core:**
- Express.js 4.18.2/5.2.1 - REST API backend (`server/index.js`, `Metrics/package.json`)
- CORS 2.8.5 - Cross-origin request handling

**Testing:**
- None configured - Manual validation via scripts (`check_invalid.mjs`, `validation.js`)

**Build/Dev:**
- TypeScript 5.3.2 - Type checking and compilation (`Metrics/tsconfig.json`)
- tsx 4.21.0 - TypeScript execution without build step (`package.json`)
- ts-node 10.9.1 - TypeScript execution (`Metrics/package.json`)

## Key Dependencies

**Critical:**
- better-sqlite3 11.0.0 - Embedded database for benchmark metrics (`Metrics/db_utils.js`)
- Puppeteer 24.31.0/24.32.0 - Headless Chrome for screenshots (`Metrics/ScreenshotUtils.ts`)
- Sharp 0.33.5 - Image processing for logos (`server/logo_processor.js`)
- fs-extra 11.x - Enhanced file system operations (`package.json`, `Metrics/package.json`)

**Infrastructure:**
- glob 10.5.0/13.0.0 - File pattern matching (`Metrics/RepositoryAnalyzer.ts`)
- multer 1.4.5-lts.1 - File upload middleware (`server/index.js`)
- node-fetch 3.3.2 - HTTP client for external resources (`server/index.js`)

## Configuration

**Environment:**
- `.env` file with `WEBHOST=localhost`, `WEBPORT=3000`
- `PORT` environment variable (defaults to 9001)
- No `.env.example` template

**Build:**
- `Metrics/tsconfig.json` - TypeScript configuration (ES2022, strict: false)
- `benchmark_config.json` - Benchmark execution parameters (baseline language, matrix lists)
- `docker-compose.yml` - Container orchestration

## Platform Requirements

**Development:**
- macOS/Linux/Windows with Node.js 18+
- Chrome/Chromium for Puppeteer screenshot capture
- Docker for containerized benchmark execution (optional)

**Production:**
- Docker container with Ubuntu 24.04 base
- 80+ language toolchains installed
- Port 9001 exposed for HTTP API
- Volume mounts for Languages, Matrices, Metrics, logos, screenshots

---

*Stack analysis: 2025-12-24*
*Update after major dependency changes*
