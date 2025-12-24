# Technology Stack

**Analysis Date:** 2025-12-24

## Languages

**Primary:**
- TypeScript 5.x - Metrics processing and report generation (`Metrics/*.ts`)
- JavaScript - Server API and solver implementations (`server/index.js`, `Languages/JavaScript/`)

**Secondary:**
- 80+ programming languages - Sudoku solver implementations (`Languages/*/`)
- Bash - Build scripts, solver wrappers, benchmarking (`Languages/common.sh`, `runBenchmarks.sh`)

**Polyglot Language Support:**
- Compiled: C, C++, Rust, Go, Swift, D, Nim, Crystal, Zig, V, Haxe
- JVM: Java, Kotlin, Scala, Groovy, Clojure
- .NET: C#, F#
- Scripting: Python, Ruby, Perl, PHP, Lua, R, Julia
- Functional: Haskell, OCaml, Elixir, Racket, Lisp
- Esoteric: Brainfuck, COBOL, Fortran, Ada, Prolog, Verilog

## Runtime

**Environment:**
- Node.js 20.x LTS - Primary runtime (`server/Dockerfile` lines 81-86)
- Python 3 - Python solver and utilities (`server/Dockerfile` lines 72-78)
- Go 1.21.5 - Custom compiled binary (`server/Dockerfile` lines 89-98)
- Rust (stable) - Via rustup (`server/Dockerfile` lines 100-104)
- OpenJDK 21 - JVM languages (`server/Dockerfile` lines 106-114)
- .NET SDK 8.0 - C#/F# support (`server/Dockerfile` lines 124-131)
- Docker Base: Ubuntu 24.04 LTS (Noble Numbat)

**Package Manager:**
- npm - Node.js dependencies
- Lockfiles: `package-lock.json` present in root, `Metrics/`, `server/`, `Languages/TypeScript/`, `Languages/JavaScript/`

## Frameworks

**Core:**
- Express.js 4.18.2 - HTTP server (`server/package.json`)
- Express.js 5.2.1 - Metrics API (`Metrics/package.json`)

**Testing:**
- Custom validation framework - Reference-based testing (`Metrics/validation.js`)
- No traditional test runner (Jest, Vitest) installed

**Build/Dev:**
- tsx 4.21.0 - TypeScript execution without build step (`package.json`)
- ts-node 10.9.0+ - TypeScript Node runtime
- TypeScript 5.x - Compilation (`Metrics/tsconfig.json`, `Languages/TypeScript/tsconfig.json`)

## Key Dependencies

**Critical:**
- better-sqlite3 11.0.0 - Embedded SQL database (`server/package.json`, `Metrics/package.json`)
- puppeteer 24.32.0 - Headless browser for screenshots (`package.json`)
- sharp 0.33.5 - Image processing for logos (`server/package.json`)

**Infrastructure:**
- cors 2.8.5 - CORS middleware (`server/package.json`)
- multer 1.4.5 - File upload handling (`server/package.json`)
- node-fetch 3.3.2 - HTTP client (`server/package.json`)
- glob 13.0.0 - File pattern matching (`package.json`)
- fs-extra 11.3.2 - Enhanced file operations (`package.json`)
- svg2png-wasm 1.4.1 - SVG to PNG conversion (`server/package.json`)

## Configuration

**Environment:**
- `.env` file for server configuration
- Variables: `WEBHOST=localhost`, `WEBPORT=3000`
- Docker: `PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true`, `PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium-browser`

**Build:**
- `Metrics/tsconfig.json` - ES2022 target, ESM modules
- `Languages/TypeScript/tsconfig.json` - ES2020 target, CommonJS, strict mode
- `benchmark_config.json` - Benchmark execution configuration

## Platform Requirements

**Development:**
- macOS/Linux/Windows (any platform with Node.js and Docker)
- Docker Desktop for full benchmark suite

**Production:**
- Docker container (Ubuntu 24.04 base)
- Port 9001 for API server
- Volume mounts for Languages, Matrices, Metrics, logos, screenshots

---

*Stack analysis: 2025-12-24*
*Update after major dependency changes*
