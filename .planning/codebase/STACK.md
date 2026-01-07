# Technology Stack

**Analysis Date:** 2026-01-07

## Languages

**Primary:**
- JavaScript/Node.js - Server, metrics collection, report generation (`server/index.js`, `Metrics/*.js`)
- TypeScript 5.3.2 - Core metrics modules (`Metrics/*.ts`, `Metrics/tsconfig.json`)

**Secondary:**
- Bash/Shell - Build scripts, solver runners (`Languages/*/setupAndRunMe.sh`, `runBenchmarks.sh`)
- **88 benchmark languages** - Polyglot solver implementations (`Languages/` directory)
  - Systems: C, C++, Rust, Go, Zig, Nim, Ada, Pascal, D
  - JVM: Java, Kotlin, Scala, Clojure, Groovy
  - .NET: C#, F#, VB.NET
  - Scripting: Python, Ruby, Perl, Lua, PHP, JavaScript, TypeScript
  - Functional: Haskell, OCaml, Elixir, Erlang, Scheme, Racket
  - And 50+ more esoteric languages

## Runtime

**Environment:**
- Node.js 20.x (LTS) - Primary runtime
- Docker/Ubuntu 24.04 - Containerized execution (`server/Dockerfile`)

**Package Manager:**
- npm - Primary package manager
- Lockfiles:
  - `package-lock.json` - Root dependencies
  - `Metrics/package-lock.json` - Metrics module dependencies

## Frameworks

**Core:**
- Express.js 4.18.2 / 5.2.1 - REST API server (`server/package.json`, `Metrics/package.json`)

**Build/Dev:**
- tsx 4.21.0 - TypeScript execution without compilation (`package.json`)
- TypeScript 5.3.2 - Type checking (`Metrics/package.json`)
- ts-node 10.9.1 - TypeScript Node runner (`Metrics/package.json`)

**Browser Automation:**
- Puppeteer 21.0.0-24.32.0 - Headless Chrome for screenshots/reports (`package.json`, `server/package.json`)

## Key Dependencies

**Critical:**
- better-sqlite3 11.0.0 - SQLite database access (`Metrics/db_utils.js`)
- sharp 0.33.5 - Image processing, SVG to PNG conversion (`server/logo_processor.js`)
- multer 1.4.5-lts.1 - File upload handling (`server/index.js`)
- node-fetch 3.3.2 - HTTP client for external resources (`server/logo_processor.js`)

**Infrastructure:**
- cors 2.8.5 - CORS middleware (`server/package.json`)
- glob 10.0.0/13.0.0 - File pattern matching (`package.json`, `Metrics/package.json`)
- fs-extra 11.0.0 - Enhanced file operations (`package.json`)
- svg2png-wasm 1.4.1 - WebAssembly SVG conversion (`server/package.json`)

## Configuration

**Environment:**
- `.env` file with minimal config:
  - `WEBHOST=localhost`
  - `WEBPORT=3000`
- Server defaults to port 9001 if PORT not set

**TypeScript:**
- `Metrics/tsconfig.json`:
  - Target: ES2022
  - Module: ES2022 (ESM)
  - strict: false
  - noEmit: true (no compilation output)

**Build:**
- `docker-compose.yml` - Container orchestration
- `server/Dockerfile` - Ubuntu 24.04 base image

## Platform Requirements

**Development:**
- macOS/Linux/Windows (cross-platform Node.js)
- Docker optional for containerized execution
- 88 language runtimes for full benchmark coverage

**Production:**
- Docker container (Ubuntu 24.04)
- Port 9001 exposed
- Volumes for Languages, Matrices, Metrics, logos, screenshots

---

*Stack analysis: 2026-01-07*
*Update after major dependency changes*
