# Plan: Docker & Database Foundation

**Phase:** 1 - Foundation & C Baseline
**Plan:** 01 of 05
**Created:** 2025-12-16
**Status:** Ready for execution

## Objective

Build the foundational infrastructure: Docker image with all 15 Tier 1 language toolchains and SQLite database schema. These are critical path blockers for all subsequent work.

## Context

Current Dockerfile only includes Node 18 + Python 3. We need to add 13 more language toolchains (C, C++, Go, Rust, Java, Kotlin, Scala, C#, PHP, Ruby, Perl, Swift, TypeScript).

SQLite database is identified as the critical path dependency - must be operational before metrics can be captured from language runs.

## Tasks

<task id="1" type="auto">
<description>Update Dockerfile with all 15 Tier 1 language toolchains</description>

<context>
Current Dockerfile at /server/Dockerfile has:
- Node 18 (JavaScript/TypeScript base)
- Python 3
- build-essential (gcc, g++, make for C/C++)
- Chromium + Puppeteer

Need to add:
- Go (latest stable)
- Rust (via rustup)
- Java (OpenJDK 17 or later)
- Kotlin (via SDKMAN or manual install)
- Scala (via SDKMAN)
- C# (.NET SDK 8.0)
- PHP (8.x latest)
- Ruby (3.x latest)
- Perl (already in Debian, verify)
- Swift (official Docker image or manual)

Use ubuntu:24.04 as base (upgrade from node:18-bullseye which is Debian 11).
</context>

<files>
- /server/Dockerfile - Complete rewrite for ubuntu:24.04 base
</files>

<actions>
1. Replace FROM node:18-bullseye with FROM ubuntu:24.04
2. Install Node.js 20 LTS (via NodeSource PPA or nvm)
3. Add language toolchains in logical groups:
   - System tools: build-essential, time, curl, wget, git
   - Compiled: gcc, g++, make (C/C++)
   - Go: Download and install Go 1.21+
   - Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   - JVM: OpenJDK 17+, Kotlin, Scala via SDKMAN
   - .NET: Download .NET SDK 8.0
   - Scripting: Python 3.11+, PHP 8.x, Ruby 3.x, Perl
   - Swift: Investigate if available for Ubuntu 24.04
   - TypeScript: npm install -g typescript tsx ts-node
4. Keep Puppeteer setup (Chromium + ENV variables)
5. Set WORKDIR /app
6. Install server npm dependencies
7. Add comment documenting each toolchain version
</actions>

<verification>
- [ ] Dockerfile builds without errors
- [ ] All 15 language toolchains installed
- [ ] Can run: gcc --version, python3 --version, go version, rustc --version, java --version, etc.
- [ ] Image size documented (expected ~8-12GB)
</verification>

<acceptance>
Docker image builds successfully and includes working installations of all 15 Tier 1 language toolchains with versions documented in Dockerfile comments.
</acceptance>
</task>

<task id="2" type="auto">
<description>Design and implement SQLite database schema</description>

<context>
Need time-series benchmark database to replace scattered JSON files. Must support queries like:
- "Which language is fastest for Matrix 3?"
- "How has C performance changed over time?"
- "Show all runs for Go with -O3 optimization"

From PROJECT.md: Database should track language, matrix, timestamp, iterations, time, memory, CPU usage, status, compiler variant.
</context>

<files>
- /Metrics/schema.sql - New SQLite schema definition
- /Metrics/init_database.js - Database initialization script
- /Metrics/benchmarks.db - Created by init script (gitignored)
</files>

<actions>
1. Create schema.sql with tables:
   ```sql
   CREATE TABLE IF NOT EXISTS runs (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       timestamp TEXT NOT NULL,
       language TEXT NOT NULL,
       matrix INTEGER NOT NULL,
       iterations INTEGER,
       time_seconds REAL,
       memory_kb INTEGER,
       cpu_user REAL,
       cpu_sys REAL,
       status TEXT NOT NULL,
       output TEXT,
       compiler_variant TEXT,
       toolchain_version TEXT,
       created_at DATETIME DEFAULT CURRENT_TIMESTAMP
   );

   CREATE INDEX idx_runs_timestamp ON runs(timestamp);
   CREATE INDEX idx_runs_language ON runs(language);
   CREATE INDEX idx_runs_matrix ON runs(matrix);
   CREATE INDEX idx_runs_language_matrix ON runs(language, matrix);
   ```

2. Create init_database.js that:
   - Creates Metrics/benchmarks.db if not exists
   - Runs schema.sql
   - Validates table creation
   - Reports success/failure

3. Add database utilities:
   - insertRun(data) function
   - queryRuns(filters) function
   - Export for use in generate_report_only.ts

4. Update Metrics/package.json to include better-sqlite3 dependency
</actions>

<verification>
- [ ] schema.sql creates all tables and indexes
- [ ] init_database.js runs without errors
- [ ] benchmarks.db file created in Metrics/
- [ ] Can insert test run and query it back
- [ ] Indexes exist (check with .schema in sqlite3 CLI)
</verification>

<acceptance>
SQLite database operational with complete schema. Init script creates database, test insert/query works, indexes present for time-series queries.
</acceptance>
</task>

<task id="3" type="checkpoint:build_test">
<description>Build Docker image and verify all toolchains functional</description>

<context>
Must validate that the updated Dockerfile builds successfully and all 15 language toolchains are working before proceeding to next plan. This is a critical gate - if any toolchain fails, we need to fix it now.

Image name: sudoku-benchmark:latest (reuse this name, remove old image first per PROJECT.md constraint)
</context>

<files>
- /docker-compose.yml - May need port update (9001 external per PROJECT.md)
- /server/Dockerfile - Built in task 1
</files>

<actions>
1. Remove old Docker image if exists:
   ```bash
   docker rmi sudoku-benchmark:latest || true
   ```

2. Build new image:
   ```bash
   docker build -t sudoku-benchmark:latest -f server/Dockerfile .
   ```

3. Update docker-compose.yml:
   - Change image name to sudoku-benchmark:latest
   - Change external port to 9001 (was 3000)
   - Verify volume mounts include Metrics/ for database

4. Start container:
   ```bash
   docker-compose up -d
   ```

5. Test each toolchain inside container:
   ```bash
   docker exec -it <container> bash
   # Test each:
   gcc --version
   g++ --version
   python3 --version
   go version
   rustc --version
   java --version
   kotlinc -version
   scalac -version
   dotnet --version
   php --version
   ruby --version
   perl --version
   swift --version  # if available
   node --version
   tsc --version
   ```

6. Test SQLite database:
   ```bash
   docker exec -it <container> node /app/Metrics/init_database.js
   docker exec -it <container> ls -lh /app/Metrics/benchmarks.db
   ```

7. Document actual image size:
   ```bash
   docker images sudoku-benchmark:latest
   ```
</actions>

<verification>
- [ ] Old image removed
- [ ] New image builds without errors
- [ ] Image size documented (actual vs estimated)
- [ ] Container starts successfully on port 9001
- [ ] All 15 toolchain version commands succeed
- [ ] SQLite database initializes inside container
- [ ] Can access Content Server at localhost:9001
</verification>

<acceptance>
Docker image sudoku-benchmark:latest built and running. All 15 language toolchains verified functional. SQLite database initialized. Content Server accessible on port 9001.
</acceptance>
</task>

## Dependencies

**Blocks:**
- Plan 02 (Modular Scripts) - needs Docker container to test scripts
- Plan 03 (Validation Systems) - needs database for metrics storage
- Plan 04 (C Baseline) - needs toolchains and database operational
- Plan 05 (Content Server) - needs container running

**Blocked by:**
- None (first plan in phase)

## Risks

- **Docker image size >15GB**: Acceptable per PROJECT.md, defer optimization
- **Swift unavailable for Ubuntu 24.04**: Skip Swift for Phase 1, add in Phase 5 with macOS runner or alternative
- **Toolchain installation conflicts**: Use Docker multi-stage builds or careful ordering
- **Build time >30 minutes**: Expected with 15 toolchains, acceptable for one-time setup

## Success Criteria

- [x] Dockerfile updated with all 15 Tier 1 toolchains
- [x] SQLite schema created and tested
- [x] Docker image builds successfully
- [x] All toolchains verified working
- [x] Database operational
- [x] Container accessible on port 9001

## Notes

This plan establishes the foundation for all Phase 1 work. The Docker image is built once and reused across all remaining plans. SQLite database enables metrics capture from Plan 04 onward.

Per 01-CONTEXT.md: "Critical path dependency: SQLite database must be operational before other components" - this plan unblocks that critical path.
