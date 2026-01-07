# External Integrations

**Analysis Date:** 2026-01-07

## APIs & External Services

**No Third-Party External APIs:**
- No payment processing (Stripe, Square, PayPal)
- No authentication services (Auth0, Firebase, OAuth)
- No analytics (Google Analytics, Mixpanel)
- No email/notification services (SendGrid, Twilio)
- No cloud storage (AWS S3, Azure Blob)

This is a self-contained benchmarking system with no external API dependencies.

## Data Storage

**Databases:**
- SQLite - Local benchmark metrics storage
  - Connection: `better-sqlite3` in `Metrics/db_utils.js`
  - Location: `Metrics/benchmarks.db`
  - Schema: `Metrics/schema.sql`
  - Tables: `runs` with 19 columns tracking performance metrics

**File Storage:**
- Local file system only - No cloud storage
  - Matrices: `Matrices/*.matrix` (test inputs)
  - Logos: `logos/*.png` (cached language logos)
  - Screenshots: `screenshots/` (report captures)
  - Uploads: `server/uploads/` (temporary file uploads)

**Caching:**
- File-based caching only
  - `benchmark_history.json` - All historical runs
  - `session_state.json` - UI state persistence
  - `Languages/*/metrics.json` - Per-language results

## Authentication & Identity

**Auth Provider:**
- None - No authentication implemented
- Server runs without access controls
- Intended for local/internal use only

## Monitoring & Observability

**Error Tracking:**
- None (console.log/console.error only)

**Analytics:**
- None

**Logs:**
- stdout/stderr logging only
- No structured logging or log aggregation

## CI/CD & Deployment

**Hosting:**
- Docker container on local machine
- Port 9001 exposed via docker-compose
- No cloud deployment configured

**CI Pipeline:**
- Not detected - No GitHub Actions, Jenkins, etc.

## Environment Configuration

**Development:**
- Required env vars: `WEBHOST`, `WEBPORT` (optional)
- Secrets location: `.env` (minimal, non-sensitive)
- No mock services needed - all local execution

**Production:**
- Docker container with mounted volumes
- Environment: `NODE_ENV=development` (in docker-compose.yml)

## Internal API Endpoints

**REST API (port 9001):**
- `GET /` - Serves benchmark report HTML
- `GET /runner` - Matrix runner UI
- `GET /api/matrices` - List test matrices
- `GET /api/languages` - List available solvers
- `POST /api/run` - Execute solver benchmark
- `GET /api/metadata/:lang` - Language metadata
- `POST /api/save-metadata` - Update metadata
- `POST /api/lock` - Lock/unlock languages
- `POST /api/upload-media` - File upload (multer)
- `POST /api/download-media` - Fetch external media
- `POST /api/upload-logo` - Logo upload
- `POST /api/fetch-logo` - Fetch logo from URL
- `POST /api/generate-report` - Trigger report generation
- `GET /api/session-state` - Read session state
- `POST /api/session-state` - Save session state

## HTTP Client Usage

**External Image Fetching:**
- `node-fetch` used in `server/logo_processor.js`
- Purpose: Download language logos from URLs
- Method: `fetchLogoFromUrl()` - fetches and converts SVG to PNG
- No authentication required

## Webhooks & Callbacks

**Incoming:**
- None

**Outgoing:**
- None

## Child Process Execution

**Benchmark Execution:**
- Framework: Node.js `child_process.exec()`
- Purpose: Execute language solvers via shell scripts
- Scripts: `Languages/*/setupAndRunMe.sh`, `Languages/*/runMe.sh`
- Location: `server/index.js` (lines 145-170)
- Timeout: 180 seconds default

---

*Integration audit: 2026-01-07*
*Update when adding/removing external services*
