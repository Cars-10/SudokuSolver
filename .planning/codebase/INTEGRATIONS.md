# External Integrations

**Analysis Date:** 2025-12-24

## APIs & External Services

**Payment Processing:**
- Not detected

**Email/SMS:**
- Not detected

**External APIs:**
- Logo fetching - Downloads language logos from external URLs
  - GitHub profile avatars: `https://github.com/{username}.png`
  - Wikimedia images: `https://upload.wikimedia.org/`
  - Integration method: node-fetch (`server/index.js:343`)
  - No authentication required

**Google Fonts:**
- Font delivery service (Inter, JetBrains Mono)
- Integration: Preconnect link in `server/public/index.html`

## Data Storage

**Databases:**
- SQLite via better-sqlite3 11.0.0 - Optional metrics storage
  - Connection: Direct file access (`Metrics/benchmarks.db`)
  - Client: `Metrics/db_utils.js`
  - Schema: `runs` table with performance metrics

**File Storage:**
- Local file system - Primary storage mechanism
  - Metrics: `Languages/*/metrics.json`, `benchmark_history.json`
  - Configuration: `benchmark_config.json`, `.env`
  - Reports: `_report.html`, `benchmark_report.html`
  - Logos: `logos/` directory
  - Screenshots: `screenshots/` directory

**Caching:**
- None (all data read from files on each request)

## Authentication & Identity

**Auth Provider:**
- None - No authentication implemented
- API endpoints are public

**OAuth Integrations:**
- Not detected

## Monitoring & Observability

**Error Tracking:**
- None configured
- Errors logged to console

**Analytics:**
- None configured

**Logs:**
- Console output only (stdout/stderr)
- No log aggregation service

## CI/CD & Deployment

**Hosting:**
- Docker container - Ubuntu 24.04 with 80+ language toolchains
  - Image: `sudoku-benchmark:latest`
  - Port: 9001
  - Configuration: `docker-compose.yml`, `server/Dockerfile`

**CI Pipeline:**
- Not detected
- No GitHub Actions or similar

## Environment Configuration

**Development:**
- Required env vars: `WEBHOST`, `WEBPORT` (optional, defaults available)
- Secrets location: `.env` file (gitignored)
- No `.env.example` template

**Staging:**
- Not applicable (no staging environment)

**Production:**
- Docker deployment
- Volume mounts for source code, matrices, metrics, logos, screenshots

## Webhooks & Callbacks

**Incoming:**
- None detected

**Outgoing:**
- None detected

## Third-Party Tools

**Image Processing:**
- Puppeteer 24.31.0+ - Headless Chrome automation
  - Used for: Screenshot capture of HTML reports
  - Chrome detection: macOS (`/Applications/Google Chrome.app/...`)
  - Configuration: 1920x1080 resolution

- Sharp 0.33.5 - Image processing library
  - Used for: SVG to PNG conversion, logo transformations
  - Location: `server/logo_processor.js`
  - Features: Color inversion, transparent white conversion
  - Dependencies: librsvg2-dev (in Dockerfile)

## API Endpoints Exposed

**Benchmark Execution:**
- `POST /api/run` - Execute solver for language + matrix
- `POST /api/generate-report` - Trigger full report generation

**Data Retrieval:**
- `GET /api/matrices` - List available matrix files
- `GET /api/matrix/:filename` - Fetch matrix content
- `GET /api/languages` - List available language implementations
- `GET /api/metrics/:language` - Fetch metrics for specific language
- `GET /api/session-state` - Retrieve UI session state

**Media Management:**
- `POST /api/fetch-logo` - Download and process external logo
- `POST /api/upload-logo` - Handle multipart logo file upload
- `POST /api/save-metadata` - Persist language metadata

**Static Assets:**
- `GET /` - Serve generated benchmark report
- `GET /runner` - Serve Matrix Runner UI
- Static directories: `/logos`, `/Metrics`, `/js`, `/css`

## Docker Container Dependencies

From `server/Dockerfile` (Ubuntu 24.04):
- chromium-browser - For Puppeteer screenshot capture
- libvips-dev, librsvg2-dev - For image processing (Sharp)
- 80+ language toolchains installed

## Data Exchange Formats

**JSON:**
- `metrics.json` - Per-language benchmark results
- `benchmark_history.json` - Historical run data
- `benchmark_config.json` - Benchmark parameters
- `session_state.json` - UI state persistence
- `Languages/metadata.json` - Language definitions

**HTML:**
- `_report.html` - Self-contained benchmark report
- Embedded D3.js for interactive visualizations
- Embedded JSON data for client-side rendering

---

*Integration audit: 2025-12-24*
*Update when adding/removing external services*
