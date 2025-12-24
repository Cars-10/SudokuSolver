# External Integrations

**Analysis Date:** 2025-12-24

## APIs & External Services

**Payment Processing:**
- Not applicable - No payment integration

**Email/SMS:**
- Not applicable - No email or messaging integration

**External APIs:**
- Not applicable - No external API consumption

**Note:** This is a self-contained benchmark system with no external service dependencies.

## Data Storage

**Databases:**
- SQLite (better-sqlite3 11.0.0) - Embedded relational database
  - Connection: Direct file access via better-sqlite3
  - Client: `better-sqlite3` npm package
  - Files: `Metrics/benchmarks.db`, `Metrics/db_utils.js`
  - Usage: Optional database storage for benchmark results

**File Storage:**
- Local file system only
  - Metrics: `Languages/*/metrics.json`
  - History: `benchmark_history.json`
  - Reports: `_report.html`, `_report_screenshot.html`
  - Uploads: `server/uploads/`

**Caching:**
- None - All operations read from disk

## Authentication & Identity

**Auth Provider:**
- None - No authentication system
- Open API without access control

**OAuth Integrations:**
- None

## Monitoring & Observability

**Error Tracking:**
- Console logging only
- No Sentry, Datadog, or similar

**Analytics:**
- None

**Logs:**
- stdout/stderr to console
- No centralized logging

## CI/CD & Deployment

**Hosting:**
- Docker container (Ubuntu 24.04 base)
- Port 9001 for API
- Volume mounts for persistent data

**CI Pipeline:**
- Not configured
- Manual validation via `validate_run.js`

## Environment Configuration

**Development:**
- Required env vars: `WEBHOST`, `WEBPORT` (in `.env`)
- No secrets management (no external services)
- Docker optional for full language support

**Production:**
- Same as development
- Docker recommended for all 80+ languages
- Environment vars in `docker-compose.yml`

## Webhooks & Callbacks

**Incoming:**
- None

**Outgoing:**
- None

## Browser Automation

**Puppeteer:**
- Used for: Screenshot capture of HTML reports
- Configuration: `Metrics/ScreenshotUtils.ts`
- Environment: `PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium-browser`
- Chrome path: `/Applications/Google Chrome.app/...` (macOS dev)
- Viewport: 1920x1080 for consistent rendering

## Image Processing

**Sharp:**
- Used for: Logo processing and transformation
- Location: `server/logo_processor.js`
- Features:
  - SVG to PNG conversion
  - Color inversion for logos
  - White-to-transparent transformation
  - Resize with background specification

**svg2png-wasm:**
- Used for: Alternative SVG conversion
- Location: `server/package.json`

## Process Execution

**child_process:**
- Used for: Running language-specific solvers
- Location: `server/index.js` (lines 152, 434)
- Timeout: 120-180 seconds
- Working directory: Language-specific folders

## HTTP Client

**node-fetch:**
- Used for: Downloading language logos from URLs
- Location: `server/index.js`, `server/logo_processor.js`
- Endpoint: `/api/download-media`

---

*Integration audit: 2025-12-24*
*Update when adding/removing external services*

**Summary:** This codebase has NO external API integrations, NO authentication systems, and NO third-party SaaS dependencies. It is a self-contained polyglot benchmark system using only local file I/O, local process execution, local SQLite database, and local browser automation via Puppeteer.
