# Concerns & Technical Debt

## Infrastructure
- **Docker:** The `Dockerfile` and build process need validation/rebuilding for the new server environment.
- **Dependency Hell:** Managing compilers/interpreters for 80+ languages is fragile.

## Code Quality
- **Broken UI:** The Content Server UI (modals, editing) is reported as partially broken.
- **Inconsistency:** Iteration counts in historical data (`benchmark_history.db`) may drift from current C reference.

## Architecture
- **Scalability:** `metrics.json` scattered across directories makes aggregation slow/brittle.
- **Concurrency:** `runMeGlobal.sh` is primarily sequential; parallel execution is ad-hoc.
