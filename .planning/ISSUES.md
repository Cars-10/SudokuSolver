# Project Issues Log

Enhancements discovered during execution. Not critical - address in future phases.

## Open Enhancements

### ISS-001: Expand SQLite schema to capture all 10 metrics

- **Discovered:** Phase 1 Task 2 (2025-12-24)
- **Type:** Data Integrity / Schema
- **Description:** The SQLite `runs` table only captures 4 metrics (time_seconds, memory_kb, cpu_user, cpu_sys). common.sh now captures 6 additional metrics: page_faults_major, page_faults_minor, context_switches_voluntary, context_switches_involuntary, io_inputs, io_outputs. Schema and db_utils.js need updating to store all 10 metrics.
- **Impact:** Low (metrics.json has all data; SQLite is incomplete)
- **Effort:** Quick (add 6 columns to schema, update INSERT statements)
- **Suggested phase:** Phase 2 or 3

**Files to modify:**
- `Metrics/schema.sql` - Add columns to runs table
- `Metrics/db_utils.js` - Update insertRun/insertRuns functions

## Closed Enhancements

[None yet]
