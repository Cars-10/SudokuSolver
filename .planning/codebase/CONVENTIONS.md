# Conventions

## Solver Implementation
- **Algorithm**: Standard recursive backtracking only (unless specified).
- **Iteration Count**: Must match C reference (656 for Matrix 1).
- **Output Format**:
  - Grid output (9x9)
  - `Solved in Iterations=X`
- **Scripting**: Each language directory should have a `setupAndRunMe.sh` or `runMe.sh`.

## Data Storage
- **History**: `benchmark_history.db` (SQLite) for long-term tracking.
- **Metadata**: `Algorithms/metadata.json` for language-specific details.

## Scripting
- Use absolute paths where possible or robust relative path resolution.
- Prefer `bash` over `sh` for orchestration.
