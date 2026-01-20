# Research Summary

## Stack
- **Math**: `simple-statistics` or `mathjs` for weighted scoring.
- **UI**: `screenfull` for robust fullscreen handling.
- **Viz**: Continue using D3.js.

## Features
- **Scoring**: Geometric Mean of Time/Memory/CPU ratios (normalized to C).
- **UI**: Sortable tables, interactive charts, simplified view modes.

## Architecture
- **Calculation**: Scores computed at build time in `HTMLGenerator.ts`.
- **Display**: Client-side visualization in `report_client.js`.
- **Data**: Centralized history in SQLite/JSON.

## Pitfalls
- **Inconsistency**: Code vs. Docs mismatch on scoring formulas.
- **Bias**: Peak RSS unfairly penalizes managed runtimes.
- **Clutter**: Overloaded UI hides key controls.

