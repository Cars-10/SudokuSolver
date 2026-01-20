# Phase 2 Context: UI/UX Refinement

## Decisions

### 1. Server & Backend Removal
*   **Action:** Delete the `server/` directory entirely.
*   **Implication:** The benchmark report (`_report.html`) becomes a purely static file (or served via generic HTTP server).
*   **Feature Removal:** 
    *   Remove all "Run" / "Play" buttons from the dashboard grid.
    *   Remove `/api/run` calls from `report_client.js`.
    *   Remove `/api/save-metadata`, `/api/upload-logo` logic (or make them no-ops/alert users they are disabled in static mode).
    *   *Note:* The "Matrix Race" screensaver is client-side and will be preserved.

### 2. Fullscreen & Matrix Race
*   **Current State:** Matrix Race only exits on `Alt` key press. Clicks are explicitly ignored.
*   **New Behavior:**
    *   **Trigger:** Allow `Esc` key to exit (Standard UX).
    *   **Trigger:** Allow `Click` (anywhere) to exit after a 2-second grace period (to prevent accidental exits immediately after starting).
    *   **Visuals:** Add a subtle "Press ESC to Exit" overlay that fades out after 3 seconds.

### 3. Cleanup & Verification
*   **Files to Remove:**
    *   `server/` (Recursive)
    *   `check_invalid.mjs` (If unused/server-related)
    *   `debug_mismatches.js` (If unused)
*   **Code Cleanup:**
    *   `Metrics/HTMLGenerator.ts`: Force `staticMode = true` by default or remove the `!staticMode` checks (treating it as always static).
    *   `Metrics/report_client.js`: Remove fetch calls to missing APIs to prevent console errors.

## Technical Implementation Notes

### Matrix Race Exit Logic (`Metrics/report_client.js`)
Modify `resetTimer` function:
```javascript
// Allow ESC
if (e.key === 'Escape' || e.key === 'Esc') {
    stopScreensaver();
}
// Allow Click (if not ignoring input)
if (e.type === 'click' && !ignoreInput) {
    stopScreensaver();
}
```

### Static Report Generation
*   Ensure `Metrics/HTMLGenerator.ts` embeds all necessary data (it already appears to do so).
*   Verify `logoMap` logic handles local relative paths correctly without the server (it uses `Algorithms/BruteForce/...` which works with relative file paths).

## Success Criteria (Refined)
1.  `server/` directory does not exist.
2.  `_report.html` opens directly in a browser (file://) or via simple `python -m http.server` and renders correctly.
3.  Matrix Race starts via "Red Pill" and exits reliably via `Esc` or `Click`.
4.  No "Run" buttons are visible in the grid.
