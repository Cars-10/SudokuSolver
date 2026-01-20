# Phase 2 Research: UI/UX Refinement

## Context
This phase focuses on polishing the user interface, specifically removing clutter ("Run" buttons), stabilizing the fullscreen experience using `screenfull`, and fixing a specific bug where the Matrix Race screensaver re-enters fullscreen immediately after exiting.

## Findings

### 1. Remove Individual "Run" Buttons (UI-01)
*   **Location**: `Metrics/HTMLGenerator.ts`
*   **Current State**: Lines ~628 generate a `<button class="run-btn" ...>⏵</button>` for each matrix cell.
*   **Action**: Remove this button generation. Keep the "Run All" button in the total column (line ~656) as it provides sufficient functionality without clutter.

### 2. Implement `screenfull` (UI-02)
*   **Location**: `Metrics/HTMLGenerator.ts` (inline script) and `Metrics/package.json`.
*   **Current State**: Uses raw `elem.requestFullscreen()` and `document.exitFullscreen()` with vendor prefix handling missing or manual.
*   **Action**:
    1.  Add `screenfull` to `Metrics/package.json`.
    2.  Import/include `screenfull` in the generated HTML (or bundle it). Since `HTMLGenerator.ts` generates a single HTML file, we might need to use a CDN link or read the library content and inline it, similar to how `report_client.js` is handled.
    3.  Update `window.toggleChartFullscreen` to use `screenfull.toggle()`.

### 3. Fix Re-entry Bug (UI-03)
*   **Issue**: When exiting fullscreen (e.g., via ESC), the "Matrix Race" (screensaver) might remain active or re-trigger.
*   **Analysis**:
    *   In `report_client.js` (inlined in HTML), `stopScreensaver` sets `active = false`.
    *   `resetTimer` calls `stopScreensaver` on interaction (mousemove, keydown, etc.).
    *   However, if the user exits fullscreen via ESC (browser native action), `resetTimer` might not trigger `stopScreensaver` immediately, or the `fullscreenchange` event listener in `HTMLGenerator.ts` (line 342) might interfere.
    *   Crucially, `report_client.js` does *not* listen to `fullscreenchange` to call `stopScreensaver`.
*   **Action**: Add a `screenfull.on('change', ...)` or `document.addEventListener('fullscreenchange', ...)` listener in `report_client.js` (inside the Matrix Screensaver closure or globally) that checks: `if (!screenfull.isFullscreen && active) stopScreensaver();`.

### 4. Fullscreen State Indication (UI-04)
*   **Location**: `Metrics/HTMLGenerator.ts` / `Metrics/index.css`.
*   **Action**:
    *   Add visual feedback (e.g., icon change, toast, or class on the button) when fullscreen is active.
    *   `screenfull` provides state checking.

## Plan
1.  **Dependencies**: Install `screenfull` in `Metrics/`.
2.  **HTMLGenerator**:
    *   Remove cell run buttons.
    *   Inject `screenfull` (CDN or inline).
    *   Refactor `toggleChartFullscreen`.
3.  **Report Client**:
    *   Update screensaver logic to listen for fullscreen exit events and call `stopScreensaver`.

## Questions
*   **Inlining `screenfull`**: Since the report is a single HTML file, we should probably inline the script or use a reliable CDN (unpkg). `report_client.js` is read and inlined. We can do the same for `screenfull` if we install it, or just use a CDN link in the `<head>`. Given "Internet-free" robustness is nice, inlining `node_modules/screenfull/dist/screenfull.js` is preferred.

