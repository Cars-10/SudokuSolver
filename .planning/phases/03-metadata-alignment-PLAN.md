# Phase 3 Plan: Metadata Alignment & UI Polish

## Goal
Establish a single source of truth for media, implement a rich "Glassmorphism" UI for language details, and add an admin interface for the scoring engine.

## Step 1: Media Consolidation & Migration
*   **Audit:** Map `logos/` filenames to `Algorithms/BruteForce/{Lang}/` directory names.
*   **Migrate:** Create and run a script (`scripts/migrate_logos.js`) to:
    1.  Move matching files from `logos/` to `Algorithms/BruteForce/{Lang}/Media/`.
    2.  Ensure no duplicates (prefer existing `Media/` files if collision).
    3.  Delete the `logos/` directory after successful verification.
*   **Metadata:** Update `metadata.json` generation logic (if dynamic) or run a script to update the `image` paths in the JSON to point to the new relative locations.

## Step 2: Backend Updates (Server)
*   **Static Serving:** Update `server/index.js` to serve the `Algorithms/BruteForce` directory (or specific `Media` subdirectories) under a route like `/media`.
    *   *Constraint:* Ensure it handles URL encoding for special characters (C++, C#, etc.).
*   **Config API:** Implement endpoints for the Scoring Engine:
    *   `GET /api/config`: Read `benchmark_config.json`.
    *   `POST /api/config`: Write updates to `benchmark_config.json`.

## Step 3: Frontend - Components & visual Polish
*   **NeonTooltip:** Create a reusable tooltip component with Neon styling (glow, dark bg).
    *   *Usage:* Hover over language names in the main table.
    *   *Content:* Display the "Concise Description" (from `benefits` or `description`).
*   **GlassModal:** Create a new modal component with:
    *   Width: 600px.
    *   Background: Blurred backdrop (Glassmorphism).
    *   Header: Full-width banner using the language logo/art.
*   **Language Detail View:** Implement the internal layout of the modal:
    *   **Top:** Two-column grid (Left: Creator/Date, Right: Paradigm/Type System).
    *   **Middle:** Scrollable areas for "History" and "Benefits".
    *   **Bottom:** Author Image Gallery (handling single or multiple authors).

## Step 4: Frontend - Admin UI
*   **Scoring Modal:** Update the existing Scoring/Methodology modal.
*   **Edit Mode:** Add input fields for weighting parameters.
*   **Persistence:** Wire up the "Save" button to the `POST /api/config` endpoint.

## Step 5: Verification
*   **Visual Check:** Verify all language logos load correctly from their new paths.
*   **Interaction Check:** Verify Tooltip appears on hover and Modal opens on click.
*   **Functional Check:** Verify scoring weights can be changed and persist after server restart.