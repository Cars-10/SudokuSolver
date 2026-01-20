# Phase 3 Context: Metadata Alignment & UI Polish

## Decisions & Direction

### 1. Scope Expansion: Admin UI & Rich Language Modals
This phase now includes:
-   **Admin UI:** For configuring scoring weights (persisted to `benchmark_config.json`).
-   **Rich Language Modals:** A "fancy" and "compact" visual presentation of language metadata.

### 2. Implementation Decisions

#### A. Media & Server Architecture
-   **Single Source of Truth:** All media assets (logos, author photos) will reside in `Algorithms/BruteForce/{Lang}/Media/`.
-   **Migration:** Any logos currently in the root `logos/` folder must be moved to their respective language's `Media/` folder. The `logos/` directory will be deleted.
-   **Server Route:** The Node.js server will expose a route (e.g., `/media/:lang/:filename`) to serve these assets dynamically.
-   **Auto-Discovery:** A script (or server logic) will scan these folders to populate metadata references automatically.

#### B. UI/UX: Language Modal
-   **Dimensions:** ~600px wide (wider than previous default).
-   **Visual Style:** "Glassmorphism" (blurred background) with Neon accents.
-   **Layout:**
    -   **Header:** Banner style (using the language logo/art).
    -   **Top Section:** Two-column grid for key fields.
        -   *Left:* Creator, Date.
        -   *Right:* Paradigm, Type System.
    -   **Body:** Scrollable text areas for "History" and "Benefits".
    -   **Gallery:** Display multiple author images if available (e.g., Awk).
-   **Hover Effect:** Custom Neon-styled tooltip showing a concise description (derived from `benefits` or `history`).

#### C. Documentation & Methodology
-   **Content:** "Simple English" explanations of the scoring algorithm.
-   **Location:** A dedicated "Methodology" tab or modal, accessible from the UI.
-   **Terminology:** Use "Score" instead of complex math jargon.

## Technical Strategy
1.  **Migration Script:** Move files from `logos/` to `Algorithms/BruteForce/...`, then delete `logos/`.
2.  **Server Update:** Add static file serving for `Algorithms/BruteForce`.
3.  **Frontend:**
    -   Implement `GlassModal` component.
    -   Implement `NeonTooltip` component.
    -   Update `ScoringModal` to support weight editing.
    -   Refactor language cell interactions to trigger the new modal.

## Open Questions (Resolved)
-   *Media Source?* `Algorithms/BruteForce/{Lang}/Media` is the authority.
-   *Modal Style?* Blurred/Glass, 600px width.
-   *Author Display?* Gallery view for multiple authors.
-   *Admin Persistence?* Yes, save to config file.