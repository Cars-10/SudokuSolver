# PRD: Metadata Enrichment & Wikipedia Extraction

## Introduction
The Sudoku Solver project aims to provide a rich, visually engaging comparison of programming languages. Currently, the metadata (descriptions, history, authors) is manually maintained and incomplete. This feature aims to automate the extraction of this data from Wikipedia, including fetching author images, to populate `LanguagesMetadata.ts` and the respective language `Media` folders.

## Goals
1.  **Automate Metadata Collection:** Reduce manual effort in maintaining language facts.
2.  **Enrich UI:** Provide author images and detailed histories for a "Red Pill" aesthetic depth.
3.  **Standardize:** Ensure all languages have a baseline level of information.

## User Stories

### Story 1: Python Scraper Setup
**As a** developer,
**I want** a Python script utilizing the `wikipedia` library,
**So that** I can fetch standardized data for any given programming language.

**Acceptance Criteria:**
- Script `scripts/fetch_metadata.py` exists.
- `pip install wikipedia` dependency is documented/handled.
- Script accepts a language name or iterates through a configured list.

### Story 2: Data Extraction & Image Handling
**As a** benchmark runner,
**I want** the script to download author images and text metadata,
**So that** the data is available locally.

**Acceptance Criteria:**
- Fetches: Description/Summary, Author Name(s), Creation Date, Paradigm.
- Downloads author images to `Algorithms/BruteForce/<Lang>/Media/<LastName>.<ext>`.
- **CRITICAL:** Does not overwrite `*_logo.*` files.
- **CRITICAL:** Deletes images in `Media/` that are *not* the logo and *not* referenced in the new metadata (cleanup).
- Handles multiple authors (e.g., Go: Griesemer, Pike, Thompson).

### Story 3: TypeScript Integration
**As a** frontend developer,
**I want** the script to output or update `Metrics/LanguagesMetadata.ts`,
**So that** the UI reflects the new data immediately.

**Acceptance Criteria:**
- Output format matches the existing `LanguageMeta` interface in `LanguagesMetadata.ts`.
- Updates `languageMetadata` object with new entries.
- Preserves existing manual overrides if flagged (or simply overwrites - assuming overwrite for now based on "very complete metadata" request).

## Functional Requirements
1.  **Input:** Read `orderedLanguages` from `Metrics/LanguagesMetadata.ts` (or a mirrored list in the python script) to know which languages to process.
2.  **Processing:**
    - Search Wikipedia for `[Language Name] (programming language)`.
    - Extract Infobox data if possible, or parse summary text.
    - Identify authors.
    - Find author images on Wikipedia/Wikimedia.
3.  **Output:**
    - JSON dump or direct TS file modification.
    - File system changes in `Algorithms/BruteForce/<Lang>/Media/`.

## Non-Goals
- Real-time fetching during the benchmark run (this is a build-time/maintenance task).
- OCR or complex image processing.

## Considerations
- **Rate Limiting:** Wikipedia API has limits; script should be polite.
- **Ambiguity:** "Go" vs "Go (game)" vs "Go (programming language)".
- **Missing Data:** Fallback to existing data or placeholders if Wikipedia fails.
