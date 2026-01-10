# Metrics Directory Knowledge

## Configuration
- `tsconfig.json` requires `"lib": ["ES2022", "DOM"]` because `ScreenshotUtils.ts` uses `window` in a Puppeteer context.

## Code Organization
- CSS for reports is centralized in `SharedStyles.ts` to ensure consistency between Single Run and History reports.
- `HTMLGenerator.ts` injects this CSS via template literals.
- `generateHistoryHtml` in `HTMLGenerator.ts` also uses `SharedStyles.ts` to maintain the Neon theme for the history report.
