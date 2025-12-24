# Coding Conventions

**Analysis Date:** 2025-12-24

## Naming Patterns

**Files:**
- PascalCase for service classes: `HTMLGenerator.ts`, `SolverRunner.ts`, `HistoryManager.ts`
- snake_case for scripts/utilities: `db_utils.js`, `gather_metrics.ts`, `run_suite.ts`
- lowercase for data: `metrics.json`, `benchmark_config.json`
- UPPERCASE for docs: `MANIFESTO.md`, `README.md`

**Functions:**
- camelCase for all functions: `appendRecord()`, `getHistory()`, `runSolver()`
- No special prefix for async functions
- Descriptive verbs: `readReferenceOutputs()`, `findSolvers()`, `captureScreenshot()`

**Variables:**
- camelCase for variables: `allMetrics`, `historyFile`, `solverDir`, `langName`
- UPPER_SNAKE_CASE for directory constants: `MATRICES_DIR`, `LANGUAGES_DIR`
- Descriptive names preferred over abbreviations

**Types:**
- PascalCase for interfaces: `MetricResult`, `SolverMetrics`
- No `I` prefix (use `User`, not `IUser`)
- Type aliases in PascalCase: `Record<string, string>`

## Code Style

**Formatting:**
- 4 space indentation
- Single quotes for strings
- Semicolons required
- No line length limit (but maintain readability)

**Linting:**
- No ESLint configured
- No Prettier configured
- Manual formatting consistency

## Import Organization

**Order:**
1. Node.js built-ins (`import * as fs from 'fs/promises'`)
2. External packages (`import { glob } from 'glob'`)
3. Internal modules (`import { SolverMetrics } from './types.ts'`)
4. Type imports last (`import type { MetricResult } from './types.ts'`)

**Grouping:**
- Blank line between groups
- Star imports for fs/path: `import * as fs from 'fs/promises'`
- Named imports for specific exports: `import { glob } from 'glob'`

**Path Aliases:**
- None configured
- Relative paths used: `./types.ts`, `../Metrics/`

## Error Handling

**Patterns:**
- try/catch at function boundaries
- Error messages include context
- Status codes for solver results: `'env_error'`, `'error'`, success states

**Error Types:**
- Throw on file system failures
- Return empty array on missing data (HistoryManager)
- Log errors before throwing

**Example:**
```typescript
try {
    const data = await fs.readFile(filePath, 'utf-8');
    return JSON.parse(data);
} catch (e: any) {
    console.error(`Failed to read ${filePath}: ${e.message}`);
    return [];
}
```

## Logging

**Framework:**
- console.log for normal output
- console.error for errors
- No structured logging (pino/winston not used)

**Patterns:**
- Log at service boundaries
- Include relevant context in messages
- Debug logs guarded by `process.env.DEBUG`

## Comments

**When to Comment:**
- File headers explaining module purpose
- Complex logic that isn't self-evident
- Section markers for large files

**JSDoc/TSDoc:**
- Used in JavaScript utilities (`db_utils.js`)
- Optional for TypeScript (interfaces are self-documenting)
- Block comments for function documentation

**Section Comments:**
```typescript
// --- Master Language List (Ordered by Performance/Tier) ---
// --- Language Quotes (short descriptions per language) ---
// --- Language Histories (historical context per language) ---
```

**TODO Comments:**
- Format: `// TODO: description` or `// FIXME: description`
- No username tracking (use git blame)

## Function Design

**Size:**
- No strict limit, but extract helpers for complex logic
- HTMLGenerator.generateHtml() is notably large (needs refactoring)

**Parameters:**
- Use options object for 4+ parameters
- Destructure in parameter list when appropriate
- Default values in function signature

**Return Values:**
- Explicit return statements
- Return early for guard clauses
- Consistent return types (array returns `[]` not `null`)

## Module Design

**Exports:**
- Named exports preferred: `export function runSolver()`
- Re-exports for backward compatibility: `export { personalities } from './PersonaMetadata'`
- Default exports rarely used

**Barrel Files:**
- `gather_metrics.ts` re-exports public API
- Internal helpers kept private (not exported from barrel)

**Type Exports:**
- Types in dedicated `types.ts` file
- Import with `import type { X }` for type-only imports

## TypeScript Configuration

**Key Settings (from `Metrics/tsconfig.json`):**
- target: ES2022
- module: ES2022
- strict: false (not strict mode)
- esModuleInterop: true
- resolveJsonModule: true
- allowImportingTsExtensions: true

## Git Conventions

**Commit Format:** `<Type>: <Subject>`

**Types:**
- `UI:` - User interface changes
- `Feature:` - New features
- `Fix:` - Bug fixes
- `Phase X:` - Phase completion
- `docs:` - Documentation updates

**Examples:**
- `UI: Unify modal logos with table logos`
- `Feature: Add tier legend to benchmark report`
- `Fix: Add special logo lookup handling for C# and F#`

---

*Convention analysis: 2025-12-24*
*Update when patterns change*
