# Coding Conventions

**Analysis Date:** 2025-12-24

## Naming Patterns

**Files:**
- PascalCase.ts for class-like modules: `SolverRunner.ts`, `HTMLGenerator.ts`, `HistoryManager.ts`
- snake_case.ts for script-style files: `run_suite.ts`, `gather_metrics.ts`, `generate_report_only.ts`
- kebab-case.sh for shell scripts: `runBenchmarks.sh`, `setupAndRunMe.sh`
- UPPERCASE.md for important documents: `README.md`, `CLAUDE.md`, `ALGORITHM.md`

**Functions:**
- camelCase for all functions: `runSolver()`, `generateHtml()`, `loadReferenceIterations()`
- Async functions return `Promise<T>`: `async function runSolver(...): Promise<SolverMetrics | null>`
- Prefix patterns: `check_*` (validation), `run_*` (execution), `generate_*` (output), `validate_*` (checks)

**Variables:**
- camelCase for local variables: `solverDir`, `langName`, `parentDir`
- UPPER_SNAKE_CASE for shell constants: `LANGUAGE`, `SOLVER_BINARY`, `METRICS_FILE`
- camelCase for TypeScript constants: `compilerMapping`, `orderedLanguages`

**Types:**
- PascalCase for interfaces: `SolverMetrics`, `MetricResult`
- PascalCase for type aliases: `LanguageMetadata`, `PersonaConfig`
- No `I` prefix for interfaces

## Code Style

**Formatting:**
- 2-space indentation (consistent across TS, JS, shell)
- Single quotes for strings in JS/TS
- Semicolons required and present
- No automatic formatter configured (.prettierrc, .eslintrc not present)

**Linting:**
- No ESLint or Prettier configuration at project level
- TypeScript strict mode selective: `strict: true` in `Languages/TypeScript/`, `strict: false` in `Metrics/`
- Manual code style adherence

## Import Organization

**Order:**
1. Node.js built-ins (`import * as fs from 'fs/promises'`)
2. External packages (`import express from 'express'`)
3. Internal modules (`import { runSolver } from './SolverRunner'`)
4. Type imports last (`import type { SolverMetrics } from './types'`)

**Patterns:**
- Star imports for modules: `import * as fs from 'fs/promises'`
- Named imports for specific functions: `import { glob } from 'glob'`
- Type-only imports: `import type { MetricResult } from './types'`
- Re-exports for public API: `export * from './types.ts'`

**Path Aliases:**
- No path aliases configured
- Relative imports throughout: `./`, `../`

## Error Handling

**Patterns:**
- Try-catch at service boundaries
- Structured status codes: `success`, `timeout`, `error`, `env_error`
- Shell scripts use `report_env_error()` from `common.sh`

**Error Types:**
- Throw on invalid input, missing dependencies
- Return `null` on recoverable failures in `SolverRunner.ts`
- Shell: Exit codes 0 (success), 1 (error), signal handling

**Async Errors:**
```typescript
try {
  const result = await runSolver(...);
} catch (error) {
  console.error('Solver failed:', error);
  return null;
}
```

## Logging

**Framework:**
- console.log for normal output
- console.error for errors
- No structured logging library

**Patterns:**
- Extensive logging throughout codebase (1131+ statements in Metrics)
- No log level control
- Format: Plain text messages

## Comments

**When to Comment:**
- Algorithm explanations at file header
- Business logic annotations inline
- Step-by-step algorithm markers

**File Headers:**
```typescript
/**
 * Sudoku Solver - TypeScript Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * Algorithm:
 * - Row-major search for empty cells (top-to-bottom, left-to-right)
 * - Try values 1-9 in ascending order
 * - Count EVERY placement attempt (algorithm fingerprint)
 */
```

**JSDoc:**
```javascript
/**
 * Validate iteration count for a single matrix
 * @param {string|number} matrix - Matrix number (e.g., "1" or 1)
 * @param {number} actualIterations - Actual iteration count from solver
 * @returns {Object} Validation result with valid flag and details
 */
```

**TODO Comments:**
- Format: `// TODO: description`
- Located in code where deferred work is noted

## Function Design

**Size:**
- Most functions under 50 lines
- Exception: `HTMLGenerator.ts` has large generation functions

**Parameters:**
- Destructuring in parameter lists
- Options objects for complex configurations
- Explicit return types on async functions

**Return Values:**
- Explicit returns, no implicit undefined
- Union types for nullable: `SolverMetrics | null`
- Promise-wrapped for async

## Module Design

**Exports:**
- Named exports preferred
- Re-export patterns for public API
- No default exports in Metrics modules

**Module Pattern:**
- Singleton-like (imported as modules, not instantiated)
- Stateless functions with explicit dependencies
- File-based configuration loading

---

*Convention analysis: 2025-12-24*
*Update when patterns change*
