# Coding Conventions

**Analysis Date:** 2026-01-07

## Naming Patterns

**Files:**
- PascalCase.ts for TypeScript modules (`HTMLGenerator.ts`, `SolverRunner.ts`, `HistoryManager.ts`)
- snake_case.ts for multi-word scripts (`sync_db.ts`, `run_suite.ts`, `gather_metrics.ts`)
- lowercase.js for JavaScript files (`db_utils.js`, `report_client.js`)
- setupAndRunMe.sh for language entry scripts (camelCase)
- UPPERCASE.md for important docs (`MANIFESTO.md`, `README.md`)

**Functions:**
- camelCase for all functions (`generateHtml()`, `runSolver()`, `appendRecord()`)
- No special prefix for async functions
- Descriptive verb-noun naming (`insertRun()`, `getDatabase()`, `findSolvers()`)

**Variables:**
- camelCase for variables (`currentSort`, `searchInput`, `solverName`)
- UPPER_SNAKE_CASE for constants (`DB_PATH`, `MATRICES_DIR`)
- No underscore prefix for private (use TypeScript `private` keyword)

**Types:**
- PascalCase for interfaces (`MetricResult`, `SolverMetrics`)
- PascalCase for type aliases
- No `I` prefix on interfaces

## Code Style

**Formatting:**
- 4-space indentation (observed across TS/JS files)
- Single quotes for strings (`'utf8'`, `'node'`)
- Semicolons required at end of statements
- No linter/formatter config files (.eslintrc, .prettierrc not present)

**Line Length:**
- Generally 80-100 characters
- Long lines wrapped for readability

## Import Organization

**Order:**
1. Node.js builtins (`fs`, `path`, `child_process`)
2. Third-party packages (`express`, `better-sqlite3`)
3. Local modules (relative imports)
4. Type imports last (`import type { ... }`)

**Grouping:**
- Blank line between groups
- Type imports use `import type { }` syntax

**Example from `SolverRunner.ts`:**
```typescript
import * as path from 'path';
import * as util from 'util';
import { exec } from 'child_process';
import * as fs from 'fs/promises';
import type { SolverMetrics } from './types.ts';
```

**Path Aliases:**
- None configured
- Relative imports used (`./types.ts`, `../Metrics/`)

## Error Handling

**Patterns:**
- try/catch at function boundaries
- Errors logged to console.error
- Functions return error objects or throw

**Error Types:**
- Standard Error class used
- No custom error classes

**Example:**
```typescript
try {
  const result = await exec(command);
  return result;
} catch (error) {
  console.error('Execution failed:', error);
  throw error;
}
```

## Logging

**Framework:**
- Console.log/console.error (no logging library)
- Emoji markers for visual clarity

**Patterns:**
- `console.log('🔄 Starting...')` - Progress indicators
- `console.log('✅ Complete')` - Success markers
- `console.error('❌ Error:', err)` - Error markers

**When:**
- Log at function entry/exit for major operations
- Log errors with context
- Log state transitions

## Comments

**When to Comment:**
- Explain why, not what
- Document complex algorithms
- Mark non-obvious workarounds

**JSDoc/TSDoc:**
- Used for exported functions
- Format: `@param`, `@returns` tags

**Example from `db_utils.js`:**
```javascript
/**
 * Insert a benchmark run into the database
 * @param {Object} run - Run data object
 * @returns {Object} Insert result with lastInsertRowid
 */
export function insertRun(run) {
```

**TODO Comments:**
- Format: `// TODO: description`
- Used sparingly
- Example: `// Optional: Prune history if it gets too large?`

## Function Design

**Size:**
- Keep under 50 lines when possible
- Extract helpers for complex logic
- Some exceptions (HTMLGenerator has large functions)

**Parameters:**
- Max 3-4 parameters
- Use options object for more
- Destructure when appropriate

**Return Values:**
- Explicit return statements
- Return early for guard clauses
- Async functions return Promises

## Module Design

**Exports:**
- Named exports preferred
- `export async function`, `export const`
- Re-exports for backward compatibility

**Example from `LanguagesMetadata.ts`:**
```typescript
export {
    methodologyTexts,
    personalities
} from './PersonaMetadata.js';
```

**Barrel Files:**
- Not used (direct imports to specific modules)

## SQL Conventions

**Column Names:**
- snake_case: `time_ms`, `memory_bytes`, `cpu_user`, `cpu_sys`
- Full words preferred: `context_switches_voluntary`

**Queries:**
- Raw SQL via better-sqlite3
- Parameterized queries for user input

## Shell Script Conventions

**Shebang:**
- `#!/bin/bash` for bash scripts
- `#!/usr/bin/env node` for Node scripts

**Variables:**
- UPPER_CASE for environment/constants
- lowercase for local variables
- Quote variables in expansions: `"$VAR"`

**Functions:**
- Defined with `function_name() { }`
- Local variables with `local`

---

*Convention analysis: 2026-01-07*
*Update when patterns change*
