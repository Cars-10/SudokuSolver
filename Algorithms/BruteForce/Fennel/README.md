# Fennel Sudoku Solver

Status: **DEFERRED**

The Fennel language solver is currently deferred due to persistent parsing errors encountered during development. Despite multiple attempts to correct parenthesis mismatches and structural issues, the Fennel compiler (`fennel`) consistently reported syntax errors (`unexpected closing delimiter )`, `expected body`) that prevented successful compilation and execution.

## Reason for Deferral

- **Parsing Complexity**: The Lisp-like syntax of Fennel, combined with its compilation to Lua, proved challenging to debug within the given time constraints, especially concerning proper parenthesis matching and macro expansion.
- **Development Environment**: Debugging Fennel parsing errors requires deep familiarity with its compiler and error reporting, which was difficult to achieve rapidly.

## Future Work

- Revisit Fennel implementation with a dedicated focus on understanding its parsing rules and macro system more thoroughly.
- Test small, isolated Fennel snippets directly to gain better insight into its syntax requirements.
- Consult Fennel documentation and community resources for common parsing pitfalls.
- If persistent parsing issues continue, consider deferring permanently or seeking an alternative Lua-based Lisp.