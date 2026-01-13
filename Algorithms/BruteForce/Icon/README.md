# Icon Sudoku Solver

Status: **DEFERRED**

The Icon language solver is currently deferred due to persistent issues with its execution environment and debugging capabilities within the Docker container. Debug statements for file operations (opening, reading, writing to standard output/error) did not produce any visible output, making it impossible to diagnose parsing and runtime errors effectively.

## Reason for Deferral

- **Debugging Challenges**: Lack of visible output from debug statements (`write`, `stop`) makes it extremely difficult to trace program flow and identify errors in file I/O or other operations.
- **Execution Environment**: Unclear interaction with standard input/output streams within the Docker `bash -c` execution context.

## Future Work

- Investigate alternative methods for capturing Icon runtime output (e.g., writing to a file and inspecting it).
- Explore running Icon interactively or with a more verbose logging setup to understand its execution model better.
- If debugging remains intractable, consider deferring permanently or seeking an alternative Icon environment.