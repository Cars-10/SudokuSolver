---
wave: 1
depends_on: []
files_modified:
  - Metrics/HTMLGenerator.ts
autonomous: true
status: Completed
---

<tasks>
  <task id="ui-cleanup-01" title="Remove individual Run buttons" status="Completed">
    <description>
      Remove the "Run" button generation from the individual matrix cells in the HTML report.
      Keep the "Run All" button in the total column.
    </description>
    <file_path>Metrics/HTMLGenerator.ts</file_path>
    <verification>
      <cmd>grep "onclick=\"runSolver" Metrics/HTMLGenerator.ts | wc -l</cmd>
      <expected>0</expected> <!-- Should be 0 if we remove the individual ones, assuming runAllSolver uses a different function name or arguments. Actually runAllSolver is runAllSolver. runSolver is for single. -->
    </verification>
  </task>
</tasks>

<verification_criteria>
  - [x] Individual 'Run' buttons are removed from the generated HTML.
  - [x] 'Run All' button remains available.
  - [x] TypeScript compiles without errors.
</verification_criteria>

