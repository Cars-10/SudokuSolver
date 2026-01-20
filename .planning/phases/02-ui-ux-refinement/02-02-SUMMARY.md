---
wave: 2
depends_on: [02-01-PLAN.md]
files_modified:
  - Metrics/package.json
  - Metrics/HTMLGenerator.ts
  - Metrics/report_client.js
autonomous: true
status: Completed
---

<tasks>
  <task id="ui-fullscreen-01" title="Install screenfull" status="Completed">
    <description>
      Add `screenfull` to Metrics/package.json and install it.
    </description>
    <command>npm install screenfull --save --prefix Metrics</command>
    <verification>
      <cmd>grep "screenfull" Metrics/package.json</cmd>
    </verification>
  </task>

  <task id="ui-fullscreen-02" title="Inline screenfull in HTMLGenerator" status="Completed">
    <description>
      Update `generateHtml` in `Metrics/HTMLGenerator.ts` to read the `screenfull` library from node_modules and inline it into the generated HTML head (script tag).
      Use `require.resolve('screenfull')` or path join to locate `dist/screenfull.js` or `screenfull.min.js`.
    </description>
    <file_path>Metrics/HTMLGenerator.ts</file_path>
  </task>

  <task id="ui-fullscreen-03" title="Update Toggle Logic" status="Completed">
    <description>
      Update `window.toggleChartFullscreen` in `Metrics/HTMLGenerator.ts` to use `screenfull.toggle()`.
      Add logic to check `screenfull.isEnabled`.
      Add visual state indication (toggle class or button text) based on `screenfull.isFullscreen`.
    </description>
    <file_path>Metrics/HTMLGenerator.ts</file_path>
  </task>

  <task id="ui-fullscreen-04" title="Fix Re-entry Bug in Report Client" status="Completed">
    <description>
      In `Metrics/report_client.js`:
      1. Add a listener for `screenfull` change event (if available) or `fullscreenchange`.
      2. In the listener, check if we are NOT in fullscreen (`!screenfull.isFullscreen`).
      3. If not in fullscreen AND screensaver is active, call `stopScreensaver()`.
      4. Ensure `stopScreensaver` cleans up correctly.
    </description>
    <file_path>Metrics/report_client.js</file_path>
  </task>
</tasks>

<verification_criteria>
  - [x] `screenfull` library is included in the generated HTML.
  - [x] Fullscreen toggle uses `screenfull` API.
  - [x] Exiting fullscreen automatically stops the Matrix Race screensaver.
  - [x] Visual indicator shows active fullscreen state.
</verification_criteria>
