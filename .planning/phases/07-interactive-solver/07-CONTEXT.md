# Phase 7: Interactive Solver - Context

**Gathered:** 2026-01-24
**Status:** Ready for planning

<domain>
## Phase Boundary

Provide engaging, visually entertaining solver animation with Neon/Matrix theme styling for interactive user exploration. Users can select matrix and algorithm to run interactive solver in browser using JavaScript implementation, with visually entertaining animation featuring alien glitch effects and letter spinning during backtracking.

</domain>

<decisions>
## Implementation Decisions

### Animation behavior
- **Speed control**: Continuous speed slider (1x - 100x) with preset markers at 1x, 10x, 50x, 100x, Max
- **Manual stepping**: Full step forward/backward buttons - users can examine each iteration with rewind capability
- **Backtracking visualization**: Red/warning color + reverse animation - clear visual distinction with red cells and reverse spin direction
- **Live information display**: Show all four metrics during animation:
  - Iteration count (live updating)
  - Current cell being tried (highlighted row/col coordinates)
  - Candidate number (which 1-9 is being tested)
  - Backtrack depth (recursion stack depth indicator)

### Visual styling
- **Theme approach**: Neon Matrix hybrid - colorful glows on dark background with neon colors (cyan, magenta, green) instead of just classic Matrix green
- **Color scheme**: Multi-color state-based - blue for trying, green for success, red for backtrack, etc.
- **Alien glitch effect**: Combination of effects triggered at different moments:
  - Screen shake or color inversion on backtrack
  - Numbers briefly show as alien symbols during intense solving
  - CRT-style distortion or chromatic aberration during fast iterations
- **Grid depth**: 3D with perspective - cells have depth, tilt slightly, numbers appear to float

### User controls
- **UI location**: New dedicated tab/section - add 'Interactive Solver' as separate section in report
- **Selection interface**: Dropdowns like current chart selector - separate dropdowns for matrix (1-6) and algorithm (BruteForce/DLX)
- **Playback controls**: All four controls available:
  - Play/Pause button
  - Reset button (return to initial puzzle)
  - Skip to end (jump to solved state)
  - Speed slider (1x-100x as discussed)
- **Mobile adaptation**: Simplified mobile version - fewer controls on mobile (just play/pause and speed), full controls on desktop

### Letter spinning
- **Individual cell spins**: Only the active cell being modified spins its number (not row/column cascade or synchronized grid)
- **Spin direction**: Clockwise for forward progress, counter-clockwise for backtrack - direction conveys information about algorithm state

### Claude's Discretion
- Exact spin trigger timing (when to activate the spin - every backtrack vs major backtracks vs high-speed moments)
- Spin duration/speed (how fast the rotation happens - somewhere between 0.3-2 seconds, can vary with playback speed)
- Specific glitch effect implementation details (exact timing, intensity, combinations)
- 3D perspective angle and depth amount
- Exact glow/bloom intensity for neon effects
- Control panel layout and styling specifics

</decisions>

<specifics>
## Specific Ideas

- Animation should feel like watching the algorithm "think" - users should see the search process unfold
- Backtracking should be visually dramatic (red + reverse spin) to make the search strategy obvious
- The glitch effects should feel "alien" - like the solver is a foreign intelligence working on the puzzle
- 3D perspective gives depth and makes it feel more like a Matrix-style digital environment
- Step controls let users examine specific iterations - educational aspect

</specifics>

<deferred>
## Deferred Ideas

None - discussion stayed within phase scope

</deferred>

---

*Phase: 07-interactive-solver*
*Context gathered: 2026-01-24*
