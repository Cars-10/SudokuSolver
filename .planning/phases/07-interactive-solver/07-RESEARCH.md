# Phase 7: Interactive Solver - Research

**Researched:** 2026-01-24
**Domain:** Browser-based Sudoku solver animation with 3D CSS effects and interactive controls
**Confidence:** HIGH

## Summary

Interactive Sudoku solver visualization is a well-established domain with extensive prior art on GitHub and clear technical patterns. The core technical stack involves CSS 3D transforms for visual depth, requestAnimationFrame for smooth animation timing, and state history arrays for step-by-step navigation.

The standard approach uses pure CSS for 3D effects (transform, perspective, rotateY) combined with JavaScript animation control via requestAnimationFrame. For the Neon/Matrix aesthetic, multiple stacked text-shadow/box-shadow layers create glow effects. The critical insight is that GPU-accelerated animations require careful memory management—each composite layer consumes significant RAM (307KB for a 320×240 layer in RGBa format).

For step-by-step animation with rewind capability, the proven pattern is maintaining an immutable history array of solver states, with a position pointer that increments/decrements during playback. This allows instant forward/backward navigation without re-running the algorithm.

**Primary recommendation:** Use CSS-only approach for grid rendering and animations (not Canvas) with transform/opacity GPU-accelerated properties, maintain immutable state history array for step controls, and apply strict memory limits on composite layers for mobile compatibility.

## Standard Stack

The established libraries/tools for browser-based algorithm visualization:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| requestAnimationFrame | Browser API | Smooth animation timing | Syncs with display refresh rate (60Hz/120Hz), pauses in background tabs for battery life |
| CSS 3D Transforms | CSS3 | 3D perspective grid | Hardware-accelerated, widely supported since 2015, no library needed |
| CSS text-shadow | CSS3 | Neon glow effects | Native browser support, multiple shadow stacking for glow intensity |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| Range Input (native) | HTML5 | Speed control slider | Native browser control, full customization via CSS pseudo-elements |
| will-change | CSS | GPU compositing hint | Sparingly—only on actively animating elements to avoid memory waste |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| CSS Grid | Canvas 2D | Canvas faster for 1000+ elements but CSS better for 81 cells with GPU acceleration and easier styling |
| Pure CSS | WebGL CRT Filter | WebGL provides realistic CRT effects but adds 50KB+ library weight and complexity |
| State Array | Redux/XState | State machines add overhead for simple linear history (forward/backward navigation) |

**Installation:**
```bash
# No npm packages required - uses browser native APIs
# Existing project already has alien character set in screensaver.js
```

## Architecture Patterns

### Recommended Project Structure
```
Metrics/modules/
├── interactive-solver.js    # Main solver animation controller
├── solver-state.js          # State history management (immutable array)
├── solver-ui.js             # Playback controls and speed slider
└── solver-grid.js           # 3D grid rendering and cell animations
```

### Pattern 1: State History for Step Navigation
**What:** Immutable array of solver states with position pointer for instant forward/backward navigation
**When to use:** Algorithm visualization with rewind/step controls
**Example:**
```javascript
// Source: https://gist.github.com/sag1v/86dabe5f5ca9e3ca28ea39c374d51c9f (verified pattern)
class SolverHistory {
  constructor() {
    this.states = [];      // [{grid, row, col, val, iteration, depth}]
    this.position = -1;    // Current state index
  }

  push(state) {
    // When moving forward, discard future states
    this.states.length = this.position + 1;
    this.states.push(Object.freeze(state)); // Immutable
    this.position++;
  }

  canStepBack() { return this.position > 0; }
  canStepForward() { return this.position < this.states.length - 1; }

  stepBack() {
    if (this.canStepBack()) this.position--;
    return this.states[this.position];
  }

  stepForward() {
    if (this.canStepForward()) this.position++;
    return this.states[this.position];
  }

  getCurrentState() {
    return this.states[this.position];
  }
}
```

### Pattern 2: requestAnimationFrame with Speed Control
**What:** Delta-time based animation loop that scales speed without FPS dependency
**When to use:** All smooth animations that must adapt to different refresh rates (60Hz/120Hz/144Hz)
**Example:**
```javascript
// Source: MDN https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame
class AnimationController {
  constructor(history, onRender) {
    this.history = history;
    this.onRender = onRender;
    this.playing = false;
    this.speed = 1.0;  // 1x-100x multiplier
    this.lastTimestamp = null;
    this.stepInterval = 100; // ms between steps at 1x speed
    this.accumulated = 0;
  }

  start() {
    this.playing = true;
    this.lastTimestamp = null;
    requestAnimationFrame((t) => this.tick(t));
  }

  stop() {
    this.playing = false;
  }

  tick(timestamp) {
    if (!this.playing) return;

    if (this.lastTimestamp === null) {
      this.lastTimestamp = timestamp;
    }

    // Delta time in ms, scaled by speed multiplier
    const delta = (timestamp - this.lastTimestamp) * this.speed;
    this.lastTimestamp = timestamp;
    this.accumulated += delta;

    // Step forward when accumulated time exceeds interval
    while (this.accumulated >= this.stepInterval) {
      if (this.history.canStepForward()) {
        const state = this.history.stepForward();
        this.onRender(state);
      } else {
        this.playing = false;
        return;
      }
      this.accumulated -= this.stepInterval;
    }

    requestAnimationFrame((t) => this.tick(t));
  }

  setSpeed(multiplier) {
    this.speed = Math.max(1, Math.min(100, multiplier));
  }
}
```

### Pattern 3: CSS 3D Grid with Perspective
**What:** Parent container provides perspective, child cells use transform: rotateY() for spin animations
**When to use:** Creating depth effect for grid-based visualizations
**Example:**
```css
/* Source: MDN https://developer.mozilla.org/en-US/docs/Web/CSS/Guides/Transforms/Using */
.solver-grid {
  perspective: 1000px; /* Parent defines 3D space */
  perspective-origin: center center;
  display: grid;
  grid-template-columns: repeat(9, 1fr);
  gap: 2px;
}

.solver-cell {
  transform-style: preserve-3d; /* Enable 3D for children */
  transition: transform 0.3s ease-in-out;
  backface-visibility: hidden; /* Hide back during rotation */
}

.solver-cell.spinning {
  animation: spinY 0.6s ease-in-out;
}

@keyframes spinY {
  0% { transform: rotateY(0deg); }
  50% { transform: rotateY(90deg); } /* Number hidden at 90deg */
  100% { transform: rotateY(0deg); }
}

/* Reverse for backtracking */
.solver-cell.spinning-reverse {
  animation: spinY-reverse 0.6s ease-in-out;
}

@keyframes spinY-reverse {
  0% { transform: rotateY(0deg); }
  50% { transform: rotateY(-90deg); } /* Counter-clockwise */
  100% { transform: rotateY(0deg); }
}
```

### Pattern 4: Neon Glow Effects
**What:** Multiple stacked shadows with increasing blur radius for neon glow
**When to use:** Matrix/Neon theme visual styling
**Example:**
```css
/* Source: https://css-tricks.com/how-to-create-neon-text-with-css/ */
.neon-number {
  color: #fff;
  text-shadow:
    0 0 5px #fff,
    0 0 10px #fff,
    0 0 20px #0ff,      /* Cyan glow */
    0 0 40px #0ff,
    0 0 80px #0ff;
}

.neon-success {
  color: #0f0;
  text-shadow:
    0 0 5px #fff,
    0 0 10px #0f0,      /* Green glow */
    0 0 20px #0f0,
    0 0 40px #0f0;
}

.neon-backtrack {
  color: #f00;
  text-shadow:
    0 0 5px #fff,
    0 0 10px #f00,      /* Red glow */
    0 0 20px #f00,
    0 0 40px #f00;
}

.neon-box {
  border: 2px solid #0ff;
  box-shadow:
    0 0 5px #fff,
    0 0 10px #0ff,
    inset 0 0 10px #0ff;  /* Inner glow */
}
```

### Pattern 5: Glitch Effects
**What:** CSS filters and transforms for chromatic aberration and screen shake
**When to use:** Visual feedback during backtracking or high-speed solving
**Example:**
```css
/* Source: https://freefrontend.com/css-glitch-effects/ */
.glitch-screen-shake {
  animation: shake 0.2s ease-in-out;
}

@keyframes shake {
  0%, 100% { transform: translate(0, 0); }
  25% { transform: translate(-2px, 2px); }
  50% { transform: translate(2px, -2px); }
  75% { transform: translate(-2px, -2px); }
}

.glitch-chromatic {
  position: relative;
}

.glitch-chromatic::before,
.glitch-chromatic::after {
  content: attr(data-text);
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
}

.glitch-chromatic::before {
  left: 2px;
  text-shadow: -2px 0 #f0f; /* Magenta offset */
  clip: rect(0, 900px, 0, 0);
  animation: glitch-before 0.3s infinite;
}

.glitch-chromatic::after {
  left: -2px;
  text-shadow: -2px 0 #0ff; /* Cyan offset */
  clip: rect(0, 900px, 0, 0);
  animation: glitch-after 0.3s infinite;
}

@keyframes glitch-before {
  0%, 100% { clip: rect(0, 900px, 0, 0); }
  50% { clip: rect(0, 900px, 40px, 0); }
}

@keyframes glitch-after {
  0%, 100% { clip: rect(40px, 900px, 100px, 0); }
  50% { clip: rect(20px, 900px, 60px, 0); }
}
```

### Pattern 6: Alien Character Substitution
**What:** Temporarily replace numbers with alien characters during effects
**When to use:** Visual enhancement during fast solving or backtracking
**Example:**
```javascript
// Source: Existing project code - Metrics/modules/screensaver.js
class AlienEffect {
  constructor() {
    this.aliens = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
  }

  scramble(element, duration = 300) {
    const original = element.textContent;
    const startTime = Date.now();

    const interval = setInterval(() => {
      const elapsed = Date.now() - startTime;
      if (elapsed > duration) {
        clearInterval(interval);
        element.textContent = original;
        return;
      }

      // Randomly replace with alien characters
      const random = this.aliens[Math.floor(Math.random() * this.aliens.length)];
      element.textContent = random;
    }, 50);
  }
}
```

### Anti-Patterns to Avoid
- **Animating left/top/margin**: Use transform: translate() instead—these cause expensive layout recalculations
- **Over-using will-change**: Creates composite layers that consume memory; only apply to actively animating elements
- **Deep DOM nesting of animated elements**: Triggers implicit compositing of all parent layers
- **Animating background-color**: Not GPU-accelerated until late 2026; use colored overlays with opacity instead
- **Creating 81 separate composite layers**: Animate only the active cell, not the entire grid simultaneously

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Sudoku solving algorithm | Custom solver from scratch | Existing JavaScript implementation | Project already has Algorithms/BruteForce/JavaScript/Sudoku.js with verified iteration counts |
| Range slider styling | JavaScript-based slider widget | Native input type="range" + CSS | Native control works on all devices, fully accessible, customizable via ::-webkit-slider-thumb |
| Animation timing | Manual setInterval timing | requestAnimationFrame | Automatically syncs with refresh rate, pauses in background, prevents screen tearing |
| State history undo/redo | Complex event sourcing | Simple array + position pointer | Linear history (no branches) only needs array indexing, not graph traversal |
| FPS limiting | setTimeout wrapper | Delta time accumulation | Smoother on high-refresh displays (120Hz+), adapts to dropped frames |
| CRT/glitch effects | Canvas pixel manipulation | CSS filters + transforms | CSS runs on GPU, no JavaScript overhead, easier to maintain |

**Key insight:** Browser APIs have evolved specifically for animation use cases—CSS transforms and requestAnimationFrame provide better performance than custom implementations because they leverage GPU acceleration and compositor threads that JavaScript cannot access directly.

## Common Pitfalls

### Pitfall 1: Memory Explosion from Composite Layers
**What goes wrong:** Adding transform: translateZ(0) or will-change to every grid cell creates 81 separate GPU textures, consuming 19MB+ on mobile devices and potentially crashing the browser
**Why it happens:** Developers treat GPU hints as "free performance" without understanding memory cost—each composite layer is rendered as full RGBa bitmap (width × height × 4 bytes)
**How to avoid:**
- Only composite the actively animating cell, not the entire grid
- Monitor memory in Chrome DevTools > Performance > Memory
- Test on actual mobile devices (iPhone 6 has ~200MB GPU limit)
- Use parent perspective + child transforms instead of per-cell compositing
**Warning signs:**
- Increased memory usage in DevTools
- Janky animations on mobile
- Browser tab crashes on low-end devices
**Source:** Smashing Magazine "GPU Animation: Doing It Right" (verified pitfall)

### Pitfall 2: Animation Runs Faster on High-Refresh Displays
**What goes wrong:** Animation speed doubles on 120Hz displays compared to 60Hz if using frame count instead of delta time
**Why it happens:** requestAnimationFrame fires at display refresh rate—120 callbacks/sec on 120Hz display vs 60 callbacks/sec on 60Hz display
**How to avoid:**
- Always use the timestamp parameter passed to requestAnimationFrame callback
- Calculate progress as (timestamp - startTime) / duration
- Never increment counters per frame (e.g., x += 1 per frame)
**Warning signs:**
- Users report animation speeds vary between devices
- Animation completes in half the expected time on some screens
**Source:** MDN requestAnimationFrame documentation (verified best practice)

### Pitfall 3: Memory Leaks from Unreleased Event Listeners
**What goes wrong:** Animation stops but memory usage keeps growing because event listeners (play/pause buttons, speed slider) aren't removed when switching views
**Why it happens:** addEventListener creates strong references—even if DOM elements are removed, listeners keep objects alive in memory
**How to avoid:**
- Store listener references and call removeEventListener on cleanup
- Use {once: true} option for single-use listeners
- Create cleanup function that runs when user navigates away from interactive solver
**Warning signs:**
- Memory usage increases each time user opens interactive solver
- Browser becomes sluggish after multiple interactions
**Source:** DEV Community "How to Avoid Memory Leaks in JavaScript Event Listeners" (2026, verified pattern)

### Pitfall 4: State History Array Grows Unbounded
**What goes wrong:** Solver runs for 439,269 iterations (matrix 2), creating 439K state objects consuming 100MB+ RAM and freezing the browser during array operations
**Why it happens:** Storing every single iteration state without considering memory limits
**How to avoid:**
- Limit history to last N states (e.g., 10,000) for step-by-step mode
- For auto-play at high speeds (50x+), skip history recording—just animate final result
- Implement circular buffer to overwrite old states
- Use "skip to end" option that only stores initial and final states
**Warning signs:**
- Browser freezes when stepping through long-running solvers
- Memory usage spikes to 500MB+
- Array operations (stepForward) become slower over time
**Recommendation:** Record full history only for simple puzzles (<10K iterations) or when user explicitly requests step-by-step mode; otherwise use summary mode with keyframes only

### Pitfall 5: CSS 3D Transforms Break on Mobile Safari
**What goes wrong:** Grid appears flat or numbers disappear during rotation on iOS devices
**Why it happens:** Mobile Safari requires explicit -webkit- prefixes for some 3D transform properties and has stricter z-index stacking with transform-style: preserve-3d
**How to avoid:**
- Include -webkit-transform, -webkit-perspective vendor prefixes
- Test backface-visibility: hidden—sometimes causes flickering on mobile, use sparingly
- Avoid nesting preserve-3d elements more than 2 levels deep
- Use simpler transforms on mobile (detected via media queries or touch events)
**Warning signs:**
- Desktop works perfectly but mobile shows broken rendering
- Numbers vanish at rotation midpoint
**Source:** CSS-Tricks 3D transform examples (verified cross-browser issue)

### Pitfall 6: Spinner Animation Timing Conflicts
**What goes wrong:** Cell spins at constant 0.6s duration regardless of playback speed—at 100x speed, cells are still spinning from previous iterations when new changes arrive
**Why it happens:** CSS animation duration is fixed; doesn't scale with JavaScript playback speed
**How to avoid:**
- Calculate animation duration dynamically: `duration = baseTime / speed`
- Disable individual cell animations at speeds >10x, use only color changes
- For very high speeds (50x+), skip per-iteration animations entirely—just show solving progress indicator
**Warning signs:**
- Overlapping animations create visual chaos at high speeds
- Can't see individual changes at 50x+ speed anyway
**Recommendation:** 1x-10x = full animations, 10x-50x = color-only changes, 50x+ = progress bar with final state

## Code Examples

Verified patterns from official sources:

### Complete Animation Controller Integration
```javascript
// Source: Combined patterns from MDN + GitHub examples
class InteractiveSolver {
  constructor(containerElement) {
    this.container = containerElement;
    this.history = new SolverHistory();
    this.animator = null;
    this.gridRenderer = new GridRenderer(containerElement);
    this.controls = new PlaybackControls(this);
  }

  async loadPuzzle(matrixPath, algorithm) {
    // Run solver and capture all states
    const solver = algorithm === 'BruteForce'
      ? new BruteForceSolver()
      : new DLXSolver();

    await solver.solve(matrixPath, (state) => {
      this.history.push(state);
    });

    // Render initial state
    const firstState = this.history.getCurrentState();
    this.gridRenderer.render(firstState);
  }

  play() {
    if (this.animator) this.animator.stop();

    this.animator = new AnimationController(
      this.history,
      (state) => this.gridRenderer.render(state)
    );

    this.animator.setSpeed(this.controls.getSpeed());
    this.animator.start();
  }

  pause() {
    if (this.animator) this.animator.stop();
  }

  stepForward() {
    this.pause();
    if (this.history.canStepForward()) {
      const state = this.history.stepForward();
      this.gridRenderer.render(state);
    }
  }

  stepBackward() {
    this.pause();
    if (this.history.canStepBack()) {
      const state = this.history.stepBack();
      this.gridRenderer.render(state);
    }
  }

  reset() {
    this.pause();
    this.history.position = 0;
    const state = this.history.getCurrentState();
    this.gridRenderer.render(state);
  }

  skipToEnd() {
    this.pause();
    this.history.position = this.history.states.length - 1;
    const state = this.history.getCurrentState();
    this.gridRenderer.render(state);
  }

  cleanup() {
    if (this.animator) this.animator.stop();
    this.controls.cleanup();
    this.gridRenderer.cleanup();
  }
}
```

### Custom Range Slider Styling
```css
/* Source: https://www.smashingmagazine.com/2021/12/create-custom-range-input-consistent-browsers/ */
/* Remove default styling */
input[type="range"] {
  -webkit-appearance: none;
  appearance: none;
  background: transparent;
  cursor: pointer;
  width: 100%;
}

/* Track styling - WebKit browsers */
input[type="range"]::-webkit-slider-runnable-track {
  background: linear-gradient(
    to right,
    #0ff 0%,
    #0ff var(--progress, 50%),
    rgba(255, 255, 255, 0.2) var(--progress, 50%),
    rgba(255, 255, 255, 0.2) 100%
  );
  height: 6px;
  border-radius: 3px;
  box-shadow: 0 0 5px #0ff;
}

/* Track styling - Firefox */
input[type="range"]::-moz-range-track {
  background: rgba(255, 255, 255, 0.2);
  height: 6px;
  border-radius: 3px;
}

input[type="range"]::-moz-range-progress {
  background: #0ff;
  height: 6px;
  border-radius: 3px;
  box-shadow: 0 0 5px #0ff;
}

/* Thumb styling - WebKit */
input[type="range"]::-webkit-slider-thumb {
  -webkit-appearance: none;
  appearance: none;
  height: 20px;
  width: 20px;
  background: #fff;
  border: 2px solid #0ff;
  border-radius: 50%;
  box-shadow:
    0 0 5px #fff,
    0 0 10px #0ff;
  margin-top: -7px; /* Center on track */
  transition: transform 0.1s ease;
}

input[type="range"]::-webkit-slider-thumb:hover {
  transform: scale(1.2);
}

/* Thumb styling - Firefox */
input[type="range"]::-moz-range-thumb {
  height: 20px;
  width: 20px;
  background: #fff;
  border: 2px solid #0ff;
  border-radius: 50%;
  box-shadow:
    0 0 5px #fff,
    0 0 10px #0ff;
  transition: transform 0.1s ease;
}

input[type="range"]::-moz-range-thumb:hover {
  transform: scale(1.2);
}

/* JavaScript to update progress CSS variable */
const slider = document.getElementById('speed-slider');
slider.addEventListener('input', (e) => {
  const value = e.target.value;
  const max = e.target.max;
  const progress = (value / max) * 100;
  slider.style.setProperty('--progress', `${progress}%`);
});
```

### Grid Renderer with State-Based Styling
```javascript
// Source: Combined best practices from GitHub Sudoku visualizer projects
class GridRenderer {
  constructor(containerElement) {
    this.container = containerElement;
    this.cells = [];
    this.alienEffect = new AlienEffect();
    this.initGrid();
  }

  initGrid() {
    this.container.innerHTML = `
      <div class="solver-grid">
        ${Array(81).fill(0).map((_, i) => `
          <div class="solver-cell" data-row="${Math.floor(i/9)}" data-col="${i%9}">
            <span class="cell-value"></span>
          </div>
        `).join('')}
      </div>
    `;

    this.cells = Array.from(this.container.querySelectorAll('.solver-cell'));
  }

  render(state) {
    const { grid, row, col, isBacktrack, depth } = state;

    // Update all cells with current grid values
    for (let r = 0; r < 9; r++) {
      for (let c = 0; c < 9; c++) {
        const idx = r * 9 + c;
        const cell = this.cells[idx];
        const valueSpan = cell.querySelector('.cell-value');

        valueSpan.textContent = grid[r][c] || '';

        // Remove all state classes
        cell.classList.remove('active', 'success', 'backtrack',
                              'spinning', 'spinning-reverse');
      }
    }

    // Highlight active cell
    if (row !== undefined && col !== undefined) {
      const idx = row * 9 + col;
      const cell = this.cells[idx];

      if (isBacktrack) {
        cell.classList.add('backtrack', 'spinning-reverse');
        // Optional glitch effect at deep backtracking
        if (depth > 5) {
          this.container.classList.add('glitch-screen-shake');
          setTimeout(() => {
            this.container.classList.remove('glitch-screen-shake');
          }, 200);
        }
      } else {
        cell.classList.add('active', 'spinning');
        if (grid[row][col] !== 0) {
          // Success - value placed
          setTimeout(() => {
            cell.classList.remove('active');
            cell.classList.add('success');
          }, 300); // After spin completes
        }
      }
    }
  }

  cleanup() {
    // No event listeners to remove - purely declarative rendering
    this.container.innerHTML = '';
  }
}
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Canvas-based grids | CSS Grid + CSS Transforms | 2020+ | GPU acceleration now standard, CSS easier to style and maintain |
| setInterval for animation | requestAnimationFrame | 2015+ | Automatic sync with refresh rate, better battery life |
| JavaScript animation libraries | CSS transitions + GPU hints | 2018+ | Compositor thread offloads from main thread, 60fps easier |
| will-change: transform everywhere | Selective compositing | 2022+ | Reduced memory usage after mobile performance studies |
| jQuery animate() | Native CSS animations | 2016+ | Lighter weight, better performance, no library dependency |
| Redux for animation state | Simple array + pointer | 2024+ | Linear history doesn't need graph state management |

**Deprecated/outdated:**
- **translate3d(0,0,0) hack**: Modern browsers auto-composite transform animations; explicit translateZ(0) adds unnecessary layers
- **-webkit-transform-style: preserve-3d**: Dropped -webkit- prefix requirement in Safari 15.4 (2022), though still works
- **jQuery.animate()**: Superseded by CSS animations and Web Animations API for better performance

## Open Questions

Things that couldn't be fully resolved:

1. **Optimal history size threshold**
   - What we know: 439K states (matrix 2) causes memory issues; <10K states works smoothly
   - What's unclear: Exact crossover point where chunking/keyframes become necessary varies by device RAM
   - Recommendation: Start with 10K limit, provide user setting to increase for power users

2. **Mobile glitch effect performance**
   - What we know: CSS filters and chromatic aberration work on desktop
   - What's unclear: Performance impact on low-end mobile devices (< iPhone 12)
   - Recommendation: Feature-detect GPU capabilities or provide "reduced motion" mode that disables glitches

3. **Algorithm state capture overhead**
   - What we know: BruteForce solver needs modification to emit states at each iteration
   - What's unclear: Performance impact of Object.freeze() on every state vs deep cloning
   - Recommendation: Test both approaches; if solver slows >10%, use shallow clone with warnings about mutation

## Sources

### Primary (HIGH confidence)
- MDN Web Docs: requestAnimationFrame - Official browser API documentation
- MDN Web Docs: CSS 3D Transforms - Official CSS specification guide
- Smashing Magazine: GPU Animation Doing It Right (2016) - Memory pitfalls verified current in 2026
- https://gist.github.com/sag1v/86dabe5f5ca9e3ca28ea39c374d51c9f - Simple undo/redo implementation pattern

### Secondary (MEDIUM confidence)
- https://github.com/MichealNestor01/Sudoku-Solver - JavaScript animated Sudoku solver (reference implementation)
- https://github.com/nelfarna/Animated-Sudoku-Solver - Canvas-based backtracking visualizer (alternative approach)
- FreeCodeCamp: 65 CSS 3D Transforms - Community examples collection
- CSS-Tricks: Neon Text Effects - Verified shadow stacking technique
- Polypane: CSS 3D Transform Examples (2024) - Recent visual examples

### Secondary (MEDIUM confidence) - Performance
- TestMu AI: CSS GPU Acceleration (2026) - Current performance best practices
- Chrome Developers Blog: Hardware-Accelerated Animations - Browser implementation details
- LogRocket: CSS Range Slider Customization - Cross-browser styling guide

### Secondary (MEDIUM confidence) - Memory Management
- DEV Community: Memory Leaks in JavaScript Event Listeners (2026) - Recent cleanup patterns
- Medium: Understanding Memory Leaks in JavaScript (2025) - Practical debugging examples

### Tertiary (LOW confidence)
- Various GitHub Sudoku visualizer projects - Approaches vary widely, no single standard
- W3Schools tutorials - Good starting point but lacks depth on performance pitfalls

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - requestAnimationFrame and CSS 3D transforms are stable, well-documented browser APIs with no alternatives needed
- Architecture: HIGH - State history pattern verified across multiple sources; animation loop pattern from official MDN docs
- Pitfalls: HIGH - Memory limits and refresh-rate issues verified from authoritative sources (Smashing Magazine, MDN)
- Code examples: MEDIUM - Combined from multiple sources; patterns are standard but integration needs testing with existing Sudoku.js

**Research date:** 2026-01-24
**Valid until:** 2026-03-24 (60 days—stable APIs, but mobile performance characteristics evolve with new devices)
