
// Matrix Screensaver & Alien Status Effects
import { state } from './globals.js';

// Global variables for Matrix stats
let matrixPuzzles = []; // Will be populated from window or passed in

export function setMatrixPuzzles(puzzles) {
    matrixPuzzles = puzzles;
}

// --- Alien Status Effect ---
export class AlienStatusSystem {
    constructor() {
        this.aliens = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
        this.interval = null;
        this.scrambleInterval = null;
        this.rotations = []; // Track rotation state for each character
        this.originals = new Map(); // Store original text for each element
        console.log('[AlienStatusSystem] Constructor');
    }

    start() {
        // Store original text values on start
        this.storeOriginals();
        // Run every 60 seconds (one minute between glitches)
        this.interval = setInterval(() => this.triggerEffect(), 60000);
    }

    stop() {
        if (this.interval) {
            clearInterval(this.interval);
            this.interval = null;
        }
        // Restore all originals when stopping
        this.restoreOriginals();
    }

    storeOriginals() {
        const targets = this.getTargets();
        targets.forEach(el => {
            if (!this.originals.has(el)) {
                this.originals.set(el, el.innerText);
            }
        });
    }

    restoreOriginals() {
        this.originals.forEach((text, el) => {
            if (el && document.body.contains(el)) {
                // Use innerHTML and convert \n to <br> to preserve line breaks
                el.innerHTML = text.replace(/\n/g, '<br>');
                el.style.perspective = '';
            }
        });
    }

    getTargets() {
        return [
            ...document.querySelectorAll('.screensaver-diagnostics span'),
            document.querySelector('.screensaver-mismatch'),
            document.querySelector('.screensaver-title-text'),
            document.getElementById('riddle-container')
        ].filter(el => el);
    }

    triggerEffect() {
        // Only run if Red Pill screensaver is active
        const isFullscreen = document.body.classList.contains('fullscreen-active');
        if (!isFullscreen) return;

        const targets = this.getTargets();
        if (targets.length === 0) return;

        // Re-store originals in case they changed
        this.storeOriginals();

        // Get originals for this effect run
        const originals = targets.map(el => this.originals.get(el) || el.innerText);
        this.rotations = originals.map(text =>
            Array.from(text).map(() => Math.random() * 360)
        );

        // Track which characters have been "locked" back to original
        const locked = originals.map(text => Array.from(text).map(() => false));

        const startTime = Date.now();
        const glitchDuration = 2500; // Main glitch phase
        const revealDuration = 4000; // Reveal phase - chars lock in one at a time (slowed down)
        const cylonDuration = 3000; // Red cylon scanning phase after reveal
        const totalDuration = glitchDuration + revealDuration + cylonDuration;
        window.alienEffectActive = true;

        // Calculate total non-space characters for reveal timing
        let totalChars = 0;
        originals.forEach(text => {
            for (const ch of text) {
                if (ch !== ' ' && ch !== ':' && ch !== '\n') totalChars++;
            }
        });

        // Add perspective to parent for 3D effect
        targets.forEach(el => {
            el.style.perspective = '500px';
            el.style.perspectiveOrigin = 'center';
        });

        let lastUpdate = 0;
        let revealedCount = 0;

        const animate = (timestamp) => {
            const elapsed = Date.now() - startTime;

            if (elapsed > totalDuration) {
                // Restore original text properly - use innerHTML to preserve <br> line breaks
                targets.forEach((el, i) => {
                    el.innerHTML = originals[i].replace(/\n/g, '<br>');
                    el.style.perspective = '';
                });
                window.alienEffectActive = false;
                return;
            }

            const inRevealPhase = elapsed > glitchDuration && elapsed <= glitchDuration + revealDuration;
            const inCylonPhase = elapsed > glitchDuration + revealDuration;

            // During reveal phase, calculate how many chars should be locked
            if (inRevealPhase) {
                const revealProgress = (elapsed - glitchDuration) / revealDuration;
                const targetRevealed = Math.floor(revealProgress * totalChars);

                // Lock new characters one at a time
                while (revealedCount < targetRevealed) {
                    // Find next unlocked character (iterate through all targets/chars)
                    let found = false;
                    for (let i = 0; i < originals.length && !found; i++) {
                        for (let j = 0; j < originals[i].length && !found; j++) {
                            const ch = originals[i][j];
                            if (ch !== ' ' && ch !== ':' && ch !== '\n' && !locked[i][j]) {
                                locked[i][j] = true;
                                revealedCount++;
                                found = true;
                            }
                        }
                    }
                    if (!found) break; // All locked
                }
            } else if (inCylonPhase) {
                // Ensure all characters are locked during cylon phase
                for (let i = 0; i < locked.length; i++) {
                    for (let j = 0; j < locked[i].length; j++) {
                        locked[i][j] = true;
                    }
                }
            }

            // Progress for glitch intensity (0 to 1 during glitch phase, stays at 1 during reveal)
            const glitchProgress = Math.min(elapsed / glitchDuration, 1);

            // Rotation speed: starts fast (25 deg), slows to near-stop (1 deg)
            const rotationSpeed = 25 * (1 - glitchProgress * 0.96); // 25 -> 1

            // Character change frequency: starts every frame, slows down
            const changeInterval = 30 + glitchProgress * 150; // 30ms -> 180ms
            const shouldChangeChars = (timestamp - lastUpdate) > changeInterval;
            if (shouldChangeChars) lastUpdate = timestamp;

            // Calculate cylon scanner position (sweeps back and forth) - runs from the very beginning
            let cylonPos = -1;
            let cylonWidth = 2; // Cover 2 letters at a time
            // Cylon runs throughout the entire effect on OptionIsEscape
            const cylonCycleDuration = 3000; // 3 seconds per sweep (slower)
            const cylonProgress = (elapsed % cylonCycleDuration) / cylonCycleDuration;
            // Ping-pong effect: go right then left
            const phase = (cylonProgress * 2) % 2;
            const normalizedPos = phase < 1 ? phase : 2 - phase; // 0->1->0
            // Find the riddle container's text length for cylon scanning
            const riddleEl = document.getElementById('riddle-container');
            const riddleLen = riddleEl ? (this.originals.get(riddleEl) || riddleEl.innerText).replace(/[ :\n]/g, '').length : 0;
            cylonPos = Math.floor(normalizedPos * (riddleLen + cylonWidth - 1));

            // Scramble ALL targets together with rotation
            for (let i = 0; i < targets.length; i++) {
                const el = targets[i];
                const orig = originals[i];
                const isRiddleContainer = el.id === 'riddle-container';
                let html = '';
                let charIndex = 0; // Track non-space character index for cylon effect
                for (let j = 0; j < orig.length; j++) {
                    if (orig[j] === ' ') {
                        html += ' ';
                    } else if (orig[j] === '\n') {
                        html += '<br>';  // Convert newlines to <br> for HTML rendering
                    } else if (orig[j] === ':') {
                        html += orig[j];
                    } else if (locked[i][j]) {
                        // Character is locked - show original
                        // Riddle container always has cylon effect (white text with red scanner)
                        if (isRiddleContainer) {
                            const distFromScanner = Math.abs(charIndex - cylonPos);
                            if (distFromScanner < cylonWidth) {
                                // Cylon scanner - red glow with gradient from red to white
                                const intensity = 1 - (distFromScanner / cylonWidth);
                                const glow = Math.floor(25 * intensity);
                                const g = Math.floor(255 * (1 - intensity));
                                const b = Math.floor(255 * (1 - intensity));
                                html += `<span style="display:inline-block;transform:rotateY(0deg);color:rgb(255,${g},${b});text-shadow:0 0 ${glow}px #ff0000, 0 0 ${glow * 2}px #ff3300, 0 0 ${glow * 3}px #990000;">${orig[j]}</span>`;
                            } else {
                                // Not in scanner range - white text
                                html += `<span style="display:inline-block;transform:rotateY(0deg);color:#ffffff;text-shadow:0 0 5px rgba(255,255,255,0.3);">${orig[j]}</span>`;
                            }
                            charIndex++;
                        } else {
                            // Other elements - green glow
                            html += `<span style="display:inline-block;transform:rotateY(0deg);color:#00ff9d;text-shadow:0 0 10px #00ff9d;">${orig[j]}</span>`;
                        }
                    } else {
                        // Update rotation - speed decreases over time
                        this.rotations[i][j] = (this.rotations[i][j] + rotationSpeed + Math.random() * rotationSpeed * 0.5) % 360;
                        const rot = this.rotations[i][j];
                        // Only change character at the slowing interval
                        const char = shouldChangeChars
                            ? this.aliens[Math.floor(Math.random() * this.aliens.length)]
                            : el.children[j]?.textContent || this.aliens[Math.floor(Math.random() * this.aliens.length)];

                        // Apply cylon effect to riddle container even during glitch
                        if (isRiddleContainer) {
                            const distFromScanner = Math.abs(charIndex - cylonPos);
                            if (distFromScanner < cylonWidth) {
                                const intensity = 1 - (distFromScanner / cylonWidth);
                                const glow = Math.floor(25 * intensity);
                                const g = Math.floor(180 * (1 - intensity)); // Slightly dimmer for glitch chars
                                const b = Math.floor(180 * (1 - intensity));
                                html += `<span style="display:inline-block;transform:rotateY(${rot}deg);transform-style:preserve-3d;color:rgb(255,${g},${b});text-shadow:0 0 ${glow}px #ff0000, 0 0 ${glow * 2}px #ff3300;">${char}</span>`;
                            } else {
                                html += `<span style="display:inline-block;transform:rotateY(${rot}deg);transform-style:preserve-3d;color:#888888;">${char}</span>`;
                            }
                            charIndex++;
                        } else {
                            html += `<span style="display:inline-block;transform:rotateY(${rot}deg);transform-style:preserve-3d;">${char}</span>`;
                        }
                    }
                }
                el.innerHTML = html;
            }

            requestAnimationFrame(animate);
        };

        requestAnimationFrame(animate);
    }
}

// --- Riddle System ---
// Displays "OptionIsEscape" with continuous cylon scanning effect
export class RiddleSystem {
    constructor() {
        this.target = "OptionIsEscape";
        this.container = null;
        this.active = false;
        this.animationId = null;
        this.startTime = 0;
    }

    start() {
        this.container = document.getElementById('riddle-container');
        if (!this.container) return;
        this.active = true;
        this.startTime = Date.now();
        // Start continuous cylon animation
        this.animateCylon();
    }

    animateCylon() {
        if (!this.active || !this.container) return;

        // Skip rendering if alien glitch effect is active (it handles cylon during glitch)
        if (window.alienEffectActive) {
            this.animationId = requestAnimationFrame(() => this.animateCylon());
            return;
        }

        const elapsed = Date.now() - this.startTime;
        const text = this.target;
        const textLen = text.length;

        // Cylon parameters - 2 letters at a time, slower sweep
        const cylonWidth = 2; // Cover 2 letters
        const cylonCycleDuration = 3000; // 3 seconds per full sweep

        // Calculate cylon position (ping-pong)
        const cylonProgress = (elapsed % cylonCycleDuration) / cylonCycleDuration;
        const phase = (cylonProgress * 2) % 2;
        const normalizedPos = phase < 1 ? phase : 2 - phase; // 0->1->0
        const cylonPos = normalizedPos * (textLen - 1);

        // Build HTML with cylon effect
        let html = '';
        for (let i = 0; i < text.length; i++) {
            const distFromScanner = Math.abs(i - cylonPos);
            if (distFromScanner < cylonWidth) {
                // In scanner range - red glow with gradient
                const intensity = 1 - (distFromScanner / cylonWidth);
                const red = Math.floor(255 * intensity);
                const glow = Math.floor(25 * intensity);
                // Blend from white to red based on intensity
                const g = Math.floor(255 * (1 - intensity));
                const b = Math.floor(255 * (1 - intensity));
                html += `<span style="display:inline-block;color:rgb(255,${g},${b});text-shadow:0 0 ${glow}px #ff0000, 0 0 ${glow * 2}px #ff3300, 0 0 ${glow * 3}px #990000;">${text[i]}</span>`;
            } else {
                // Outside scanner - white text
                html += `<span style="display:inline-block;color:#ffffff;text-shadow:0 0 5px rgba(255,255,255,0.3);">${text[i]}</span>`;
            }
        }
        this.container.innerHTML = html;

        this.animationId = requestAnimationFrame(() => this.animateCylon());
    }

    stop() {
        this.active = false;
        if (this.animationId) {
            cancelAnimationFrame(this.animationId);
            this.animationId = null;
        }
        if (this.container) {
            this.container.innerText = '';
        }
    }
}


// --- Matrix Screensaver Logic ---
let active = false;
let animationId;
let width, height;
let ctx;
let columns;
let drops = [];
let cars10State = [];
let puzzleLines = [];
let currentPuzzleIndex = 0;
let puzzleY = -1000;
let puzzleColorIndex = 0;
const puzzleColors = ['#0f0', '#00ffff', '#ff00ff', '#ffff00', '#ff0080', '#ff4500'];
const chars = '/Cars10アァカサタナハマヤャラワガザダバパイィキシチニヒミリヰギジヂビピウゥクスツヌフムユュルグズブヅプエェケセテネヘメレヱゲゼデベペオォコソトノホモヨョロヲゴゾドボポヴッン0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
let currentMode = 'red';
let specialRows = new Set();
let lastCars10Time = 0;
let lastDanceTime = 0;
let dancingRow = 0;
let dancingCol = 0;
let slideInComplete = false;
let interactionGraceTime = 0; // Grace period before mouse exits blue pill

// Initialize
export function initMatrixScreensaver(puzzles) {
    matrixPuzzles = puzzles || [];
    const canvas = document.getElementById('matrix-canvas');
    if (!canvas) {
        console.error('[Screensaver] Canvas not found during init');
        return;
    }
    ctx = canvas.getContext('2d');
    console.log('[Screensaver] Initialized, ctx:', !!ctx);

    // Resize handler
    window.addEventListener('resize', () => {
        if (active) resize(canvas);
    });
}

function resize(canvas) {
    // Get device pixel ratio for sharp rendering on high-DPI displays
    const dpr = window.devicePixelRatio || 1;

    if (currentMode === 'red') {
        // Red pill: fullscreen - force visibility with inline styles
        canvas.style.cssText = `
            position: fixed !important;
            top: 0 !important;
            left: 0 !important;
            width: 100vw !important;
            height: 100vh !important;
            z-index: 9999 !important;
            background: #000 !important;
            display: block !important;
        `;
        width = window.innerWidth;
        height = window.innerHeight;
        console.log('[Screensaver] Red pill resize - viewport:', width, 'x', height);
    } else {
        // Blue pill: use parent container dimensions
        const parent = canvas.parentElement;
        if (parent) {
            canvas.style.position = 'absolute';
            canvas.style.top = '0';
            canvas.style.left = '0';
            canvas.style.width = '100%';
            canvas.style.height = '100%';
            canvas.style.zIndex = '10';
            canvas.style.background = '#000';
            width = parent.clientWidth || 800;
            height = parent.clientHeight || 400;
        } else {
            width = 800;
            height = 400;
        }
    }

    // Set canvas resolution accounting for device pixel ratio (fixes blurriness)
    canvas.width = width * dpr;
    canvas.height = height * dpr;

    // Scale the context to match the device pixel ratio
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);

    const fontSize = 14;
    columns = Math.ceil(width / fontSize);

    drops = [];
    cars10State = [];
    for (let i = 0; i < columns; i++) {
        drops[i] = Math.random() * -100;
        cars10State[i] = -1;
    }
}

export function startScreensaver(mode) {
    console.log("Starting screensaver mode:", mode);
    currentMode = mode;
    const canvas = document.getElementById('matrix-canvas');
    if (!canvas) return;

    active = true;

    // Set grace period for blue pill mode (500ms before mouse can exit)
    interactionGraceTime = mode === 'blue' ? Date.now() + 500 : 0;

    // Request fullscreen if Red
    if (mode === 'red') {
        const elem = document.documentElement;
        if (elem.requestFullscreen) {
            elem.requestFullscreen().catch(err => {
                console.warn(`Error attempting to enable full-screen mode: ${err.message}`);
                // Continue anyway
            });
        }
        document.body.classList.add('fullscreen-active');
        const header = document.getElementById('fullscreen-header');
        if (header) header.style.display = 'block';
    } else {
        // Blue pill: chart area only
        document.body.classList.remove('fullscreen-active');
        const header = document.getElementById('fullscreen-header');
        if (header) header.style.display = 'none';
    }

    resize(canvas);

    // Debug: log canvas dimensions
    console.log('[Screensaver] Canvas dimensions:', width, 'x', height, 'columns:', columns, 'drops:', drops.length);

    // Reset draw counter for debug
    drawCount = 0;

    // Initialize with solid black background
    if (ctx) {
        ctx.fillStyle = '#000000';
        ctx.fillRect(0, 0, width, height);
    } else {
        console.error('[Screensaver] No ctx when trying to fill background!');
    }

    animationId = requestAnimationFrame(draw);

    // Input handling to stop
    setupInputHandlers();
}

export function stopScreensaver() {
    active = false;
    cancelAnimationFrame(animationId);

    if (document.fullscreenElement) {
        document.exitFullscreen().catch(e => console.log(e));
    }
    document.body.classList.remove('fullscreen-active');

    const header = document.getElementById('fullscreen-header');
    if (header) header.style.display = 'none';

    // Cleanup input handlers
    removeInputHandlers();

    // Clear canvas
    if (ctx) ctx.clearRect(0, 0, width, height);
}

function setupInputHandlers() {
    document.addEventListener('keydown', handleKeyInput);
    document.addEventListener('mousemove', handleInteraction);
    document.addEventListener('click', handleInteraction);
}

function removeInputHandlers() {
    document.removeEventListener('keydown', handleKeyInput);
    document.removeEventListener('mousemove', handleInteraction);
    document.removeEventListener('click', handleInteraction);
}

function handleKeyInput(e) {
    if (!active) return;

    // Helper to stop screensaver properly through component if available
    const doStop = () => {
        if (window.screensaver && typeof window.screensaver.stop === 'function') {
            window.screensaver.stop();
        } else {
            stopScreensaver();
        }
    };

    // ESC fix: Always exit on Escape
    if (e.key === 'Escape' || e.key === 'Esc') { // Added ESC check
        doStop();
        return;
    }

    if (currentMode === 'red') {
        if (e.key === 'Shift') {
            puzzleColorIndex = (puzzleColorIndex + 1) % puzzleColors.length;
        } else if (e.key === 'Alt') {
            doStop();
        }
    } else {
        doStop(); // Blue pill exits on any key
    }
}

function handleInteraction(e) {
    if (!active) return;

    // Blue pill: exit on mouse move or click (after grace period)
    if (currentMode !== 'red') {
        // Check grace period for mouse movement
        if (e.type === 'mousemove' && Date.now() < interactionGraceTime) {
            return; // Still in grace period, ignore mouse movement
        }
        // Use component stop if available
        if (window.screensaver && typeof window.screensaver.stop === 'function') {
            window.screensaver.stop();
        } else {
            stopScreensaver();
        }
    }
}

let drawCount = 0;
let rainFrame = 0;
function draw() {
    if (!active) return;
    animationId = requestAnimationFrame(draw);
    rainFrame++;

    // Debug logging (first few frames only)
    if (drawCount < 5) {
        console.log('[Screensaver] draw() called, frame:', drawCount, 'drops:', drops.length, 'ctx:', !!ctx, 'dimensions:', width, 'x', height);
        drawCount++;
    }

    if (!ctx) {
        console.error('[Screensaver] No canvas context!');
        return;
    }

    // Black background with opacity for trail effect (0.05 = faster fade, crisper rain)
    ctx.fillStyle = 'rgba(0, 0, 0, 0.05)';
    ctx.fillRect(0, 0, width, height);

    ctx.shadowBlur = 8;
    ctx.shadowColor = 'rgba(0, 255, 0, 0.5)';
    ctx.fillStyle = '#0F0';
    ctx.font = '14px monospace';

    const secretMessage = "/Cars10 ";

    const shouldMove = rainFrame % 2 === 0;

    for (let i = 0; i < drops.length; i++) {
        const x = i * 14;
        const y = drops[i] * 14;
        let char = '';
        let isSpecial = false;

        if (cars10State[i] >= 0) {
            const idx = cars10State[i] % secretMessage.length;
            char = secretMessage[idx];
            // Only advance to next character when the drop moves
            if (shouldMove) {
                cars10State[i]++;
            }
            isSpecial = true;
        } else {
            char = chars.charAt(Math.floor(Math.random() * chars.length));
        }

        if (isSpecial) {
            ctx.fillStyle = '#FFFFFF';
            ctx.font = 'bold 14px monospace';
            if (cars10State[i] < secretMessage.length * 2) {
                // Bright glow for first two cycles of the message
                ctx.shadowColor = '#FFFFFF';
                ctx.shadowBlur = 15;
            } else {
                ctx.shadowColor = 'rgba(200, 255, 255, 0.8)';
                ctx.shadowBlur = 8;
            }
            ctx.fillText(char, x, y);
            ctx.shadowBlur = 8;
            ctx.shadowColor = 'rgba(0, 255, 0, 0.5)';
            ctx.fillStyle = '#0F0';
            ctx.font = '14px monospace';
        } else {
            ctx.fillText(char, x, y);
        }

        if (y > height && Math.random() > 0.975) {
            drops[i] = 0;
            if (Math.random() > 0.98) cars10State[i] = 0;
            else cars10State[i] = -1;
        }
        // Rain moves at half speed (only increment every 2 frames)
        if (shouldMove) {
            drops[i]++;
        }
    }

    ctx.shadowBlur = 0;
    ctx.shadowColor = 'transparent';

    // Puzzle Overlay logic omitted for brevity in thought, but will include in file.
    // Actually, I should include it to match functionality.

    // ... Puzzle Overlay Code ...
    drawPuzzle();
}

function drawPuzzle() {
    // ... Logic from report_client.js ...
    // Simplified:
    if (matrixPuzzles.length === 0) {
        if (puzzleLines.length === 0) prepareNextPuzzle();
    }

    if (puzzleLines.length > 0 && currentMode === 'red') {
        // Drawing logic...
        const lineHeight = 42;
        ctx.font = 'bold 24px "JetBrains Mono", monospace';
        ctx.textAlign = 'center';
        ctx.fillStyle = puzzleColors[puzzleColorIndex];

        for (let i = 0; i < puzzleLines.length; i++) {
            const lineY = puzzleY + (i * lineHeight);
            if (lineY > 0 && lineY < height) {
                ctx.fillText(puzzleLines[i], width / 2, lineY);
            }
        }
        puzzleY -= 2;
        if (puzzleY < -(puzzleLines.length * lineHeight)) {
            prepareNextPuzzle();
        }
    }
}

function prepareNextPuzzle() {
    if (matrixPuzzles.length === 0) {
        puzzleLines = ["M A T R I X", "S Y S T E M", "F A I L U R E"];
        return;
    }
    const raw = matrixPuzzles[currentPuzzleIndex];
    puzzleLines = raw.split('\\n');
    currentPuzzleIndex = (currentPuzzleIndex + 1) % matrixPuzzles.length;
    puzzleY = height;
}

