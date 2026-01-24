// solver-effects.js - Glitch Effects for Interactive Solver

export class GlitchEffects {
    constructor(gridContainer) {
        this.container = gridContainer;
        this.aliens = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
        this.activeEffects = new Set();
        this.enabled = true;
    }

    // Enable/disable all effects
    setEnabled(enabled) {
        this.enabled = enabled;
    }

    // Screen shake effect
    // Trigger on deep backtracks (depth > threshold)
    screenShake(duration = 200) {
        if (!this.enabled || this.activeEffects.has('shake')) return;

        this.activeEffects.add('shake');
        this.container.classList.add('glitch-screen-shake');

        setTimeout(() => {
            this.container.classList.remove('glitch-screen-shake');
            this.activeEffects.delete('shake');
        }, duration);
    }

    // Alien character scramble on a specific cell
    // Shows random alien characters briefly before settling
    alienScramble(cellElement, finalValue, duration = 300) {
        if (!this.enabled) return;

        const valueSpan = cellElement.querySelector('.cell-value');
        if (!valueSpan) return;

        const startTime = Date.now();
        const originalValue = finalValue || valueSpan.textContent;

        const scrambleInterval = setInterval(() => {
            const elapsed = Date.now() - startTime;
            if (elapsed >= duration) {
                clearInterval(scrambleInterval);
                valueSpan.textContent = originalValue;
                valueSpan.setAttribute('data-value', originalValue);
                return;
            }

            // Random alien character
            const randomChar = this.aliens[Math.floor(Math.random() * this.aliens.length)];
            valueSpan.textContent = randomChar;
        }, 50);
    }

    // Chromatic aberration effect on cell
    chromaticAberration(cellElement, duration = 500) {
        if (!this.enabled) return;

        cellElement.classList.add('chromatic');

        setTimeout(() => {
            cellElement.classList.remove('chromatic');
        }, duration);
    }

    // Color inversion flash (entire grid)
    colorInvert(duration = 100) {
        if (!this.enabled) return;

        this.container.style.filter = 'invert(1)';

        setTimeout(() => {
            this.container.style.filter = '';
        }, duration);
    }

    // Trigger combined glitch based on intensity level
    // intensity: 0-1 (0 = subtle, 1 = maximum chaos)
    triggerGlitch(intensity, cellElement = null) {
        if (!this.enabled) return;

        if (intensity > 0.7) {
            this.screenShake(200 + intensity * 100);
        }

        if (intensity > 0.5 && cellElement) {
            this.chromaticAberration(cellElement, 300 + intensity * 200);
        }

        if (intensity > 0.8) {
            this.colorInvert(50);
        }
    }

    // Smart trigger based on solver state
    // Call this from animation controller when state changes
    onStateChange(state, prevState) {
        if (!this.enabled) return;

        const { isBacktrack, depth, row, col } = state;

        // Screen shake on deep backtrack
        if (isBacktrack && depth > 5) {
            const intensity = Math.min(1, depth / 15);
            this.screenShake(150 + intensity * 150);
        }

        // Alien scramble on backtrack
        if (isBacktrack && row !== undefined && col !== undefined) {
            const cellIdx = row * 9 + col;
            const cells = this.container.querySelectorAll('.solver-cell');
            if (cells[cellIdx]) {
                // Only scramble occasionally to avoid chaos
                if (Math.random() > 0.7) {
                    this.alienScramble(cells[cellIdx], '', 150);
                }
            }
        }
    }

    // Clear all active effects
    clear() {
        this.container.classList.remove('glitch-screen-shake');
        this.container.style.filter = '';
        this.container.querySelectorAll('.chromatic').forEach(el => {
            el.classList.remove('chromatic');
        });
        this.activeEffects.clear();
    }
}

// Factory function
export function createGlitchEffects(gridContainer) {
    return new GlitchEffects(gridContainer);
}
