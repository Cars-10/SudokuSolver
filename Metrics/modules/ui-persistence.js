// UI state persistence in localStorage

// Save current UI state
export function saveUIState() {
    try {
        const state = {
            scrollX: window.scrollX,
            scrollY: window.scrollY,
            chartMode: window.currentChartMode || 'scatter',
            algorithm: window.currentAlgorithm || 'BruteForce',
            showLogos: window.showLogos !== undefined ? window.showLogos : true,
            showMismatches: !document.getElementById('toggleMismatchesBtn')?.classList.contains('active'),
            sort: window.currentSort ? { ...window.currentSort } : null,
            personality: document.getElementById('personality-selector')?.value || 'Standard'
        };
        localStorage.setItem('benchmarkUIState', JSON.stringify(state));
        console.log('[UI Persistence] State saved:', state);
    } catch (e) {
        console.warn('[UI Persistence] Failed to save state:', e);
    }
}

// Restore saved UI state
export function restoreUIState() {
    try {
        const stateStr = localStorage.getItem('benchmarkUIState');
        if (!stateStr) return;

        const state = JSON.parse(stateStr);
        console.log('[UI Persistence] Restoring state:', state);

        // Restore Chart Mode
        if (state.chartMode && window.changeChartMode) {
            window.changeChartMode(state.chartMode);
        }

        // Restore Algorithm Filter
        if (state.algorithm && window.changeAlgorithmFilter) {
            window.changeAlgorithmFilter(state.algorithm);
        }

        // Restore Logo/Text Mode
        if (state.showLogos !== undefined) {
            window.showLogos = !state.showLogos; // Toggle flips it, so set inverse first
            const btn = document.querySelector('.zoom-btn[title*="Switch"]');
            if (btn && window.toggleLogoMode) window.toggleLogoMode(btn);
        }

        // Restore Mismatch Toggle
        const btn = document.getElementById('toggleMismatchesBtn');
        if (btn) {
            const currentlyHidden = btn.classList.contains('active');
            const shouldBeVisible = state.showMismatches;

            // If we want visible but it's hidden, OR we want hidden but it's visible -> toggle
            if (shouldBeVisible === currentlyHidden && window.toggleMismatches) {
                window.toggleMismatches();
            }
        }

        // Restore Personality
        if (state.personality) {
            const selector = document.getElementById('personality-selector');
            if (selector) {
                selector.value = state.personality;
                if (window.changePersonality) window.changePersonality();
            }
        }

        // Restore Scroll (Last step)
        if (state.scrollX !== undefined && state.scrollY !== undefined) {
            window.scrollTo(state.scrollX, state.scrollY);
        }

    } catch (e) {
        console.warn('[UI Persistence] Failed to restore state:', e);
    }
}

// Initialize auto-save on state changes
export function initializeAutoSave() {
    // Save state periodically
    setInterval(saveUIState, 5000); // Every 5 seconds

    // Save on page unload
    window.addEventListener('beforeunload', saveUIState);

    // Save on major UI changes
    const saveOnEvent = () => setTimeout(saveUIState, 100);

    // Watch for chart mode changes
    const originalChangeChartMode = window.changeChartMode;
    if (originalChangeChartMode) {
        window.changeChartMode = function(...args) {
            originalChangeChartMode.apply(this, args);
            saveOnEvent();
        };
    }

    // Watch for personality changes
    const personalitySelector = document.getElementById('personality-selector');
    if (personalitySelector) {
        personalitySelector.addEventListener('change', saveOnEvent);
    }

    // Watch for scroll
    let scrollTimeout;
    window.addEventListener('scroll', () => {
        clearTimeout(scrollTimeout);
        scrollTimeout = setTimeout(saveUIState, 500);
    });
}
