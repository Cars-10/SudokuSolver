
/**
 * report_client_modular.js - Modular benchmark report client
 *
 * This is the modularized version of report_client.js
 * Functions are organized into logical modules for better maintainability
 */

// Import all modules
import { state, switchExecutionMode, initializeDockerToggle } from './modules/globals.js';
import { normalizeMatrix, hexToRgba, trapFocus, updateRelativeTimes } from './modules/utils.js';
import { filterLanguages, sortRows, sortMatrix, toggleMismatches, filterByAlgorithm, toggleLanguageSelector, openLanguageModal } from './modules/table-operations.js';
import {
    closeModal,
    showMethodology,
    closeMethodology,
    showGoals,
    closeGoals,
    showWhy,
    closeWhy,
    closeSourceModal,
    closeMismatchModal,
    closeScoreModal,
    makeDraggable,
    makeElementDraggable,
    enableModalDragging
} from './modules/modal-operations.js';
import { changePersonality } from './modules/personality.js';
import { saveUIState, restoreUIState, initializeAutoSave } from './modules/ui-persistence.js';
import { initMatrixScreensaver, startScreensaver, stopScreensaver, setMatrixPuzzles, AlienStatusSystem } from './modules/screensaver.js';
import { initializeChart, switchChart, toggleLogoMode, handleZoomExtend, toggleChartFullscreen } from './modules/d3-chart.js';

// Expose functions to window for onclick handlers and legacy code
window.filterLanguages = filterLanguages;
window.sortRows = sortRows;
window.sortMatrix = sortMatrix;
window.toggleMismatches = toggleMismatches;
window.filterByAlgorithm = filterByAlgorithm;
window.toggleLanguageSelector = toggleLanguageSelector;
window.openLanguageModal = openLanguageModal;
window.changePersonality = changePersonality;
window.closeModal = closeModal;
window.showMethodology = showMethodology;
window.closeMethodology = closeMethodology;
window.showGoals = showGoals;
window.closeGoals = closeGoals;
window.showWhy = showWhy;
window.closeWhy = closeWhy;
window.closeSourceModal = closeSourceModal;
window.closeMismatchModal = closeMismatchModal;
window.closeScoreModal = closeScoreModal;
window.switchExecutionMode = switchExecutionMode;
window.normalizeMatrix = normalizeMatrix;
window.hexToRgba = hexToRgba;
window.startScreensaver = startScreensaver;
window.stopScreensaver = stopScreensaver;
window.AlienStatusSystem = AlienStatusSystem;

// Chart Exports
window.switchChart = switchChart;
window.toggleLogoMode = toggleLogoMode;
window.handleZoomExtend = handleZoomExtend;
window.toggleChartFullscreen = toggleChartFullscreen;

// Initialize on DOM ready
document.addEventListener('DOMContentLoaded', function () {
    console.log('[Modular Client] Initializing...');

    // Initialize Docker toggle
    initializeDockerToggle();

    // Start relative time updates
    updateRelativeTimes();
    setInterval(updateRelativeTimes, 60000); // Update every minute

    // Restore saved UI state
    restoreUIState();

    // Initialize auto-save
    initializeAutoSave();

    // Initialize Screensaver Puzzles (if data available)
    if (window.matrixPuzzles) {
        setMatrixPuzzles(window.matrixPuzzles);
        initMatrixScreensaver(window.matrixPuzzles);
    }

    // Start Alien System
    new AlienStatusSystem().start();

    // Initialize Chart
    initializeChart();

    console.log('[Modular Client] Initialization complete');
});

// Initialize modal dragging after page load
window.addEventListener('load', () => {
    enableModalDragging();
});

// ESC key handler for closing modals
document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape' || event.key === 'Esc') {
        // Close any visible modal
        const modals = [
            'langModal',
            'methodModal',
            'goalsModal',
            'whyModal',
            'sourceModal',
            'diagnosticsModal',
            'scoreModal'
        ];

        modals.forEach(modalId => {
            const modal = document.getElementById(modalId);
            if (modal && modal.classList.contains('visible')) {
                modal.classList.remove('visible');
                if (modalId === 'langModal') {
                    document.body.classList.remove('modal-open');
                }
            }
        });
    }
});

console.log('[Modular Client] report_client_modular.js loaded');
