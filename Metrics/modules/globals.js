
// Global state management
export const state = {
    currentSort: { metric: '', dir: 1 }, // 1 = Asc, -1 = Desc
    useDockerMode: window.location.port === '9001',
    isDockerPort: window.location.port === '9001',
    isLocalPort: window.location.port === '9002',
    currentAlgorithm: 'BruteForce',
    currentChartMode: 'scatter'
};

// Expose currentSort on window for legacy compatibility
window.currentSort = state.currentSort;
window.currentAlgorithm = state.currentAlgorithm;

// Log execution mode
console.log(`Execution mode: ${state.useDockerMode ? 'Docker (port 9001)' : 'Local (port 9002)'}`);

// Switch execution mode by navigating to different port
export function switchExecutionMode() {
    const currentPort = window.location.port;
    const newPort = currentPort === '9001' ? '9002' : '9001';
    const newUrl = window.location.protocol + '//' + window.location.hostname + ':' + newPort + window.location.pathname;
    window.location.href = newUrl;
}

// Initialize Docker/Local button based on current port
export function initializeDockerToggle() {
    const icon = document.getElementById('dockerIcon');
    const label = document.getElementById('dockerLabel');
    const btn = document.getElementById('dockerToggleBtn');

    if (icon && label && btn) {
        if (state.isDockerPort) {
            icon.textContent = '🐳';
            label.textContent = 'Docker';
            btn.style.background = 'rgba(0, 150, 255, 0.3)';
            btn.style.borderColor = '#0096ff';
            btn.title = 'Currently on Docker (port 9001). Click to switch to Local.';
        } else if (state.isLocalPort) {
            icon.textContent = '💻';
            label.textContent = 'Local';
            btn.style.background = 'rgba(0, 255, 157, 0.3)';
            btn.style.borderColor = '#00ff9d';
            btn.title = 'Currently on Local (port 9002). Click to switch to Docker.';
        } else {
            // Not on standard ports - show neutral state
            icon.textContent = '❓';
            label.textContent = 'File';
            btn.title = 'Open via server (localhost:9001 or :9002) to enable execution';
            btn.style.opacity = '0.5';
        }
    }
}
