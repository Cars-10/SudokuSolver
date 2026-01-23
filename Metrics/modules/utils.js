// Utility functions

// Helper to normalize matrix identifiers (handles both "1" and "1.matrix" formats)
export function normalizeMatrix(m) {
    return String(m).replace('.matrix', '');
}

// Calculate and update relative times (e.g., "3d ago", "2h ago")
export function updateRelativeTimes() {
    const now = Date.now();
    document.querySelectorAll('.relative-time[data-ts]').forEach(el => {
        const ts = parseInt(el.dataset.ts, 10);
        if (isNaN(ts)) return;

        const dateDiff = now - ts;
        let text;
        if (dateDiff > 86400000) {
            text = Math.floor(dateDiff / 86400000) + "d ago";
        } else if (dateDiff > 3600000) {
            text = Math.floor(dateDiff / 3600000) + "h ago";
        } else if (dateDiff > 60000) {
            text = Math.floor(dateDiff / 60000) + "m ago";
        } else {
            text = "Just now";
        }
        el.textContent = text;
    });
}

// Convert hex color to rgba
export function hexToRgba(hex, alpha) {
    const r = parseInt(hex.slice(1, 3), 16);
    const g = parseInt(hex.slice(3, 5), 16);
    const b = parseInt(hex.slice(5, 7), 16);
    return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

// Trap focus within an element (for accessibility)
export function trapFocus(element) {
    const focusableElements = element.querySelectorAll(
        'a[href], button:not([disabled]), textarea:not([disabled]), input:not([disabled]), select:not([disabled]), [tabindex]:not([tabindex="-1"])'
    );
    const firstFocusable = focusableElements[0];
    const lastFocusable = focusableElements[focusableElements.length - 1];

    element.addEventListener('keydown', function (e) {
        if (e.key === 'Tab') {
            if (e.shiftKey) {
                if (document.activeElement === firstFocusable) {
                    e.preventDefault();
                    lastFocusable.focus();
                }
            } else {
                if (document.activeElement === lastFocusable) {
                    e.preventDefault();
                    firstFocusable.focus();
                }
            }
        }
    });
}
