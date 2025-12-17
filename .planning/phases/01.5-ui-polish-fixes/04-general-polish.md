# Plan 04: General UI Polish & Accessibility

**Objective:** Final polish pass for overall user experience, accessibility, and performance.

## Areas to Polish

### 1. Modal Accessibility

#### Keyboard Navigation
- **ESC key:** Close modal (already planned in Plan 01)
- **Tab key:** Focus trap within modal
- **Enter key:** Submit/save when in edit mode
- **Arrow keys:** Navigate between fields

**Implementation:**
```javascript
// Focus trap
function trapFocus(element) {
    const focusableElements = element.querySelectorAll(
        'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
    );
    const firstFocusable = focusableElements[0];
    const lastFocusable = focusableElements[focusableElements.length - 1];

    element.addEventListener('keydown', function(e) {
        if (e.key === 'Tab') {
            if (e.shiftKey) {
                if (document.activeElement === firstFocusable) {
                    lastFocusable.focus();
                    e.preventDefault();
                }
            } else {
                if (document.activeElement === lastFocusable) {
                    firstFocusable.focus();
                    e.preventDefault();
                }
            }
        }
    });
}

// Apply when modal opens
window.showLanguageDetails = function(lang, x, y) {
    // ... existing code ...
    modal.style.display = 'flex';
    trapFocus(modalContent);
    firstInput.focus(); // Focus first input/button
};
```

#### ARIA Labels
```html
<div id="langModal"
     class="modal"
     role="dialog"
     aria-labelledby="modalTitle"
     aria-describedby="modalDesc"
     aria-modal="true">

    <div class="modal-content" role="document">
        <button class="modal-close"
                aria-label="Close dialog">
            &times;
        </button>
        <!-- content -->
    </div>
</div>
```

### 2. Smooth Transitions

#### Page Load Animation
```css
/* Fade in on load */
body {
    animation: fadeInPage 0.5s ease-in;
}

@keyframes fadeInPage {
    from { opacity: 0; }
    to { opacity: 1; }
}

/* Stagger language cells */
.lang-cell {
    animation: slideInCell 0.3s ease-out backwards;
}

.lang-cell:nth-child(1) { animation-delay: 0.05s; }
.lang-cell:nth-child(2) { animation-delay: 0.1s; }
.lang-cell:nth-child(3) { animation-delay: 0.15s; }
/* etc. */

@keyframes slideInCell {
    from {
        opacity: 0;
        transform: translateY(20px);
    }
    to {
        opacity: 1;
        transform: translateY(0);
    }
}
```

#### Hover Effects
```css
/* Language cell hover */
.lang-cell {
    transition: transform 0.2s ease, box-shadow 0.2s ease, background 0.2s ease;
}

.lang-cell:hover {
    transform: translateY(-4px);
    box-shadow: 0 8px 20px rgba(122, 162, 247, 0.3);
    background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
}

/* Button hover */
.btn {
    transition: background 0.2s ease, transform 0.1s ease;
}

.btn:hover {
    transform: scale(1.05);
}

.btn:active {
    transform: scale(0.98);
}
```

### 3. Loading States

#### Report Generation Progress
```javascript
// Show loading overlay during generation
function showLoadingOverlay(message = 'Generating report...') {
    const overlay = document.createElement('div');
    overlay.id = 'loading-overlay';
    overlay.innerHTML = `
        <div class="loading-spinner"></div>
        <p>${message}</p>
    `;
    document.body.appendChild(overlay);
}

function hideLoadingOverlay() {
    const overlay = document.getElementById('loading-overlay');
    if (overlay) overlay.remove();
}
```

**CSS:**
```css
#loading-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0, 0, 0, 0.8);
    backdrop-filter: blur(8px);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    z-index: 99999;
}

.loading-spinner {
    width: 50px;
    height: 50px;
    border: 4px solid rgba(122, 162, 247, 0.2);
    border-top-color: #7aa2f7;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
}

@keyframes spin {
    to { transform: rotate(360deg); }
}
```

### 4. Mobile Responsiveness

#### Viewport Meta Tag
```html
<meta name="viewport" content="width=device-width, initial-scale=1.0">
```

#### Responsive Breakpoints
```css
/* Tablet */
@media (max-width: 1024px) {
    .lang-grid {
        grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
    }

    .modal .modal-content {
        width: 85%;
    }
}

/* Mobile */
@media (max-width: 768px) {
    .lang-grid {
        grid-template-columns: 1fr;
        gap: 10px;
    }

    .modal .modal-content {
        width: 95%;
        max-height: 90vh;
    }

    .modal-header {
        flex-direction: column;
        text-align: center;
    }

    .modal-img-container {
        width: 80px;
        height: 80px;
        margin-bottom: 10px;
    }

    /* Stack buttons vertically on mobile */
    .modal-footer {
        flex-direction: column;
    }

    .modal-footer .btn {
        width: 100%;
    }
}

/* Small mobile */
@media (max-width: 480px) {
    body {
        font-size: 14px;
    }

    h1 {
        font-size: 1.5rem;
    }

    .btn {
        padding: 8px 16px;
        font-size: 0.9em;
    }
}
```

### 5. Error Handling

#### Better Error Messages
```javascript
function showError(message, details = null) {
    const errorDiv = document.createElement('div');
    errorDiv.className = 'error-toast';
    errorDiv.innerHTML = `
        <div class="error-icon">⚠️</div>
        <div class="error-content">
            <strong>Error</strong>
            <p>${message}</p>
            ${details ? `<small>${details}</small>` : ''}
        </div>
        <button onclick="this.parentElement.remove()">×</button>
    `;
    document.body.appendChild(errorDiv);

    // Auto-remove after 5 seconds
    setTimeout(() => errorDiv.remove(), 5000);
}

function showSuccess(message) {
    const successDiv = document.createElement('div');
    successDiv.className = 'success-toast';
    successDiv.innerHTML = `
        <div class="success-icon">✓</div>
        <div>${message}</div>
    `;
    document.body.appendChild(successDiv);

    setTimeout(() => successDiv.remove(), 3000);
}
```

**CSS:**
```css
.error-toast, .success-toast {
    position: fixed;
    top: 20px;
    right: 20px;
    padding: 15px 20px;
    border-radius: 8px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
    display: flex;
    align-items: center;
    gap: 12px;
    z-index: 100000;
    animation: slideInRight 0.3s ease-out;
}

.error-toast {
    background: #f7768e;
    color: #fff;
}

.success-toast {
    background: #9ece6a;
    color: #1a1b26;
}

@keyframes slideInRight {
    from {
        transform: translateX(400px);
        opacity: 0;
    }
    to {
        transform: translateX(0);
        opacity: 1;
    }
}
```

### 6. Performance Optimizations

#### Image Lazy Loading
```html
<!-- Already added in Plan 02, verify implementation -->
<img src="logos/C.png"
     alt="C"
     class="lang-logo"
     loading="lazy"
     decoding="async">
```

#### Debounce Resize Events
```javascript
let resizeTimeout;
window.addEventListener('resize', function() {
    clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(function() {
        // Expensive resize operations here
        if (typeof resize === 'function') resize();
    }, 250);
});
```

#### Minimize Reflows
```javascript
// Batch DOM updates
function updateMultipleElements(updates) {
    // Use DocumentFragment to batch changes
    const fragment = document.createDocumentFragment();
    updates.forEach(update => {
        // ... create/modify elements in fragment
    });
    // Single DOM insertion
    container.appendChild(fragment);
}
```

### 7. Browser Compatibility

#### CSS Fallbacks
```css
.modal {
    /* Fallback for older browsers */
    background-color: rgba(0, 0, 0, 0.7);
    /* Modern blur effect */
    backdrop-filter: blur(4px);
    -webkit-backdrop-filter: blur(4px);
}

/* Flexbox fallbacks */
.modal-content {
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
}
```

#### Feature Detection
```javascript
// Check for backdrop-filter support
if (CSS.supports('backdrop-filter', 'blur(4px)')) {
    document.body.classList.add('supports-backdrop-filter');
}
```

### 8. Console Cleanup

Remove debug logs in production:
```javascript
// Wrap console.log calls
const DEBUG = false;
function debug(...args) {
    if (DEBUG) console.log(...args);
}

// Replace console.log with debug()
debug("Opening modal for:", lang);
```

## Testing Checklist

### Accessibility
- [ ] Keyboard navigation works (Tab, Enter, ESC)
- [ ] Focus trap in modal
- [ ] ARIA labels present
- [ ] Screen reader friendly
- [ ] Proper heading hierarchy

### Visual Polish
- [ ] Smooth animations
- [ ] Consistent spacing/padding
- [ ] Hover effects work
- [ ] Loading states visible
- [ ] Error messages clear

### Mobile
- [ ] Responsive on tablets
- [ ] Responsive on phones
- [ ] Touch targets large enough (min 44x44px)
- [ ] No horizontal scrolling
- [ ] Modal usable on small screens

### Performance
- [ ] Images lazy load
- [ ] No layout thrashing
- [ ] Smooth 60fps animations
- [ ] Quick page load (<2s)
- [ ] No console errors/warnings

### Cross-Browser
- [ ] Works in Chrome
- [ ] Works in Firefox
- [ ] Works in Safari (if on Mac)
- [ ] Works in Edge
- [ ] Graceful degradation for older browsers

## Tools for Testing

```bash
# Lighthouse audit
npx lighthouse http://localhost:9001 --view

# Check accessibility
npx pa11y http://localhost:9001

# Mobile simulation
# Use Chrome DevTools → Toggle device toolbar (Cmd+Shift+M)
```

## Files to Modify

1. `/Metrics/HTMLGenerator.ts` - Add polish CSS/JS
2. Test across different devices/browsers

## Acceptance Criteria

✅ Accessible via keyboard
✅ Smooth animations throughout
✅ Clear loading/error states
✅ Responsive on mobile/tablet
✅ No console errors
✅ Fast page load
✅ Professional look and feel
