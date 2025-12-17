# Plan 01: Fix Modal CSS & Behavior

**Objective:** Fix language detail modal to display as proper floating overlay instead of top-left corner inline element.

## Problem Analysis

### Current State
```html
<!-- HTMLGenerator.ts generates this -->
<div id="langModal" class="modal" style="display: none;" onclick="closeModal(event)">
    <div class="modal-content" id="modalContent">
        <!-- Modal content here -->
    </div>
</div>
```

### Missing CSS
The `.modal` class has **NO CSS** defined in HTMLGenerator.ts. Only `#logModal .modal-content` exists.

Result: When `display: none` → `display: block/flex`, the modal renders in normal document flow at top-left.

## Solution

### Task 1: Add Modal Base CSS

Add to HTMLGenerator.ts in the `<style>` section:

```css
/* Modal Base Styles */
.modal {
    display: none; /* Hidden by default */
    position: fixed;
    z-index: 10000;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(0, 0, 0, 0.7); /* Dark backdrop */
    backdrop-filter: blur(4px); /* Blur effect */
    overflow: auto;
    animation: fadeIn 0.2s ease-in;
}

.modal.active {
    display: flex !important;
    align-items: center;
    justify-content: center;
}

/* Modal Content Container */
.modal .modal-content {
    background: #1a1b26;
    border: 1px solid #414868;
    border-radius: 12px;
    padding: 0;
    max-width: 600px;
    max-height: 90vh;
    width: 90%;
    margin: auto;
    overflow: hidden;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.5);
    position: relative;
    animation: slideUp 0.3s ease-out;
}

/* Animations */
@keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
}

@keyframes slideUp {
    from {
        transform: translateY(50px);
        opacity: 0;
    }
    to {
        transform: translateY(0);
        opacity: 1;
    }
}

/* Prevent body scroll when modal open */
body.modal-open {
    overflow: hidden;
}

/* Close button styling */
.modal-close {
    position: absolute;
    right: 15px;
    top: 15px;
    font-size: 28px;
    font-weight: bold;
    color: #9aa5ce;
    cursor: pointer;
    z-index: 10;
    transition: color 0.2s;
}

.modal-close:hover {
    color: #f7768e;
}

/* Modal header improvements */
.modal-header {
    padding: 20px;
    border-bottom: 1px solid #414868;
    background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
}

/* Modal body */
.modal-body {
    padding: 20px;
    max-height: calc(90vh - 140px);
    overflow-y: auto;
}

/* Modal footer */
.modal-footer {
    padding: 15px 20px;
    border-top: 1px solid #414868;
    background: #1a1b26;
    display: flex;
    justify-content: flex-end;
    gap: 10px;
}

/* Responsive */
@media (max-width: 768px) {
    .modal .modal-content {
        width: 95%;
        max-height: 95vh;
    }
}
```

### Task 2: Update Modal JavaScript

Update the modal show/hide functions in HTMLGenerator.ts:

```javascript
window.showLanguageDetails = async function (lang, x, y) {
    console.log("Opening modal for:", lang);
    currentEditingLang = lang;
    const modal = document.getElementById('langModal');
    const modalContent = document.getElementById('modalContent');

    // Prevent body scroll
    document.body.classList.add('modal-open');

    // Load language data...
    // [existing code here]

    // Show modal
    modal.classList.add('active');
    modal.style.display = 'flex';

    // Focus trap
    modal.focus();
};

function closeModal(event) {
    if (!event) {
        const modal = document.getElementById('langModal');
        modal.classList.remove('active');
        modal.style.display = 'none';
        document.body.classList.remove('modal-open');
        return;
    }
    if (event.target.id === 'langModal' || event.target.classList.contains('modal-close')) {
        const modal = document.getElementById('langModal');
        modal.classList.remove('active');
        modal.style.display = 'none';
        document.body.classList.remove('modal-open');
    }
}

// ESC key handler
document.addEventListener('keydown', function(event) {
    if (event.key === 'Escape') {
        const modal = document.getElementById('langModal');
        if (modal.style.display === 'flex') {
            closeModal({});
        }
    }
});
```

### Task 3: Add Close Button to Modal

Update modal HTML structure to include explicit close button:

```html
<div id="langModal" class="modal" style="display: none;" onclick="closeModal(event)">
    <div class="modal-content" id="modalContent">
        <span class="modal-close" onclick="closeModal(event)">&times;</span>
        <div class="modal-header">
            <!-- existing header content -->
        </div>
        <div class="modal-body">
            <!-- existing body content -->
        </div>
        <div class="modal-footer">
            <button class="btn" onclick="closeModal(event)">Close</button>
        </div>
    </div>
</div>
```

## Testing Checklist

- [ ] Modal appears as centered floating window
- [ ] Dark backdrop covers page content
- [ ] Backdrop blur effect works
- [ ] Clicking backdrop closes modal
- [ ] ESC key closes modal
- [ ] Close button (×) closes modal
- [ ] Page doesn't scroll when modal open
- [ ] Smooth open/close animations
- [ ] No jerky browser movements
- [ ] Works on mobile (responsive)
- [ ] Z-index correct (above all content)
- [ ] Multiple open/close cycles work correctly

## Files to Modify

1. `/Metrics/HTMLGenerator.ts` - Add CSS and update JS functions
2. Regenerate report to test changes

## Acceptance Criteria

✅ Language detail modal displays as proper floating overlay
✅ Modal is centered on screen with dark backdrop
✅ No page jumping or jerky movements
✅ ESC key and backdrop click close modal
✅ Smooth animations
✅ Body scroll prevented when modal open
