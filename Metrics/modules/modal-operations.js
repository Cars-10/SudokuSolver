// Modal window management
import { trapFocus } from './utils.js';

// Close main language modal
export function closeModal(event) {
    if (!event || event.target.id === 'langModal' || event.target.classList.contains('modal-close')) {
        const modal = document.getElementById('langModal');
        modal.classList.remove('visible');
        document.body.classList.remove('modal-open');
    }
}

// Show methodology modal
export function showMethodology() {
    document.getElementById('methodModal').classList.add('visible');
}

// Close methodology modal
export function closeMethodology(event) {
    if (event.target.id === 'methodModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('methodModal').classList.remove('visible');
    }
}

// Show goals modal
export function showGoals() {
    document.getElementById('goalsModal').classList.add('visible');
}

// Close goals modal
export function closeGoals(event) {
    if (event.target.id === 'goalsModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('goalsModal').classList.remove('visible');
    }
}

// Show why modal
export function showWhy() {
    document.getElementById('whyModal').classList.add('visible');
}

// Close why modal
export function closeWhy(event) {
    if (event.target.id === 'whyModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('whyModal').classList.remove('visible');
    }
}

// Close source code modal
export function closeSourceModal(event) {
    if (!event || event.target.id === 'sourceModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('sourceModal').classList.remove('visible');
    }
}

// Close mismatch details modal
export function closeMismatchModal() {
    document.getElementById('diagnosticsModal').classList.remove('visible');
}

// Close score modal
export function closeScoreModal(event) {
    if (!event || event.target.id === 'scoreModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('scoreModal').classList.remove('visible');
    }
}

// Make a modal draggable
export function makeDraggable(modal, handle) {
    let isDragging = false;
    let startX, startY, initialX, initialY;

    handle.style.cursor = 'move';

    handle.addEventListener('mousedown', (e) => {
        if (e.target.classList.contains('modal-close')) return;

        isDragging = true;
        startX = e.clientX;
        startY = e.clientY;

        const rect = modal.getBoundingClientRect();
        initialX = rect.left;
        initialY = rect.top;

        modal.style.transition = 'none';
        e.preventDefault();
    });

    document.addEventListener('mousemove', (e) => {
        if (!isDragging) return;

        const dx = e.clientX - startX;
        const dy = e.clientY - startY;

        modal.style.left = (initialX + dx) + 'px';
        modal.style.top = (initialY + dy) + 'px';
        modal.style.transform = 'none';
    });

    document.addEventListener('mouseup', () => {
        if (isDragging) {
            isDragging = false;
            modal.style.transition = '';
        }
    });
}

// Generic element dragging (used by score modal, etc.)
export function makeElementDraggable(elmnt, header) {
    let pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;

    if (header) {
        header.onmousedown = dragMouseDown;
        header.style.cursor = 'move';
    } else {
        elmnt.onmousedown = dragMouseDown;
    }

    function dragMouseDown(e) {
        if (e.target.classList.contains('modal-close')) return;
        e = e || window.event;
        e.preventDefault();
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
        e = e || window.event;
        e.preventDefault();
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
        elmnt.style.transform = 'none';
    }

    function closeDragElement() {
        document.onmouseup = null;
        document.onmousemove = null;
    }
}

// Enable dragging for all modals
export function enableModalDragging() {
    const langModal = document.getElementById('langModal');
    const langModalContent = document.getElementById('modalContent');
    const langModalTitle = langModalContent?.querySelector('h2, .modal-title');
    if (langModal && langModalContent && langModalTitle) {
        makeDraggable(langModalContent, langModalTitle);
    }

    const methodModal = document.getElementById('methodModal');
    const methodModalContent = methodModal?.querySelector('.modal-content');
    const methodModalTitle = methodModalContent?.querySelector('.modal-title');
    if (methodModal && methodModalContent && methodModalTitle) {
        makeDraggable(methodModalContent, methodModalTitle);
    }

    const goalsModal = document.getElementById('goalsModal');
    const goalsModalContent = goalsModal?.querySelector('.modal-content');
    const goalsModalTitle = goalsModalContent?.querySelector('.modal-title');
    if (goalsModal && goalsModalContent && goalsModalTitle) {
        makeDraggable(goalsModalContent, goalsModalTitle);
    }

    const whyModal = document.getElementById('whyModal');
    const whyModalContent = whyModal?.querySelector('.modal-content');
    const whyModalTitle = whyModalContent?.querySelector('.modal-title');
    if (whyModal && whyModalContent && whyModalTitle) {
        makeDraggable(whyModalContent, whyModalTitle);
    }

    const sourceModal = document.getElementById('sourceModal');
    const sourceModalContent = sourceModal?.querySelector('.modal-content');
    const sourceModalTitle = sourceModalContent?.querySelector('.modal-title, h2');
    if (sourceModal && sourceModalContent && sourceModalTitle) {
        makeDraggable(sourceModalContent, sourceModalTitle);
    }

    const diagModal = document.getElementById('diagnosticsModal');
    const diagModalContent = diagModal?.querySelector('.modal-content');
    const diagModalTitle = diagModalContent?.querySelector('.modal-title');
    if (diagModal && diagModalContent && diagModalTitle) {
        makeDraggable(diagModalContent, diagModalTitle);
    }

    const scoreModal = document.getElementById('scoreModal');
    const scoreModalContent = scoreModal?.querySelector('.score-modal-content');
    const scoreModalTitle = scoreModalContent?.querySelector('h2');
    if (scoreModal && scoreModalContent && scoreModalTitle) {
        makeElementDraggable(scoreModalContent, scoreModalTitle);
    }
}
