// Narrator personality/voice management

export function changePersonality() {
    const selector = document.getElementById('personality-selector');
    const persona = selector.value || 'Standard'; // Fallback to Standard

    // Set global state for other components (like modals)
    window.currentPersona = persona;

    const intro = document.getElementById('personality-intro');

    // Update Intro Text
    intro.innerText = window.narratorIntros?.[persona] || window.narratorIntros?.['Standard'] || "Welcome to the Sudoku Benchmark.";

    // Update Tooltips
    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        const quotes = window.personalities?.[persona] || window.personalities?.['Standard'];
        let quote = quotes?.[lang] || quotes?.['default'] || "Unknown.";

        // Append Efficiency
        const score = row.getAttribute('data-score');
        const unit = row.getAttribute('data-time-unit') || 's';
        let time = parseFloat(row.getAttribute('data-time'));

        let timeStr;
        if (unit === 'ms') {
            timeStr = time.toFixed(2) + " ms";
        } else {
            timeStr = time < 0.0001 && time > 0 ? time.toExponential(2) + "s" : time.toFixed(6) + "s";
        }

        quote += " Efficiency: " + parseFloat(score).toFixed(2) + " MB/s | Time: " + timeStr;

        row.setAttribute('data-quote', quote);
    });

    // Update Methodology Modal
    const methodDesc = document.querySelector('#methodModal .modal-desc');
    if (methodDesc) {
        methodDesc.innerHTML = window.methodologyTexts?.[persona] || window.methodologyTexts?.['Standard'];
    }

    // Update Mismatch Button text based on active state
    const btn = document.getElementById('toggleMismatchesBtn');
    if (btn) {
        const curseWord = window.mismatchLabels?.[persona] || window.mismatchLabels?.['Standard'] || "MISMATCHES";
        if (btn.classList.contains('active')) {
            // Active = mismatches hidden, button shows "Show X" to reveal them
            btn.querySelector('span').textContent = "Show " + curseWord;
        } else {
            // Inactive = all visible, button shows "Hide X" to hide mismatches
            btn.querySelector('span').textContent = "Hide " + curseWord;
        }
    }

    // Update Labels (Column Headers / Sort Buttons)
    const iterLabel = window.iterationLabels?.[persona] || window.iterationLabels?.['Standard'] || "Iterations";
    const timeLabel = window.timeLabels?.[persona] || window.timeLabels?.['Standard'] || "Time";
    const memLabel = window.memoryLabels?.[persona] || window.memoryLabels?.['Standard'] || "Memory";
    const scoreLabel = window.scoreLabels?.[persona] || window.scoreLabels?.['Standard'] || "Score";

    document.querySelectorAll('.sort-iters').forEach(b => b.title = "Sort by " + iterLabel);
    document.querySelectorAll('.sort-time').forEach(b => b.title = "Sort by " + timeLabel);
    document.querySelectorAll('.sort-mem').forEach(b => b.title = "Sort by " + memLabel);
    document.querySelectorAll('.sort-score').forEach(b => b.title = "Sort by " + scoreLabel);

    // Update row mismatch titles (if any)
    const defaultIterLabel = window.iterationLabels?.['Standard'];
    document.querySelectorAll('.mismatch').forEach(span => {
        let title = span.title;
        // Replace the label part before the colon
        const parts = title.split(':');
        if (parts.length >= 2) {
            span.title = iterLabel + ":" + parts.slice(1).join(':');
        }
    });
}
