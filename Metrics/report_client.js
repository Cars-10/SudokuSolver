let currentSort = { metric: '', dir: 1 }; // 1 = Asc, -1 = Desc (empty metric allows first click to set natural direction)

// Docker mode - auto-detect based on port (9001 = Docker, 9002 = Local)
const useDockerMode = window.location.port === '9001';
const isDockerPort = window.location.port === '9001';
const isLocalPort = window.location.port === '9002';
console.log(`Execution mode: ${useDockerMode ? 'Docker (port 9001)' : 'Local (port 9002)'}`);

// Switch execution mode by navigating to different port
window.switchExecutionMode = function() {
    const currentPort = window.location.port;
    const newPort = currentPort === '9001' ? '9002' : '9001';
    const newUrl = window.location.protocol + '//' + window.location.hostname + ':' + newPort + window.location.pathname;
    window.location.href = newUrl;
};

// Initialize Docker/Local button based on current port
document.addEventListener('DOMContentLoaded', function() {
    const icon = document.getElementById('dockerIcon');
    const label = document.getElementById('dockerLabel');
    const btn = document.getElementById('dockerToggleBtn');

    if (icon && label && btn) {
        if (isDockerPort) {
            icon.textContent = '🐳';
            label.textContent = 'Docker';
            btn.style.background = 'rgba(0, 150, 255, 0.3)';
            btn.style.borderColor = '#0096ff';
            btn.title = 'Currently on Docker (port 9001). Click to switch to Local.';
        } else if (isLocalPort) {
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
});

// Helper to normalize matrix identifiers (handles both "1" and "1.matrix" formats)
function normalizeMatrix(m) {
    return String(m).replace('.matrix', '');
}

// Calculate and update relative times (e.g., "3d ago", "2h ago")
function updateRelativeTimes() {
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

// Update relative times on load and periodically
document.addEventListener('DOMContentLoaded', updateRelativeTimes);
setInterval(updateRelativeTimes, 60000); // Update every minute

// Search Logic
function filterLanguages() {
    const input = document.getElementById('search-input');
    const filter = input.value.toUpperCase();
    const tbody = document.getElementById('mainTableBody');
    const rows = tbody.getElementsByTagName('tr');

    for (let i = 0; i < rows.length; i++) {
        const row = rows[i];
        const lang = row.getAttribute('data-lang');
        if (lang) {
            if (lang.toUpperCase().indexOf(filter) > -1) {
                row.style.display = "";
            } else {
                row.style.display = "none";
            }
        }
    }
}

// Sorting Logic
function sortRows(metric, btn) {
    console.log('sortRows called with metric:', metric, 'btn:', btn);
    const tbody = document.getElementById('mainTableBody');
    if (!tbody) {
        console.error('mainTableBody not found!');
        return;
    }
    // Only get main data rows, not expanded-content rows
    const rows = Array.from(tbody.querySelectorAll('tr[data-lang]'));
    console.log('Found', rows.length, 'rows to sort');

    // Capture row pairs BEFORE sorting (each data row with its expanded-content)
    const rowPairs = rows.map(row => {
        const expandedRow = row.nextElementSibling;
        return {
            dataRow: row,
            expandedRow: (expandedRow && expandedRow.classList.contains('expanded-content')) ? expandedRow : null
        };
    });

    // Toggle direction or set natural direction for new column
    if (currentSort.metric === metric) {
        currentSort.dir *= -1;
    } else {
        currentSort.metric = metric;
        // Natural direction: ascending for names, ascending (lowest first) for numeric values
        currentSort.dir = 1;
    }

    // Update buttons and table headers
    document.querySelectorAll('.btn, .sort-btn').forEach(b => {
        b.classList.remove('active');
        b.classList.remove('rotate-180');
    });

    // Update sortable table headers
    document.querySelectorAll('.sortable-header').forEach(th => {
        th.classList.remove('active');
        th.classList.remove('desc');
    });

    if (btn) {
        btn.classList.add('active');
        if (currentSort.dir === -1) {
            btn.classList.add('desc');  // CSS rotates the .sort-arrow inside via .desc class
        }
    }

    rowPairs.sort((a, b) => {
        const aVal = a.dataRow.getAttribute('data-' + metric);
        const bVal = b.dataRow.getAttribute('data-' + metric);

        // Special handling for score: N/A (value 0) stays at bottom regardless of direction
        if (metric === 'score') {
            const aScore = parseFloat(aVal);
            const bScore = parseFloat(bVal);
            // If both are 0 (N/A), keep original order
            if (aScore === 0 && bScore === 0) return 0;
            // N/A scores always go to bottom
            if (aScore === 0) return 1;
            if (bScore === 0) return -1;
            return (aScore - bScore) * currentSort.dir;
        }

        if (metric === 'lang') {
            return aVal.localeCompare(bVal) * currentSort.dir;
        } else if (metric === 'year') {
            return (parseInt(aVal) - parseInt(bVal)) * currentSort.dir;
        } else {
            return (parseFloat(aVal) - parseFloat(bVal)) * currentSort.dir;
        }
    });

    // Re-append rows with their expanded-content siblings
    rowPairs.forEach(pair => {
        tbody.appendChild(pair.dataRow);
        if (pair.expandedRow) {
            tbody.appendChild(pair.expandedRow);
        }
    });
}

function sortMatrix(index, metric, btn) {
    const tbody = document.getElementById('mainTableBody');
    // Only get main data rows, not expanded-content rows
    const rows = Array.from(tbody.querySelectorAll('tr[data-lang]'));

    // Capture row pairs BEFORE sorting (each data row with its expanded-content)
    const rowPairs = rows.map(row => {
        const expandedRow = row.nextElementSibling;
        return {
            dataRow: row,
            expandedRow: (expandedRow && expandedRow.classList.contains('expanded-content')) ? expandedRow : null
        };
    });

    const attr = 'data-m' + index + '-' + metric;
    const fullMetric = 'm' + index + '_' + metric;

    if (currentSort.metric === fullMetric) {
        currentSort.dir *= -1;
    } else {
        currentSort.metric = fullMetric;
        currentSort.dir = metric === 'time' || metric === 'score' ? 1 : -1;
    }

    // Update buttons and table headers
    document.querySelectorAll('.btn, .sort-btn').forEach(b => {
        b.classList.remove('active');
        b.classList.remove('rotate-180');
    });

    // Update sortable table headers
    document.querySelectorAll('.sortable-header').forEach(th => {
        th.classList.remove('active');
        th.classList.remove('desc');
    });

    if (btn) {
        btn.classList.add('active');
        if (currentSort.dir === -1) {
            btn.classList.add('desc');  // CSS rotates the .sort-arrow inside via .desc class
        }
    }

    rowPairs.sort((a, b) => {
        const aVal = parseFloat(a.dataRow.getAttribute(attr));
        const bVal = parseFloat(b.dataRow.getAttribute(attr));
        return (aVal - bVal) * currentSort.dir;
    });

    // Re-append rows with their expanded-content siblings
    rowPairs.forEach(pair => {
        tbody.appendChild(pair.dataRow);
        if (pair.expandedRow) {
            tbody.appendChild(pair.expandedRow);
        }
    });
}

// Mismatch Filter - Hides mismatches when active (clean view), shows all when inactive
function toggleMismatches() {
    const btn = document.getElementById('toggleMismatchesBtn');
    const isHidingMismatches = btn.classList.toggle('active');

    const selector = document.getElementById('personality-selector');
    const persona = selector ? selector.value : 'Standard';
    const curseWord = mismatchLabels[persona] || mismatchLabels['Standard'] || "MISMATCHES";

    if (isHidingMismatches) {
        // Active = mismatches are hidden
        btn.classList.add('filter-active-red');
        btn.querySelector('span').textContent = "Show " + curseWord;
    } else {
        // Inactive = all rows visible
        btn.classList.remove('filter-active-red');
        btn.querySelector('span').textContent = "Hide " + curseWord;
    }

    // Only affect data rows (those with data-lang), not expanded-content rows
    const rows = document.querySelectorAll('tbody tr[data-lang]');
    rows.forEach(row => {
        const expandedRow = row.nextElementSibling;
        const isMismatch = row.classList.contains('mismatch-iterations');

        if (isHidingMismatches && isMismatch) {
            // Hide mismatch rows when filter is active
            row.style.display = 'none';
            if (expandedRow?.classList.contains('expanded-content')) {
                expandedRow.style.display = 'none';
            }
        } else {
            // Show all other rows
            row.style.display = '';
            if (expandedRow?.classList.contains('expanded-content')) {
                expandedRow.style.display = row.classList.contains('expanded') ? 'table-row' : 'none';
            }
        }
    });
}

// Personality Selector
function changePersonality() {
    const selector = document.getElementById('personality-selector');
    const persona = selector.value || 'Standard'; // Fallback to Standard

    // Set global state for other components (like modals)
    window.currentPersona = persona;

    const intro = document.getElementById('personality-intro');

    // Update Intro Text
    intro.innerText = narratorIntros[persona] || narratorIntros['Standard'] || "Welcome to the Sudoku Benchmark.";

    // Update Badge Styles to match persona? (Optional, skipping for now)

    // Update Tooltips
    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        const quotes = personalities[persona] || personalities['Standard'];
        let quote = quotes[lang] || quotes['default'] || "Unknown.";

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
        methodDesc.innerHTML = methodologyTexts[persona] || methodologyTexts['Standard'];
    }

    // Update Mismatch Button text based on active state
    const btn = document.getElementById('toggleMismatchesBtn');
    if (btn) {
        const curseWord = mismatchLabels[persona] || mismatchLabels['Standard'] || "MISMATCHES";
        if (btn.classList.contains('active')) {
            // Active = mismatches hidden, button shows "Show X" to reveal them
            btn.querySelector('span').textContent = "Show " + curseWord;
        } else {
            // Inactive = all visible, button shows "Hide X" to hide mismatches
            btn.querySelector('span').textContent = "Hide " + curseWord;
        }
    }

    // Update Labels (Column Headers / Sort Buttons)
    const iterLabel = iterationLabels[persona] || iterationLabels['Standard'] || "Iterations";
    const timeLabel = timeLabels[persona] || timeLabels['Standard'] || "Time";
    const memLabel = memoryLabels[persona] || memoryLabels['Standard'] || "Memory";
    const scoreLabel = scoreLabels[persona] || scoreLabels['Standard'] || "Score";

    document.querySelectorAll('.sort-iters').forEach(b => b.title = "Sort by " + iterLabel);
    document.querySelectorAll('.sort-time').forEach(b => b.title = "Sort by " + timeLabel);
    document.querySelectorAll('.sort-mem').forEach(b => b.title = "Sort by " + memLabel);
    document.querySelectorAll('.sort-score').forEach(b => b.title = "Sort by " + scoreLabel);

    // Update row mismatch titles (if any)
    // We need to preserve the values but replace the label part
    // Title format: "Iterations: X vs C: Y"
    const defaultIterLabel = iterationLabels['Standard'];
    document.querySelectorAll('.mismatch').forEach(span => {
        let title = span.title;
        if (title.includes(defaultIterLabel)) {
            // Simple replace might be risky if "Iterations" appears elsewhere?
            // But here it's specifically in the title we set.
            // Let's regex replace the label part.
            title = title.replace(new RegExp('^' + defaultIterLabel), iterLabel);
            // Also handle previously set custom labels? 
            // Actually, we should probably store the values in data attributes to be clean, 
            // but parsing is fine for now since we control the format.
            // Better yet, just replace everything before the colon?
            // Format: "Label: 123 vs C: 456"
            const parts = title.split(':');
            if (parts.length >= 2) {
                span.title = iterLabel + ":" + parts.slice(1).join(':');
            }
        } else {
            // Maybe it was already renamed? Try to find existing label?
            // This is tricky without data attributes. 
            // Let's assume we replace the part before the first colon.
            const parts = title.split(':');
            if (parts.length >= 2) {
                span.title = iterLabel + ":" + parts.slice(1).join(':');
            }
        }
    });

}

// Tooltip Logic
const tooltip = document.getElementById('tooltip');

// Attach to all cells


document.querySelectorAll('tbody td').forEach(cell => {
    cell.addEventListener('mousemove', (e) => {
        const row = cell.parentElement;
        const lang = row.getAttribute('data-lang');

        let content = "";

        if (cell.classList.contains('lang-col')) {
            // Language Cell -> Show History
            const history = row.getAttribute('data-history');
            if (history) {
                content = '<strong style="color: var(--primary)">' + lang + '</strong><br>' +
                    '<div style="max-width: 250px; white-space: normal; margin-top: 5px;">' + history + '</div>';
            }
        } else if (cell.classList.contains('matrix-cell')) {
            // Matrix Cell -> Detailed Metrics
            const matrixIdx = parseInt(cell.getAttribute('data-matrix-index'));
            const time = row.getAttribute('data-m' + matrixIdx + '-time');
            const iters = row.getAttribute('data-m' + matrixIdx + '-iters');
            const mem = row.getAttribute('data-m' + matrixIdx + '-mem');
            const score = row.getAttribute('data-m' + matrixIdx + '-score');

            if (time && time !== '999999') {
                const memMb = (parseFloat(mem) / 1024 / 1024).toFixed(1);
                const unit = row.getAttribute('data-time-unit') || 's';
                const timeDisplay = unit === 'ms' ? parseFloat(time).toFixed(2) + ' ms' : parseFloat(time).toFixed(5) + 's';

                content = '<strong style="color: var(--primary)">Matrix ' + (matrixIdx + 1) + '</strong><br>' +
                    '<span style="color: var(--secondary)">' + lang + '</span><br>' +
                    '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                    'Time: <span style="color: #fff">' + timeDisplay + '</span><br>' +
                    'Score: <span style="color: ' + (parseFloat(score) <= 1 ? 'var(--primary)' : '#ff0055') + '">' + score + 'x</span><br>' +
                    'Iters: ' + parseInt(iters).toLocaleString() + '<br>' +
                    'Mem: ' + memMb + ' MB';
            } else {
                content = row.getAttribute('data-quote');
            }
        } else if (cell.classList.contains('score-col')) {
            // Score Cell -> Show Breakdown + Quote
            const breakdown = row.getAttribute('data-score-breakdown');
            const quote = row.getAttribute('data-quote');
            content = '<strong style="color: var(--primary)">Composite Score</strong><br>' +
                '<span style="font-size: 0.8em; color: var(--secondary)">' + breakdown + '</span><br>' +
                '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                quote;
        }

        if (content) {
            const tooltip = document.getElementById('tooltip');
            tooltip.style.display = 'block';
            tooltip.innerHTML = content;
        }
    });

    cell.addEventListener('mousemove', (e) => {
        const tooltip = document.getElementById('tooltip');
        if (tooltip.style.display === 'block') {
            tooltip.style.left = (e.clientX + 15) + 'px';
            tooltip.style.top = (e.clientY + 15) + 'px';
        }
    });

    cell.addEventListener('mouseleave', () => {
        const tooltip = document.getElementById('tooltip');
        tooltip.style.display = 'none';
    });
});

// Add click handler to language cells for modal
document.addEventListener('DOMContentLoaded', function () {
    document.querySelectorAll('.lang-col').forEach(cell => {
        cell.addEventListener('click', (e) => {
            // If click was on button, don't handle
            if (e.target.closest('button')) {
                return;
            }

            e.preventDefault();
            e.stopPropagation();

            const row = cell.parentElement;
            const lang = row.getAttribute('data-lang');

            // Clicking on language name/logo opens the modal
            if (lang && typeof window.showLanguageDetails === 'function') {
                window.showLanguageDetails(lang, e.clientX, e.clientY);
            }
        });
        cell.style.cursor = 'pointer';
    });

    // Prevent clicks on expanded content from bubbling
    document.querySelectorAll('.expanded-content').forEach(row => {
        row.addEventListener('click', (e) => {
            e.stopPropagation();
        });
    });
});

// Modal Logic
let currentEditingLang = null;
let currentMetadata = null;
let lastPopulatedMetadata = null; // Track the last saved state to prevent overwriting during edits

// Updated Show Function
window.showLanguageDetails = async function (lang, x, y) {
    console.log("Opening modal for:", lang, "at", x, y);
    currentEditingLang = lang;
    const modal = document.getElementById('langModal');
    const modalContent = document.getElementById('modalContent');

    // Add visible class to modal
    modal.classList.add('visible');
    document.body.classList.add('modal-open');

    // Reset scroll
    modalContent.scrollTop = 0;

    // Positioning logic - always position at top of viewport, horizontally centered
    modal.classList.remove('centered');

    // Initial visibility hidden to measure
    modalContent.style.visibility = 'hidden';
    modalContent.style.display = 'flex';

    // Use requestAnimationFrame to ensure it's rendered for measurement
    requestAnimationFrame(() => {
        const width = modalContent.offsetWidth;
        const padding = 20;

        // Center horizontally
        let left = (window.innerWidth - width) / 2;
        left = Math.max(padding, Math.min(left, window.innerWidth - width - padding));

        // Position at top of viewport with padding
        const top = padding;

        modalContent.style.left = left + 'px';
        modalContent.style.top = top + 'px';
        modalContent.style.transform = 'none';
        modalContent.style.visibility = 'visible';
        modalContent.style.margin = '0';
    });
    // We fallback to static if fetch fails.
    let meta = { ...(languageMetadata[lang] || {}) };

    try {
        const res = await fetch(`/api/metadata/${lang}`);
        if (res.ok) {
            const dynamicMeta = await res.json();
            // Merge: dynamic takes precedence for user-edited fields
            meta = { ...meta, ...dynamicMeta };
        }
    } catch (e) {
        console.warn("Could not fetch dynamic metadata:", e);
    }

    currentMetadata = meta;
    lastPopulatedMetadata = JSON.parse(JSON.stringify(meta)); // Deep copy for comparison/restoration

    // View Mode Population
    // Use the same logo as the table (pre-calculated in metricsData)
    const tableData = (typeof metricsData !== 'undefined') ? metricsData.find(m => m.solver === lang) : null;
    const img = tableData?.logo || meta.image || meta.logo || "";

    const modalImg = document.getElementById('modalImg');
    modalImg.src = img;

    // Apply tailoring to modal logo
    const tailoringData = (typeof tailoring !== 'undefined') ? tailoring : {};
    const langTailoring = tailoringData[lang];
    if (langTailoring?.invert) {
        modalImg.style.filter = "url(#filter-invert)";
    } else if (langTailoring?.transparent_white) {
        modalImg.style.filter = "url(#filter-transparent-white)";
    } else {
        modalImg.style.filter = "none";
    }

    const displayName = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
    document.getElementById('modalTitle').innerText = displayName;
    document.getElementById('modalSubtitle').innerText = (meta.creator || "?") + " • " + (meta.date || "????");
    document.getElementById('modalLocation').innerText = "📍 " + (meta.location || "Unknown Location");
    document.getElementById('modalBenefits').innerText = "✨ " + (meta.benefits || "Unknown Benefits");
    document.getElementById('modalRelated').innerText = meta.related ? "🔗 Related: " + meta.related : "";
    document.getElementById('modalParadigm').innerText = meta.paradigm || "-";
    document.getElementById('modalTypeSystem').innerText = meta.typeSystem || "-";

    // History: Check metadata first, then languageHistories fallback
    // The HTMLGenerator injects historyData and languageHistories/metadata
    // But we are in client JS. We used to rely on data-history attribute.
    // Let's use meta.history first.
    const historyText = meta.history || ""; 
    const historyContainer = document.getElementById('historySection');
    const historyEl = document.getElementById('modalHistory');
    
    if (historyText) {
        historyEl.innerText = historyText;
        historyContainer.style.display = 'block';
    } else {
        historyContainer.style.display = 'none';
    }


    // Description: Check Persona first
    let desc = meta.description;
    console.log("[DEBUG] meta.description:", meta.description);
    console.log("[DEBUG] full meta:", meta);
    const currentPersona = window.currentPersona || 'Standard';
    if (window.personalities && window.personalities[currentPersona]) {
        // Look for specific lang match
        const personaDesc = window.personalities[currentPersona][lang];
        if (personaDesc) {
            desc = personaDesc;
        } else {
            // Fallback to default if available in persona?
            // "default" key in personalities map
            const def = window.personalities[currentPersona]['default'];
            if (def && desc === undefined) {
                // Only use default if no meta description? Or append?
                // Let's use generic description if no specific language description exists?
                // But generic description is like "A glitch in the matrix".
                // It might overwrite a useful description like "A compiled language..."
                // User asked to embellish. Let's PREPEND or REPLACE?
                // The existing personality entries are full replacements like "C: The Source Code...".
                // So if we have a match, we use it. If not, we keep the original description?
                // Let's keep original description if no specific match, maybe append the persona flavor text.
                if (def) desc = (desc || "") + "\n\n" + def;
            }
        }
    }

    console.log("[DEBUG] Final desc to display:", desc);
    document.getElementById('modalDesc').innerText = desc || "No description available.";

    // Set button URLs
    const btnWebsite = document.getElementById('btn-website');
    const btnGrokipedia = document.getElementById('btn-grokipedia');
    const btnWikipedia = document.getElementById('btn-wikipedia');

    if (meta.website) {
        btnWebsite.href = meta.website;
        btnWebsite.style.display = 'inline-block';
    } else {
        btnWebsite.style.display = 'none';
    }

    // Fix Grokipedia Link
    btnGrokipedia.href = `https://grokipedia.com/search?q=${encodeURIComponent(displayName + " programming language")}`;
    btnWikipedia.href = `https://en.wikipedia.org/wiki/${encodeURIComponent(displayName)}_programming_language`;

    // Edit Mode Population
    document.getElementById('editInputs-title').value = displayName;
    document.getElementById('editInputs-creator').value = meta.creator || "";
    document.getElementById('editInputs-image').value = img;
    document.getElementById('editInputs-date').value = meta.date || "";
    document.getElementById('editInputs-location').value = meta.location || "";
    document.getElementById('editInputs-benefits').value = meta.benefits || "";
    document.getElementById('editInputs-related').value = meta.related || "";
    document.getElementById('editInputs-website').value = meta.website || "";
    document.getElementById('editInputs-desc').value = meta.description || "";
    document.getElementById('editInputs-history').value = meta.history || "";
    document.getElementById('editInputs-paradigm').value = meta.paradigm || "";
    document.getElementById('editInputs-typeSystem').value = meta.typeSystem || "";

    // Authors Population
    const authorList = document.getElementById('authorList');
    authorList.innerHTML = '';

    let authors = meta.authors || [];
    // If no authors array, create one from creator field
    if (authors.length === 0 && meta.creator) {
        // Split creators by comma (e.g., "Aho, Weinberger, Kernighan")
        const creatorNames = meta.creator.split(',').map(n => n.trim());
        authors = creatorNames.map((name, idx) => ({
            name: name,
            image: idx === 0 ? (meta.image || meta.logo || '') : '' // First creator gets the main image
        }));
    }
    // IMPORTANT: Assign back to meta so currentMetadata has the initialized array
    meta.authors = authors;

    window.renderAuthorList();

    // Clear editing mode
    modalContent.classList.remove('editing');
    document.getElementById('editBtn').innerText = "Edit";

    // Update Lock Button State
    const lockBtn = document.getElementById('lockBtn');
    if (lockBtn) {
        const isLocked = window.lockedLanguages && window.lockedLanguages.has(lang);
        if (isLocked) {
            lockBtn.innerHTML = "🔒 Locked";
            lockBtn.title = "Unlock this result";
        } else {
            lockBtn.innerHTML = "🔓 Unlocked";
            lockBtn.title = "Lock this result to skip future benchmarks";
        }
    }

    // Ensure modal is visible
    modal.classList.add('visible');

    // Add modal-open class to body to prevent scrolling
    document.body.classList.add('modal-open');
};

window.toggleEditMode = function (event) {
    // Prevent click from bubbling to modal container
    if (event) {
        event.stopPropagation();
        event.preventDefault(); // Stop default action (link navigation if any)
    }

    const content = document.getElementById('modalContent');
    const btn = document.getElementById('editBtn');
    if (content.classList.contains('editing')) {
        content.classList.remove('editing');
        btn.innerText = "Edit";
    } else {
        content.classList.add('editing');
        btn.innerText = "Cancel";
    }
};

window.toggleLockFromModal = function (event) {
    if (event) event.stopPropagation();

    if (!currentEditingLang) return;

    // Toggle lock state
    window.toggleLock(currentEditingLang);

    // Update button text
    const btn = document.getElementById('lockBtn');
    const isLocked = window.lockedLanguages.has(currentEditingLang);

    if (isLocked) {
        btn.innerHTML = "🔒 Locked";
        btn.title = "Unlock this result";
    } else {
        btn.innerHTML = "🔓 Unlocked";
        btn.title = "Lock this result to skip future benchmarks";
    }
};

window.saveLanguageDetails = async function (event) {
    // Prevent click from bubbling to modal container
    if (event) {
        event.stopPropagation();
    }

    if (!currentEditingLang) return;

    const newData = {
        creator: document.getElementById('editInputs-creator').value,
        image: document.getElementById('editInputs-image').value,
        date: document.getElementById('editInputs-date').value,
        location: document.getElementById('editInputs-location').value,
        benefits: document.getElementById('editInputs-benefits').value,
        related: document.getElementById('editInputs-related').value,
        website: document.getElementById('editInputs-website').value,
        description: document.getElementById('editInputs-desc').value,
        history: document.getElementById('editInputs-history').value,
        paradigm: document.getElementById('editInputs-paradigm').value,
        typeSystem: document.getElementById('editInputs-typeSystem').value,
        authors: currentMetadata.authors || []
    };

    // Save to backend
    try {
        console.log("Attempting to save metadata to:", '/api/save-metadata');
        const res = await fetch('/api/save-metadata', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ lang: currentEditingLang, metadata: newData })
        });

        if (!res.ok) {
            const errorText = await res.text();
            throw new Error(`Server responded with ${res.status}: ${errorText}`);
        }

        const json = await res.json();
        // Update local state immediately
        if (!languageMetadata[currentEditingLang]) languageMetadata[currentEditingLang] = {};
        Object.assign(languageMetadata[currentEditingLang], newData);

        const statusEl = document.getElementById('personality-intro');
        if (statusEl) {
            statusEl.innerHTML = '<span style="color: var(--primary); font-weight: bold;">✓ Metadata saved. Regenerating report...</span>';
        }

        // Wait for report regeneration
        try {
            await fetch('/api/generate-report', { method: 'POST' });
            if (statusEl) {
                statusEl.innerHTML = '<span style="color: var(--primary); font-weight: bold;">✓ Report regenerated. Reloading...</span>';
            }
        } catch (err) {
            console.warn("Report generation failed:", err);
        }

        closeModal(null);

        // Reload to reflect changes
        setTimeout(() => window.location.reload(), 500);
    } catch (e) {
        console.error("Save failed:", e);
        alert("Error saving: " + e.message + "\nCheck console for details.");
    }
};

window.renderAuthorList = function () {
    const authorList = document.getElementById('authorList');
    if (!authorList || !currentMetadata || !currentMetadata.authors) return;

    authorList.innerHTML = '';

    currentMetadata.authors.forEach((auth, idx) => {
        const div = document.createElement('div');
        div.className = 'author-item';
        div.innerHTML = `
            <div class="modal-img-container" style="width: 80px; height: 80px; position: relative; border-radius: 50%; overflow: hidden; margin-bottom: 5px;">
                <img src="${auth.image || ''}" class="author-img" id="author-img-${idx}" style="width: 100%; height: 100%; object-fit: cover;">
                <div class="edit-only" style="position: absolute; bottom: 0; left: 0; right: 0; background: rgba(0,0,0,0.7); text-align: center; padding: 2px; font-size: 0.6em; cursor: pointer; color: #fff;" onclick="handleAuthorImageChange(${idx}, event)">
                    Change
                </div>
            </div>
            <span class="view-only">${auth.name}</span>
            <div class="edit-only" style="width: 100%;">
                <input type="text" class="modal-edit-input" value="${auth.name}" placeholder="Name" onchange="updateAuthor(${idx}, 'name', this.value)" style="margin-bottom: 2px;">
                <input type="text" class="modal-edit-input" id="author-input-${idx}" value="${auth.image}" placeholder="Image URL" onchange="checkAndDownloadAuthorImage(${idx}, this.value)">
                <button class="btn" style="background:#ff5555; padding: 2px 0; font-size: 0.7em; width: 100%; margin-top: 2px;" onclick="removeAuthor(${idx})">Remove</button>
            </div>
        `;
        authorList.appendChild(div);
    });
};

window.addAuthorField = function () {
    if (!currentMetadata.authors) currentMetadata.authors = [];
    currentMetadata.authors.push({ name: "New Author", image: "" });
    window.renderAuthorList();
};

window.removeAuthor = function (idx) {
    if (!currentMetadata.authors) return;
    currentMetadata.authors.splice(idx, 1);
    window.renderAuthorList();
};

window.updateAuthor = function (idx, field, value) {
    if (!currentMetadata.authors[idx]) return;
    currentMetadata.authors[idx][field] = value;
};

window.openGoogleImageSearch = function () {
    if (currentEditingLang) {
        window.open(`https://www.google.com/search?tbm=isch&q=${encodeURIComponent(currentEditingLang + " programming language logo")}`, '_blank');
    }
};

window.uploadBlob = async function (blob, isLogo = false) {
    const formData = new FormData();
    formData.append('file', blob);
    formData.append('lang', currentEditingLang);
    if (isLogo) {
        formData.append('isLogo', 'true');  // Flag to save as <lang>_logo.<ext>
    }

    try {
        const res = await fetch('/api/upload-media', {
            method: 'POST',
            body: formData
        });

        if (res.ok) {
            const data = await res.json();
            const relativePath = `Algorithms/BruteForce/${currentEditingLang}/Media/${data.filename}`;
            return relativePath;
        } else {
            const txt = await res.text();
            alert("Upload failed: " + txt);
            return null;
        }
    } catch (e) {
        console.error(e);
        alert("Upload failed: " + e.message);
        return null;
    }
};

window.handleLogoChange = async function (event) {
    if (event) event.stopPropagation();

    // Try reading clipboard first
    try {
        const clipboardItems = await navigator.clipboard.read();
        for (const item of clipboardItems) {
            const imageTypes = item.types.filter(type => type.startsWith('image/'));
            if (imageTypes.length > 0) {
                const blob = await item.getType(imageTypes[0]);
                // Show loading on main logo
                const imgEl = document.getElementById('modalImg');
                imgEl.style.opacity = '0.5';

                const relativePath = await uploadBlob(blob, true);  // true = isLogo

                if (relativePath) {
                    imgEl.src = relativePath;
                    document.getElementById('editInputs-image').value = relativePath;
                    if (currentMetadata) currentMetadata.image = relativePath;
                }
                imgEl.style.opacity = '1';
                return;
            }
        }
    } catch (err) {
        console.log("Clipboard access not available or empty, falling back to file picker.");
    }

    // Fallback
    document.getElementById('logoInput').click();
};

window.handleAuthorImageChange = async function (index, event) {
    if (event) event.stopPropagation();

    // Try reading clipboard first
    try {
        const clipboardItems = await navigator.clipboard.read();
        for (const item of clipboardItems) {
            const imageTypes = item.types.filter(type => type.startsWith('image/'));
            if (imageTypes.length > 0) {
                const blob = await item.getType(imageTypes[0]);

                // Show loading
                const imgEl = document.getElementById(`author-img-${index}`);
                if (imgEl) imgEl.style.opacity = '0.5';

                const relativePath = await uploadBlob(blob);

                if (relativePath) {
                    // Update Author Data
                    updateAuthor(index, 'image', relativePath);
                    // Update DOM
                    if (imgEl) imgEl.src = relativePath;
                    const inputEl = document.getElementById(`author-input-${index}`);
                    if (inputEl) inputEl.value = relativePath;
                }

                if (imgEl) imgEl.style.opacity = '1';
                return;
            }
        }
    } catch (err) {
        console.log("Clipboard access not available, falling back to file input");
    }

    // Fallback to hidden input specific for authors
    // We need to set a global or data attribute to know WHICH author we are updating
    window.currentAuthorIndex = index;
    document.getElementById('authorFileInput').click();
};

// Paste Listener for Modal
document.addEventListener('paste', async (e) => {
    console.log('Paste event detected');

    // Only if modal is open and editing
    const modal = document.getElementById('langModal');
    const modalContent = document.getElementById('modalContent');

    if (!modal || !modal.classList.contains('visible')) {
        console.log('Paste ignored: modal not visible');
        return;
    }
    if (!modalContent || !modalContent.classList.contains('editing')) {
        console.log('Paste ignored: not in edit mode. Classes:', modalContent?.classList?.toString());
        return;
    }

    console.log('Paste accepted: modal visible and in edit mode');

    // Check for active element (Author Input)
    const activeEl = document.activeElement;
    let targetAuthorIdx = -1;
    if (activeEl && activeEl.id && activeEl.id.startsWith('author-input-')) {
        targetAuthorIdx = parseInt(activeEl.id.replace('author-input-', ''));
    }

    const clipboardData = e.clipboardData || e.originalEvent?.clipboardData;
    if (!clipboardData) {
        console.log('Paste failed: no clipboard data available');
        return;
    }

    const items = clipboardData.items;
    console.log('Clipboard items:', items?.length || 0);

    // First pass: check if there's an image file to handle
    let hasImageFile = false;
    for (let index in items) {
        console.log(`Item ${index}: kind=${items[index].kind}, type=${items[index].type}`);
        if (items[index].kind === 'file' && items[index].type.startsWith('image/')) {
            hasImageFile = true;
        }
    }

    console.log('Has image file:', hasImageFile);

    // Prevent default if we're going to handle an image
    if (hasImageFile) {
        e.preventDefault();
        e.stopPropagation();
    }

    for (let index in items) {
        const item = items[index];
        if (item.kind === 'file') {
            const blob = item.getAsFile();
            console.log('Processing file blob:', blob?.type, blob?.size);

            if (targetAuthorIdx >= 0) {
                // Upload for Author
                const imgEl = document.getElementById(`author-img-${targetAuthorIdx}`);
                if (imgEl) imgEl.style.opacity = '0.5';
                const relativePath = await uploadBlob(blob);
                if (relativePath) {
                    updateAuthor(targetAuthorIdx, 'image', relativePath);
                    // Add cache-buster to force image reload
                    if (imgEl) imgEl.src = relativePath + '?t=' + Date.now();
                    activeEl.value = relativePath;
                }
                if (imgEl) imgEl.style.opacity = '1';
            } else {
                // Upload for Main Logo
                const imgEl = document.getElementById('modalImg');
                imgEl.style.opacity = '0.5';
                const relativePath = await uploadBlob(blob, true);  // true = isLogo
                if (relativePath) {
                    // Add cache-buster to force image reload
                    imgEl.src = relativePath + '?t=' + Date.now();
                    document.getElementById('editInputs-image').value = relativePath;
                    if (currentMetadata) currentMetadata.image = relativePath;
                }
                imgEl.style.opacity = '1';
            }
        } else if (item.kind === 'string') {
            item.getAsString(async (url) => {
                if (url.match(/\.(jpeg|jpg|gif|png|webp)$/i) || url.startsWith('http')) {
                    const doDownload = confirm("Detected Image URL. Download and save locally?");
                    if (doDownload) {
                        try {
                            const res = await fetch('/api/download-media', {
                                method: 'POST',
                                headers: { 'Content-Type': 'application/json' },
                                body: JSON.stringify({ url: url, lang: currentEditingLang })
                            });

                            if (res.ok) {
                                const data = await res.json();
                                const relativePath = `Algorithms/BruteForce/${currentEditingLang}/Media/${data.filename}`;

                                if (targetAuthorIdx >= 0) {
                                    // Update Author
                                    updateAuthor(targetAuthorIdx, 'image', relativePath);
                                    document.getElementById(`author-img-${targetAuthorIdx}`).src = relativePath;
                                    activeEl.value = relativePath;
                                } else {
                                    // Update Main Logo
                                    document.getElementById('modalImg').src = relativePath;
                                    document.getElementById('editInputs-image').value = relativePath;
                                    if (currentMetadata) currentMetadata.image = relativePath;
                                }
                            } else {
                                alert("Download failed");
                            }
                        } catch (e) {
                            alert("Download error: " + e.message);
                        }
                    }
                }
            });
        }
    }
});

window.checkAndDownloadAuthorImage = async function (idx, value) {
    // Check if URL
    if (value.match(/\.(jpeg|jpg|gif|png|webp)$/i) || value.startsWith('http')) {
        const doDownload = confirm("Detected Image URL. Download and save locally?");
        if (doDownload) {
            try {
                const res = await fetch('/api/download-media', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ url: value, lang: currentEditingLang })
                });

                if (res.ok) {
                    const data = await res.json();
                    const relativePath = `Algorithms/BruteForce/${currentEditingLang}/Media/${data.filename}`;

                    // Update field and data
                    updateAuthor(idx, 'image', relativePath);
                    document.getElementById(`author-input-${idx}`).value = relativePath;

                    // Update Image Preview
                    const imgEl = document.getElementById(`author-img-${idx}`);
                    if (imgEl) imgEl.src = relativePath;

                    return;
                } else {
                    alert("Download failed");
                }
            } catch (e) {
                alert("Download error: " + e.message);
            }
        }
    }
    // Fallback: just update the value
    updateAuthor(idx, 'image', value);
};

// Upload Input (Main Logo)
window.uploadLogo = async function (input) {
    if (input.files && input.files[0]) {
        const imgEl = document.getElementById('modalImg');
        imgEl.style.opacity = '0.5';
        const relativePath = await uploadBlob(input.files[0], true);  // true = isLogo
        if (relativePath) {
            imgEl.src = relativePath;
            document.getElementById('editInputs-image').value = relativePath;
            if (currentMetadata) currentMetadata.image = relativePath;
        }
        imgEl.style.opacity = '1';
    }
};

// Upload Input (Author)
window.uploadAuthorLogo = async function (input) {
    if (input.files && input.files[0] && window.currentAuthorIndex !== undefined) {
        const idx = window.currentAuthorIndex;
        const imgEl = document.getElementById(`author-img-${idx}`);
        if (imgEl) imgEl.style.opacity = '0.5';

        const relativePath = await uploadBlob(input.files[0]);

        if (relativePath) {
            updateAuthor(idx, 'image', relativePath);
            if (imgEl) imgEl.src = relativePath;
            const inputEl = document.getElementById(`author-input-${idx}`);
            if (inputEl) inputEl.value = relativePath;
        }
        if (imgEl) imgEl.style.opacity = '1';
    }
};


function closeModal(event) {
    const modal = document.getElementById('langModal');

    if (!event) {
        modal.classList.remove('visible');
        document.body.classList.remove('modal-open');
        return;
    }

    // Only close if clicking directly on the modal backdrop or close button
    // Don't close if clicking on modal content or any buttons
    if (event.target.id === 'langModal' || event.target.classList.contains('modal-close')) {
        console.log('Closing modal - clicked on backdrop or close button');
        modal.classList.remove('visible');
        document.body.classList.remove('modal-open');
    } else {
        console.log('Not closing modal - clicked on:', event.target.id || event.target.className);
    }
}

// Source Code Modal Functions
window.viewSourceCode = async function() {
    if (!currentEditingLang) return;

    const modal = document.getElementById('sourceModal');
    const title = document.getElementById('sourceModalTitle');
    const content = document.getElementById('sourceCodeContent');

    title.textContent = `${currentEditingLang} Solver Source`;
    content.textContent = 'Loading...';
    modal.classList.add('visible');

    // 1. Check embedded data first (works for offline/static)
    if (typeof sourceCodeData !== 'undefined' && sourceCodeData[currentEditingLang]) {
        const data = sourceCodeData[currentEditingLang];
        content.textContent = data.source;
        title.textContent = `${currentEditingLang} - ${data.filename}`;
        return;
    }

    try {
        const encodedLang = encodeURIComponent(currentEditingLang);
        const res = await fetch(`/api/source/${encodedLang}`);
        if (res.ok) {
            const data = await res.json();
            content.textContent = data.source;
            title.textContent = `${currentEditingLang} - ${data.filename}`;
        } else {
            const errorData = await res.json().catch(() => ({}));
            content.textContent = `Error: Could not load source code\nStatus: ${res.status}\n${errorData.error || ''}\n${errorData.path || ''}`;
        }
    } catch (e) {
        content.textContent = 'Error: ' + e.message + '\n\nMake sure the server is running on this port, or use the offline report with embedded source.';
    }
};

window.closeSourceModal = function(event) {
    if (!event || event.target.id === 'sourceModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('sourceModal').classList.remove('visible');
    }
};

window.copySourceCode = function() {
    const content = document.getElementById('sourceCodeContent').textContent;
    navigator.clipboard.writeText(content).then(() => {
        alert('Source code copied to clipboard!');
    }).catch(err => {
        console.error('Failed to copy:', err);
    });
};

function showMethodology() {
    document.getElementById('methodModal').classList.add('visible');
}

function closeMethodology(event) {
    if (event.target.id === 'methodModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('methodModal').classList.remove('visible');
    }
}

function showGoals() {
    document.getElementById('goalsModal').classList.add('visible');
}

function closeGoals(event) {
    if (event.target.id === 'goalsModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('goalsModal').classList.remove('visible');
    }
}

function showWhy() {
    document.getElementById('whyModal').classList.add('visible');
}

function closeWhy(event) {
    if (event.target.id === 'whyModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('whyModal').classList.remove('visible');
    }
}

function showDiagnostics() {
    const content = document.getElementById('diagnosticsContent');
    if (!content) return;

    let html = '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px;">';

    const categories = [
        { key: 'env_error', title: 'Environment Errors', color: '#f7768e' },
        { key: 'timeout', title: 'Timeouts', color: '#ff9e64' },
        { key: 'error', title: 'Runtime Errors', color: '#db4b4b' },
        { key: 'missing', title: 'Missing Results', color: '#565f89' }
    ];

    categories.forEach(cat => {
        const data = diagnosticsData[cat.key];
        html += `
            <div style="background: rgba(0,0,0,0.2); padding: 15px; border-radius: 8px; border-left: 4px solid ${cat.color};">
                <h3 style="color: ${cat.color}; margin-top: 0;">${cat.title} (${data.count})</h3>
                <div style="max-height: 200px; overflow-y: auto; font-size: 0.9em;">
                    ${data.languages.length > 0 
                        ? data.languages.map(l => `
                            <div style="margin-bottom: 5px;">
                                <strong>${l.language}</strong>
                                ${l.matrices.length > 0 ? `<span style="color: var(--muted);"> (M${l.matrices.join(', ')})</span>` : ''}
                            </div>
                        `).join('')
                        : '<div style="color: var(--muted);">None</div>'
                    }
                </div>
            </div>
        `;
    });

    html += '</div>';
    content.innerHTML = html;
    document.getElementById('diagnosticsModal').classList.add('visible');
}

function closeDiagnostics(event) {
    if (!event || event.target.id === 'diagnosticsModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('diagnosticsModal').classList.remove('visible');
    }
}

// Initialize
// Note: Mismatch filter starts inactive (showing all rows)
window.languageStatus = {};
// Default: select ALL languages from metricsData
window.selectedLanguages = new Set(typeof metricsData !== 'undefined' ? metricsData.map(m => m.solver) : ['C']);
window.lockedLanguages = new Map(); // Languages unaffected by Bulk actions (stored with timestamp)
window.showLogos = true; // Default to showing logos

// ESC key handler for closing modal
document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape' || event.key === 'Esc') {
        const langModal = document.getElementById('langModal');
        if (langModal && langModal.classList.contains('visible')) {
            closeModal(null);
        }
        
        const scoreModal = document.getElementById('scoreModal');
        if (scoreModal && scoreModal.style.display === 'flex') {
            closeScoreModal(null);
        }

        const methodModal = document.getElementById('methodModal');
        if (methodModal && methodModal.classList.contains('visible')) {
            closeMethodology({ target: methodModal });
        }

        const goalsModal = document.getElementById('goalsModal');
        if (goalsModal && goalsModal.classList.contains('visible')) {
            closeGoals({ target: goalsModal });
        }

        const whyModal = document.getElementById('whyModal');
        if (whyModal && whyModal.classList.contains('visible')) {
            closeWhy({ target: whyModal });
        }

        const diagnosticsModal = document.getElementById('diagnosticsModal');
        if (diagnosticsModal && diagnosticsModal.classList.contains('visible')) {
            closeDiagnostics({ target: diagnosticsModal });
        }
    }
});

// Focus trap for modal accessibility
function trapFocus(element) {
    const focusableElements = element.querySelectorAll(
        'a[href], button:not([disabled]), textarea:not([disabled]), input:not([disabled]), select:not([disabled]), [tabindex]:not([tabindex="-1"])'
    );
    const firstElement = focusableElements[0];
    const lastElement = focusableElements[focusableElements.length - 1];

    element.addEventListener('keydown', function (e) {
        if (e.key !== 'Tab') return;

        if (e.shiftKey) {
            // Shift + Tab
            if (document.activeElement === firstElement) {
                lastElement.focus();
                e.preventDefault();
            }
        } else {
            // Tab
            if (document.activeElement === lastElement) {
                firstElement.focus();
                e.preventDefault();
            }
        }
    });

    // Focus first element when modal opens
    if (firstElement) {
        firstElement.focus();
    }
}

// Handle broken logo images
document.addEventListener('DOMContentLoaded', function () {
    const logos = document.querySelectorAll('.lang-logo');
    logos.forEach(img => {
        img.addEventListener('error', function () {
            console.warn(`Failed to load logo: ${this.src}`);
            // Apply fallback styling for broken images
            this.style.background = 'linear-gradient(135deg, #414868 0%, #24283b 100%)';
            this.style.padding = '8px';
            this.alt = this.alt || '?';
            this.title = `${this.alt} (logo unavailable)`;
        });
    });
});

// Make modal draggable
function makeDraggable(modal, handle) {
    let isDragging = false;
    let currentX;
    let currentY;
    let initialX;
    let initialY;

    handle.addEventListener('mousedown', dragStart);

    function dragStart(e) {
        // Only drag on header, not on buttons or links
        if (e.target.closest('button') || e.target.closest('a') || e.target.closest('.modal-close')) {
            return;
        }

        const modalContent = modal.querySelector('.modal-content');
        const rect = modalContent.getBoundingClientRect();

        initialX = e.clientX - rect.left;
        initialY = e.clientY - rect.top;

        isDragging = true;

        document.addEventListener('mousemove', drag);
        document.addEventListener('mouseup', dragEnd);

        modalContent.style.cursor = 'grabbing';
    }

    function drag(e) {
        if (!isDragging) return;

        e.preventDefault();

        const modalContent = modal.querySelector('.modal-content');
        currentX = e.clientX - initialX;
        currentY = e.clientY - initialY;

        // Keep within viewport
        const modalWidth = modalContent.offsetWidth;
        const modalHeight = modalContent.offsetHeight;

        currentX = Math.max(0, Math.min(currentX, window.innerWidth - modalWidth));
        currentY = Math.max(0, Math.min(currentY, window.innerHeight - modalHeight));

        modalContent.style.left = `${currentX}px`;
        modalContent.style.top = `${currentY}px`;
        modalContent.style.transform = 'none';
    }

    function dragEnd() {
        isDragging = false;
        const modalContent = modal.querySelector('.modal-content');
        modalContent.style.cursor = 'move';

        document.removeEventListener('mousemove', drag);
        document.removeEventListener('mouseup', dragEnd);
    }
}

// Initialize draggable modal on page load
document.addEventListener('DOMContentLoaded', function () {
    const modal = document.getElementById('langModal');
    const modalHeader = modal ? modal.querySelector('.modal-header') : null;
    if (modal && modalHeader) {
        makeDraggable(modal, modalHeader);
    }
});

window.toggleLogoMode = function (btn) {
    window.showLogos = !window.showLogos;
    // Update Icon
    if (window.showLogos) {
        // Show Text Icon (option to switch to text)
        btn.innerHTML = `<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 7V4h16v3"/><path d="M9 20h6"/><path d="M12 4v16"/></svg>`;
        btn.title = "Switch to Text Labels";
    } else {
        // Show Image Icon (option to switch to logos)
        btn.innerHTML = `<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="18" height="18" rx="2" ry="2"/><circle cx="8.5" cy="8.5" r="1.5"/><polyline points="21 15 16 10 5 21"/></svg>`;
        btn.title = "Switch to Logos";
    }
    // Redraw
    if (typeof window.switchChart === 'function') {
        window.switchChart(currentChart || 'line');
    }
};

// --- Status & Selector Logic ---
window.toggleLanguageSelector = function () {
    const dropdown = document.getElementById('language-selector-dropdown');
    if (dropdown.style.display === 'none') {
        window.populateLanguageSelector();
        dropdown.style.display = 'block';
    } else {
        dropdown.style.display = 'none';
    }
};

window.populateLanguageSelector = function () {
    const dropdown = document.getElementById('language-selector-dropdown');
    dropdown.innerHTML = '';

    // Get all languages from table rows
    const rows = document.querySelectorAll('tbody tr');
    let allLanguages = [];
    rows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        if (lang && !allLanguages.includes(lang)) {
            allLanguages.push(lang);
        }
    });

    // Buckets
    const locked = [];
    const selected = [];
    const available = [];

    allLanguages.forEach(lang => {
        if (window.lockedLanguages.has(lang)) {
            locked.push(lang);
        } else if (window.selectedLanguages.has(lang)) {
            selected.push(lang);
        } else {
            available.push(lang);
        }
    });

    // Sort within buckets
    locked.sort();
    const selectionOrder = Array.from(window.selectedLanguages);
    selected.sort((a, b) => selectionOrder.indexOf(a) - selectionOrder.indexOf(b));
    available.sort();

    // Helper to create item
    const createItem = (lang, isSelected) => {
        const div = document.createElement('div');
        const isLocked = window.lockedLanguages.has(lang);
        const status = window.languageStatus[lang] || 'Init';

        let statusColor = '#3d5afe';
        if (status === 'Ready') statusColor = '#00e676';
        else if (status === 'Testing') statusColor = '#ffa000';

        div.style.padding = '2px 5px';
        div.style.display = 'flex';
        div.style.alignItems = 'center';
        div.style.justifyContent = 'space-between';
        div.style.borderBottom = '1px solid #333';
        div.onmouseover = function () { this.style.backgroundColor = '#333'; };
        div.onmouseout = function () { this.style.backgroundColor = 'transparent'; };

        // Optional: Show lock time? User asked to "Capture" it, not necessarily show it in the tiny dropdown.
        // We will keep it minimal for now.

        div.innerHTML = `
            <div style="display:flex; align-items:center; flex:1;">
                <button onclick="toggleLock('${lang}')" style="background:none; border:none; color: ${isLocked ? '#ffd700' : '#444'}; cursor:pointer; font-size:1em; margin-right:5px; width:20px; text-align:center;" title="${isLocked ? 'Locked' : 'Unlocked'}">
                    ${isLocked ? '🔒' : '🔓'}
                </button>
                <label style="display:flex; align-items:center; cursor:pointer; flex:1;">
                    <input type="checkbox" value="${lang}" ${isSelected ? 'checked' : ''} onchange="updateLangSelection('${lang}', this.checked)">
                    <span style="margin-left:5px; color:#fff; font-size:0.8em;">${lang}</span>
                </label>
            </div>
            <span style="font-size:0.6em; padding:1px 4px; border-radius:3px; background:${statusColor}; color:#fff; min-width:35px; text-align:center;">${status}</span>
        `;
        return div;
    };

    // Generic header helper
    const addHeader = (text) => {
        const h = document.createElement('div');
        h.innerHTML = `<div style="font-size:0.7em; color:#888; margin-top:5px; border-bottom:1px solid #444;">${text}</div>`;
        dropdown.appendChild(h);
    };

    if (locked.length > 0) {
        addHeader("LOCKED");
        locked.forEach(lang => dropdown.appendChild(createItem(lang, window.selectedLanguages.has(lang))));
    }

    if (selected.length > 0) {
        addHeader("SELECTED");
        selected.forEach(lang => dropdown.appendChild(createItem(lang, true)));
    }

    if (available.length > 0) {
        addHeader("AVAILABLE");
        available.forEach(lang => dropdown.appendChild(createItem(lang, false)));
    }

    const clearBtn = document.createElement('div');
    clearBtn.style.textAlign = 'center';
    clearBtn.style.marginTop = '10px';
    clearBtn.innerHTML = `<button class="btn" style="font-size:0.6em;" onclick="window.clearSelection()">Clear Selection</button>`;
    dropdown.appendChild(clearBtn);
};

window.updateSolverStats = function () {
    // X = count of Languages Locked
    // Y = Total metrics (Planned)
    const lockedCount = window.lockedLanguages.size;
    // metricsData is injected globally by HTMLGenerator
    const planned = typeof metricsData !== 'undefined' ? metricsData.length : 76;

    const stat = document.getElementById('solver-stat');
    if (stat) stat.innerHTML = `SOLVED <span style="color: #00ff9d">${lockedCount}</span> OF <span style="color: #00ff9d">${planned}</span>`;

    // Calculate real benchmark stats for screensaver
    const screensaverText = document.getElementById('solver-text');
    if (screensaverText && typeof metricsData !== 'undefined' && metricsData.length > 0) {
        // Calculate statistics from real benchmark data
        let totalTime = 0;
        let totalIterations = 0;
        let totalMemory = 0;
        let validCount = 0;
        let fastest = null;
        let slowest = null;

        metricsData.forEach(m => {
            // Sum up total time across all matrices
            const time = (m.time_1 || 0) + (m.time_2 || 0) + (m.time_3 || 0) + (m.time_4 || 0) + (m.time_5 || 0);
            totalTime += time;

            // Sum iterations
            const iters = (m.iterations_1 || 0) + (m.iterations_2 || 0) + (m.iterations_3 || 0) + (m.iterations_4 || 0) + (m.iterations_5 || 0);
            totalIterations += iters;

            // Sum memory (in MB)
            const mem = (m.memory_1 || 0) + (m.memory_2 || 0) + (m.memory_3 || 0) + (m.memory_4 || 0) + (m.memory_5 || 0);
            totalMemory += mem;

            // Track validation
            if (m.validated) validCount++;

            // Track fastest/slowest
            if (!fastest || time < fastest.time) fastest = { solver: m.solver, time };
            if (!slowest || time > slowest.time) slowest = { solver: m.solver, time };
        });

        const avgTime = (totalTime / metricsData.length).toFixed(3);
        const avgMem = (totalMemory / metricsData.length).toFixed(1);

        // Display language count in solver text
        screensaverText.innerText = `${metricsData.length} LANGUAGES`;
    } else if (screensaverText) {
        screensaverText.innerText = `LOADING...`;
    }
};

window.saveSessionState = async function () {
    const state = {
        locked: Array.from(window.lockedLanguages.entries()), // [[lang, time], ...]
        selected: Array.from(window.selectedLanguages),
        currentPersona: window.currentPersona || 'Standard'
    };

    try {
        await fetch('/api/session-state', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(state)
        });
    } catch (e) {
        console.error("Failed to save state", e);
    }
};

window.loadSessionState = async function () {
    try {
        const res = await fetch('/api/session-state?t=' + Date.now());
        if (res.ok) {
            const state = await res.json();
            console.log("Loaded session state:", state);
            if (state.locked) {
                window.lockedLanguages = new Map(state.locked);
            }
            if (state.selected) {
                window.selectedLanguages = new Set(state.selected);
            }
            if (state.currentPersona) {
                window.currentPersona = state.currentPersona;
                // Update UI for persona if needed (but might be too early if DOM not ready? No, this runs after load usually)
                // Actually, we should probably call changePersonality if it's different.
                const selector = document.getElementById('personality-selector');
                if (selector) {
                    selector.value = window.currentPersona;
                    window.changePersonality(window.currentPersona);
                }
            }
            // Refresh UI
            window.populateLanguageSelector(); // If open? Or just to be ready.
            window.updateSolverStats();

            // Re-apply visibility based on selection
            const rows = document.querySelectorAll('tbody tr');
            rows.forEach(row => {
                const lang = row.getAttribute('data-lang');
                const selected = window.selectedLanguages.has(lang);
                row.style.display = selected ? '' : 'none';
            });
            window.updateSolverStats(); // Update again to be sure
        }
    } catch (e) {
        console.error("Failed to load state", e);
    }
};

window.toggleLock = function (lang) {
    if (window.lockedLanguages.has(lang)) {
        window.lockedLanguages.delete(lang);
    } else {
        window.lockedLanguages.set(lang, Date.now());
    }
    window.populateLanguageSelector();
    window.updateSolverStats();
    window.saveSessionState();
    window.updateRunButtonsForLockState();
};

// Update run button visual state based on locked languages
window.updateRunButtonsForLockState = function () {
    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        const isLocked = window.lockedLanguages && window.lockedLanguages.has(lang);
        const runButtons = row.querySelectorAll('.run-btn');

        runButtons.forEach(btn => {
            if (isLocked) {
                btn.classList.add('locked');
                btn.style.opacity = '0.4';
                btn.title = `${lang} is locked - click lock icon to unlock`;
            } else {
                btn.classList.remove('locked');
                btn.style.opacity = '';
                // Restore original title based on button content
                if (btn.textContent.includes('⏩')) {
                    btn.title = 'Run All Matrices';
                } else {
                    btn.title = btn.title.replace(/^.* is locked.*$/, '') || 'Run Matrix';
                }
            }
        });
    });
};

window.selectAllLangs = function (select) {
    const checkboxes = document.querySelectorAll('#language-selector-dropdown input[type="checkbox"]');
    checkboxes.forEach(cb => {
        if (!window.lockedLanguages.has(cb.value)) {
            cb.checked = select;
            // logic inline because calling updateLangSelection triggers save each time? 
            // Better to batch it? 
            // updateLangSelection calls row update.
            // Let's just update memory first then save once.
            if (select) window.selectedLanguages.add(cb.value);
            else window.selectedLanguages.delete(cb.value);

            // Update Visibility
            const rows = document.querySelectorAll('tbody tr');
            rows.forEach(row => {
                if (row.getAttribute('data-lang') === cb.value) {
                    row.style.display = select ? '' : 'none';
                }
            });
        }
    });
    window.saveSessionState();
};

window.updateLangSelection = function (lang, selected) {
    if (selected) window.selectedLanguages.add(lang);
    else window.selectedLanguages.delete(lang);

    // Update Visibility
    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        if (row.getAttribute('data-lang') === lang) {
            row.style.display = selected ? '' : 'none';
        }
    });
    window.saveSessionState();
};

window.applyTableVisibility = function () {
    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        const selected = window.selectedLanguages.has(lang);
        row.style.display = selected ? '' : 'none';
    });
};

window.initializeStatus = function () {
    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        const iters = parseInt(row.getAttribute('data-iters') || "0");
        const statusBadge = document.getElementById('status-' + lang);

        // Infer Status
        // If we have valid iterations, assume Ready. Otherwise Init.
        let status = 'Init';
        if (iters > 0) status = 'Ready';

        window.languageStatus[lang] = status;

        if (statusBadge) {
            updateStatusBadgeUI(lang, status);
        }
    });
    // Load persisted state
    window.loadSessionState();
    window.applyTableVisibility(); // Apply initial visibility
    window.updateRunButtonsForLockState(); // Apply lock state to run buttons
    window.updateSolverStats();
};

window.updateStatusBadgeUI = function (lang, status) {
    const badge = document.getElementById('status-' + lang);
    if (!badge) return;

    // Update Badge Color/Text
    badge.className = 'status-badge';
    badge.innerText = status;

    // ... Badge styling ...

    // Refresh stats (status changed)
    window.updateSolverStats();
    // Refresh dropdown to show new status?
    // Doing full re-populate might be heavy if done frequently, 
    // but useful for visual consistency in the dropdown.
    // window.populateLanguageSelector(); 


    if (status === 'Init') {
        badge.classList.add('status-init');
        badge.title = "Environment initializing / Unverified";
        badge.style.cursor = "pointer";
        badge.onclick = () => verifyLanguage(lang);
    } else if (status === 'Testing') {
        badge.classList.add('status-testing');
        badge.title = "Verifying environment...";
        badge.style.cursor = "wait";
        badge.onclick = null;
    } else if (status === 'Ready') {
        badge.classList.add('status-ready');
        badge.title = "Ready for Benchmark";
        badge.style.cursor = "default";
        badge.onclick = null;
    }

    // Toggle Run Buttons
    const row = document.querySelector(`tr[data-lang="${lang}"]`);
    if (row) {
        const runBtns = row.querySelectorAll('.run-btn');
        runBtns.forEach(btn => {
            if (status === 'Ready') {
                btn.style.display = 'inline-block';
            } else {
                btn.style.display = 'none';
            }
        });

        // Inject Verify Button if Init and not present
        let verifyBtn = row.querySelector('.verify-btn');
        if (status === 'Init') {
            if (!verifyBtn) {
                // Create verify btn in the Total Time column logic? Or next to Badge?
                // Badge is clickable, but let's make it obvious.
                // Actually, let's just use the Badge click for now to save space, or put a button in the first cell actions?
                // Badge click is implemented above.
            }
        }
    }
};

window.verifyLanguage = async function (lang) {
    console.log("Verifying " + lang);
    updateStatusBadgeUI(lang, 'Testing');

    // Run Matrix 1 as a test
    try {
        const res = await fetch('/api/run', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ language: lang, matrix: '1.matrix', useDocker: useDockerMode })
        });

        if (!res.ok) throw new Error("Server error");

        const data = await res.json();

        if (data.success) {
            // Check if passed?
            // The JSON output usually contains "status": "pass"
            // We assume it passed if we got data back for now, or check stdout
            updateStatusBadgeUI(lang, 'Ready');
            window.languageStatus[lang] = 'Ready';
            // Ideally refresh row data?
        } else {
            alert("Verification Failed:\n" + (data.stderr || data.error));
            updateStatusBadgeUI(lang, 'Init');
            window.languageStatus[lang] = 'Init';
        }
    } catch (e) {
        console.error(e);
        alert("Verification Error: " + e.message);
        updateStatusBadgeUI(lang, 'Init');
        window.languageStatus[lang] = 'Init';
    }
};

// Start
// populateLanguageSelector(); // Don't auto-populate on load, wait for click
initializeStatus();

// --- D3.js Chart Implementation ---
(function () {
    // Inject metrics with logo data
    // Data is now injected globally by HTMLGenerator before this script runs.
    // const historyData = ...
    // const referenceOutputs = ...
    // const tailoring = ...
    // const metricsData = ...

    let data = metricsData;
    const allTimes = data.flatMap(d => d.results.map(r => r ? r.time : 999999)).filter(t => t > 0 && t < 999999);
    // Ensure minTime has a floor value for log scale (log(0) is undefined)
    const minTime = allTimes.length ? Math.max(0.001, Math.min(...allTimes)) : 0.001;
    const maxTime = allTimes.length ? Math.max(...allTimes, 1) : 100;

    let currentChart = 'line';
    let color; // Exposed for charts

    // Expose switchChart globally
    // Expose switchChart globally - DEFINED EARLY TO PREVENT REFERENCE ERRORS
    // Expose switchChart globally
    // Expose switchChart globally
    let raceTicker = null;
    let currentZoomBehavior = null;

    // Log Modal Functions
    window.closeLogModal = function () {
        document.getElementById('logModal').classList.remove('visible');
    };

    window.openSidePanel = function (type) {
        const panel = document.getElementById('slidePanel');
        const frame = document.getElementById('slideFrame');
        const title = document.getElementById('slideTitle');
        const extLink = document.getElementById('slideExternalLink');

        let url = "";
        let finalTitle = "";

        const lang = currentEditingLang || "Unknown";
        const meta = currentMetadata || {};

        // Helper specifically for C++
        let lookupLang = lang;
        if (lang === "C++") lookupLang = "C++"; // No change needed usually, but URL encoding matters

        if (type === 'website') {
            url = meta.website;
            finalTitle = "Official Website";
            if (!url) {
                // Try to guess or show placeholder
                url = "https://www.google.com/search?q=" + encodeURIComponent(lang + " programming language official site") + "&btnI=1"; // I'm Feeling Lucky-ish
                // Or just fallback
                finalTitle = "Website (Searching...)";
            }
        } else if (type === 'grokipedia') {
            url = `https://grokipedia.com/languages/${encodeURIComponent(lang)}`;
            finalTitle = "Grokipedia";
        } else if (type === 'wikipedia') {
            // Handle special cases
            let wikiLang = lang;
            if (lang === "C_Sharp") wikiLang = "C_Sharp_(programming_language)";
            else if (lang === "F_Sharp") wikiLang = "F_Sharp_(programming_language)";
            else wikiLang = lang + "_(programming_language)";

            url = `https://en.wikipedia.org/wiki/${wikiLang}`;
            finalTitle = "Wikipedia";
        }

        title.innerText = finalTitle;
        frame.src = url;
        extLink.href = url;
        panel.classList.add('active');
    };

    window.closeSidePanel = function () {
        const panel = document.getElementById('slidePanel');
        const frame = document.getElementById('slideFrame');
        panel.classList.remove('active');
        setTimeout(() => { frame.src = ""; }, 400); // Clear after slide out to stop media
    };

    window.runAllSolver = function (lang, event) {
        if (event) event.stopPropagation();
        // Pass empty string or null for matrix to trigger Run All
        window.runSolver(lang, '', event);
    };

    window.runSolver = async function (lang, matrix, event) {
        if (event) event.stopPropagation(); // Prevent row click or tooltip

        const outputDiv = document.getElementById('logOutput');
        const headerTitle = document.getElementById('logTitle');
        const matrixLabel = matrix || "ALL Matrices";

        // Check if language is locked - skip with notification
        if (window.lockedLanguages && window.lockedLanguages.has(lang)) {
            console.log(`Skipping ${lang} - locked`);
            headerTitle.innerText = "Execution Skipped: " + lang;
            outputDiv.innerHTML = `<div style="color:#ffd700; font-size:1.2em; text-align:center; padding:40px;">
                <div style="font-size:2em; margin-bottom:10px;">🔒</div>
                <strong>${lang} is locked</strong>
                <p style="color:#888; margin-top:15px; font-size:0.9em;">Unlock this language to run benchmarks.<br>Click the lock icon in the dropdown or modal to unlock.</p>
            </div>`;
            document.getElementById('logModal').classList.add('visible');
            return { skipped: true, reason: 'locked' };
        }

        // Reset Modal
        outputDiv.innerText = "Running " + lang + " on " + matrixLabel + "...\nPlease wait...";
        headerTitle.innerText = "Execution Log: " + lang + " (" + matrixLabel + ")";
        document.getElementById('logModal').classList.add('visible');

        try {
            // Use relative path - assumes server is running on the same origin
            const res = await fetch('/api/run', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ language: lang, matrix: matrix, useDocker: useDockerMode })
            });

            if (!res.ok) {
                const errorText = await res.text();
                outputDiv.innerText = "Error: " + res.status + "\n" + errorText;
                return;
            }

            const data = await res.json();
            if (data.success) {
                outputDiv.innerText = "--- STDOUT ---\n" + data.stdout + "\n\n--- STDERR ---\n" + data.stderr;

                // Update the timestamp in the UI immediately (US-008)
                if (data.timestamp) {
                    const langRow = document.querySelector(`tr[data-lang="${lang}"]`);
                    if (langRow) {
                        // Update the data-timestamp attribute
                        langRow.setAttribute('data-timestamp', data.timestamp);

                        // Find and update the Updated column
                        const updatedCell = langRow.querySelector('.updated-col');
                        if (updatedCell) {
                            updatedCell.textContent = 'Just now';
                            updatedCell.title = new Date(data.timestamp).toLocaleString();
                        }
                    }
                }

                // Trigger Report Generation
                outputDiv.innerText += "\n\nGenerating updated report...";
                try {
                    const reportRes = await fetch('/api/generate-report', { method: 'POST' });
                    if (reportRes.ok) {
                        outputDiv.innerHTML += "\n\n<strong style='color:#00ff9d'>✓ Report generated successfully!</strong>\n\n";
                        // Add reload button instead of auto-reload
                        const reloadBtn = document.createElement('button');
                        reloadBtn.innerText = '🔄 Reload Page to See Results';
                        reloadBtn.style.cssText = 'background:#00ff9d; color:#000; border:none; padding:10px 20px; border-radius:5px; cursor:pointer; font-weight:bold; margin-top:10px;';
                        reloadBtn.onclick = () => window.location.reload();
                        outputDiv.appendChild(reloadBtn);
                    } else {
                        outputDiv.innerText += "\nFailed to generate report.";
                    }
                } catch (err) {
                    outputDiv.innerText += "\nError generating report: " + err.message;
                }

            } else {
                outputDiv.innerText = "Error:\n" + data.error + "\n\nStdout:\n" + data.stdout + "\n\nStderr:\n" + data.stderr;
            }

        } catch (e) {
            outputDiv.innerText = "Network/Client Error:\n" + e.message + "\n\nMake sure the metric server is running!";
        }
    };

    window.undoZoom = function () {
        if (currentZoomBehavior && typeof d3 !== 'undefined') {
            const svg = d3.select("#d3-chart-container svg");
            if (!svg.empty()) {
                svg.transition().duration(750).call(currentZoomBehavior.transform, d3.zoomIdentity);
            }
        }
    };

    window.switchChart = function (type) {
        try {
            currentChart = type;
            window.currentChart = type; // Expose globally for fullscreen handler

            // Stop race if running
            if (raceTicker) {
                raceTicker.stop();
                raceTicker = null;
            }

            // Update Dropdown UI
            const selector = document.getElementById('chart-selector');
            if (selector) selector.value = type;

            // Check if D3 is loaded
            if (typeof d3 === 'undefined') {
                throw new Error("D3.js library not loaded. Please check your internet connection.");
            }

            const container = d3.select("#d3-chart-container");

            // Fade out existing chart
            container.transition()
                .duration(200)
                .style("opacity", 0)
                .on("end", function() {
                    container.selectAll("*").remove();
                    container.style("opacity", 1);

                    // Draw new chart based on type
                    if (type === 'line') {
                        drawLineChart();
                    } else if (type === 'jockey') {
                        drawJockeyChart();
                    } else if (type === 'race') {
                        // Auto-enter fullscreen for Matrix Race to show all languages
                        if (!document.fullscreenElement && typeof window.toggleChartFullscreen === 'function') {
                            window.toggleChartFullscreen();
                            // Delay draw to allow fullscreen transition to complete
                            setTimeout(() => drawMatrixRace(), 300);
                        } else {
                            drawMatrixRace();
                        }
                    } else if (type === 'algorithm') {
                        drawAlgorithmComparisonChart();
                    } else if (type === 'language') {
                        drawLanguagePerformanceChart();
                    } else if (type === 'iterations') {
                        drawIterationCountChart();
                    }
                });
        } catch (e) {
            console.error("Error switching chart:", e);
            const container = document.getElementById('d3-chart-container');
            if (container) {
                container.innerHTML = "<div style='color:#ff4444; padding:20px; text-align:center; font-family:monospace;'>" +
                    "<h3>Chart Error</h3>" +
                    "<p>" + e.message + "</p>" +
                    "</div>";
            }
        }
    };

    function drawMatrixRace() {
        const container = document.getElementById('d3-chart-container');
        const width = container.clientWidth;
        const height = container.clientHeight;
        const margin = { top: 40, right: 40, bottom: 20, left: 150 };

        const svg = d3.select("#d3-chart-container")
            .append("svg")
            .attr("width", width)
            .attr("height", height);

        const raceDuration = 15000; // 15s
        // Show all languages that have results
        const topN = data.filter(d => d.results && d.results.length >= 5).length;

        // X scale: 0 to 5 (Matrices)
        const x = d3.scaleLinear()
            .domain([0, 5])
            .range([margin.left, width - margin.right]);

        // Y scale: Rank
        const y = d3.scaleBand()
            .domain(d3.range(topN))
            .range([margin.top, height - margin.bottom])
            .padding(0.1);

        // Solver processing
        const solvers = data
            .filter(d => d.results && d.results.length >= 5)
            .map(d => {
                let cum = 0;
                const checkpoints = d.results.map(r => {
                    cum += r.time;
                    return { time: r.time, cum: cum, matrix: r.matrix };
                });
                return {
                    solver: d.solver,
                    checkpoints: checkpoints,
                    totalTime: cum,
                    progress: 0,
                    logo: d.logo || null
                };
            });

        // Race Clock
        const logMin = Math.log(minTime / 10 || 0.0001);
        const logMax = Math.log(maxTime * 1.5 || 100);

        // Initial Draw
        let bars = svg.append("g").selectAll("g");

        // Axis
        svg.append("g")
            .attr("transform", "translate(0," + height + ")")
            .call(d3.axisBottom(x).ticks(5).tickFormat(d => "Matrix " + d))
            .attr("color", "#5c5c66")
            .selectAll("text")
            .attr("font-family", "JetBrains Mono")
            .attr("font-size", "12px")
            .attr("color", "#e0e0e0");

        // Current Time Label
        const timeLabel = svg.append("text")
            .attr("x", width - margin.right)
            .attr("y", height - 20)
            .attr("text-anchor", "end")
            .attr("fill", "#00ff9d")
            .attr("font-size", "24px")
            .attr("font-family", "JetBrains Mono")
            .text("0.00s");

        // Slider Controls
        const controls = d3.select("#d3-chart-container")
            .append("div")
            .attr("id", "race-controls")
            .style("position", "absolute")
            .style("bottom", "50px")
            .style("left", "50%")
            .style("transform", "translateX(-50%)")
            .style("width", "60%")
            .style("display", "flex")
            .style("align-items", "center")
            .style("gap", "15px")
            .style("background", "rgba(13, 13, 18, 0.8)")
            .style("padding", "10px 20px")
            .style("border", "1px solid #00ff9d")
            .style("border-radius", "20px")
            .style("z-index", "100");

        controls.append("span")
            .style("color", "#00ff9d")
            .style("font-family", "JetBrains Mono")
            .style("font-size", "0.9em")
            .text("TIME TRAVEL");

        const slider = controls.append("input")
            .attr("type", "range")
            .attr("min", 0)
            .attr("max", raceDuration)
            .attr("value", 0)
            .style("flex-grow", "1")
            .style("cursor", "pointer")
            .style("accent-color", "#00ff9d");

        let isPaused = false;
        let currentElapsed = 0;
        let lastFrameTime = 0;

        slider.on("input", function (event) {
            isPaused = true;
            currentElapsed = +this.value;
            updateFrame(currentElapsed);
        });

        // Update Function
        function updateFrame(elapsed) {
            // Clamp
            if (elapsed > raceDuration) elapsed = raceDuration;
            if (elapsed < 0) elapsed = 0;

            // Calculate Sim Time
            const normT = elapsed / raceDuration;
            const logCurrent = logMin + normT * (logMax - logMin);
            const simTime = Math.exp(logCurrent);

            // Update Solvers
            solvers.forEach(s => {
                let prog = 0;
                // Find where simTime lands
                // Checkpoints: [ {time: t1, cum: c1}, {time: t2, cum: c2}, ... ]
                /* 
                   If simTime < c1: prog = 0 + (simTime - 0) / t1
                   If c1 < simTime < c2: prog = 1 + (simTime - c1) / t2
                */
                let prevCum = 0;
                for (let i = 0; i < s.checkpoints.length; i++) {
                    const cp = s.checkpoints[i];
                    if (simTime < cp.cum) {
                        const fraction = (simTime - prevCum) / cp.time;
                        prog = i + fraction;
                        break;
                    } else {
                        prog = i + 1; // Completed this matrix
                        prevCum = cp.cum;
                    }
                }
                // Clamp to 5.0
                if (prog > 5) prog = 5;
                s.progress = prog;
            });

            // Rank and Slice - fastest first (descending by progress)
            const ranked = solvers.sort((a, b) => b.progress - a.progress).slice(0, topN);

            // Bind Data (Key by solver name)
            const group = svg.selectAll(".bar-group")
                .data(ranked, d => d.solver);

            // Enter
            const groupEnter = group.enter().append("g")
                .attr("class", "bar-group")
                .attr("transform", (d, i) => "translate(0," + y(i) + ")");

            groupEnter.append("rect")
                .attr("x", x(0))
                .attr("height", y.bandwidth())
                .attr("fill", d => color(d.solver))
                .attr("opacity", 0.8)
                .attr("width", 0);

            // Label
            groupEnter.append("text")
                .attr("class", "label")
                .attr("y", y.bandwidth() / 2)
                .attr("dy", "0.35em")
                .attr("text-anchor", "end")
                .attr("fill", "#e0e0e0")
                .attr("font-family", "JetBrains Mono")
                .attr("font-size", "12px")
                .text(d => d.solver);

            // Icon
            groupEnter.append("image")
                .attr("class", "icon")
                .attr("href", d => d.logo)
                .attr("height", y.bandwidth())
                .attr("width", y.bandwidth())
                .attr("x", margin.left - 140)
                .attr("y", 0);

            // Update
            const groupUpdate = group.merge(groupEnter);

            // Immediate update for slider dragging (no transition)
            // If paused (scrubbing), use 0 duration. If playing, use small duration.
            const dura = isPaused ? 0 : 50;
            const ease = d3.easeLinear;

            groupUpdate.transition().duration(dura).ease(ease)
                .attr("transform", (d, i) => "translate(0," + y(i) + ")");

            groupUpdate.select("rect")
                .transition().duration(dura).ease(ease)
                .attr("width", d => x(d.progress) - x(0));

            groupUpdate.select("text")
                .attr("x", margin.left - 10);

            // Exit
            group.exit().remove();

            // Update Clock Label
            timeLabel.text(simTime.toFixed(4) + "s");

            // Sync Slider if playing
            if (!isPaused) {
                slider.property("value", elapsed);
            }
        }

        // Animation Loop
        raceTicker = d3.timer((now) => {
            if (!lastFrameTime) lastFrameTime = now;
            const dt = now - lastFrameTime;
            lastFrameTime = now;

            if (!isPaused) {
                currentElapsed += dt;
                if (currentElapsed > raceDuration) {
                    currentElapsed = raceDuration; // Don't stop, just stay at end? Or loop?
                    // Previously it stopped. Let's start pause state at end.
                    isPaused = true;
                }
                updateFrame(currentElapsed);
            }
        });
    }

    try {
        // Check for D3 availability immediately
        if (typeof d3 === 'undefined') {
            throw new Error("D3.js library failed to load.");
        }

        // Filter out mismatched solvers
        const cSolver = data.find(s => s.solver === 'C');
        if (cSolver) {
            const cIters = {};
            cSolver.results.forEach(r => cIters[normalizeMatrix(r.matrix)] = r.iterations);

            data = data.filter(s => {
                if (s.solver === 'C') return true;
                const hasMismatch = s.results.some(r => {
                    const expected = cIters[normalizeMatrix(r.matrix)];
                    return expected && r.iterations !== expected;
                });
                return !hasMismatch;
            });
        }

        // Use global minTime/maxTime
        // Support both old format ("1.matrix") and new format ("1")
        const matrixNumbers = ["1", "2", "3", "4", "5", "6"];
        const matrices = ["1.matrix", "2.matrix", "3.matrix", "4.matrix", "5.matrix", "6.matrix"];


        // Tier Color Palette
        const tierColors = {
            'S': '#ffd700', // Gold
            'A': '#00ff9d', // Green
            'B': '#00b8ff', // Blue
            'C': '#e0e0e0', // White/Grey
            'D': '#ffaa00', // Orange
            'F': '#ff0055'  // Red
        };

        // Color function using enriched metricsData
        color = function (solverName) {
            const solverData = data.find(d => d.solver === solverName);
            if (solverData && solverData.tier) {
                return tierColors[solverData.tier] || '#e0e0e0';
            }
            // Fallback
            return '#00b8ff';
        };



        function drawLineChart() {
            const container = document.getElementById('d3-chart-container');
            const width = container.clientWidth;
            const height = container.clientHeight;
            const margin = { top: 20, right: 120, bottom: 50, left: 60 };

            const svg = d3.select("#d3-chart-container")
                .append("svg")
                .attr("width", width)
                .attr("height", height)
                .append("g")
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

            const chartWidth = width - margin.left - margin.right;
            const chartHeight = height - margin.top - margin.bottom;

            // Zoom
            // Zoom
            const zoom = d3.zoom();
            currentZoomBehavior = zoom; // Store for global access

            zoom
                .scaleExtent([0.5, 5])
                .filter(function (event) {
                    // Only allow zoom if mouse is within the chart area
                    if (event.type === 'wheel' || event.type === 'mousedown') {
                        const [mx, my] = d3.pointer(event, this);
                        return mx >= 0 && mx <= chartWidth && my >= 0 && my <= chartHeight;
                    }
                    return true;
                })
                .on("zoom", (event) => {
                    // Apply transform with margin offset
                    svg.attr("transform", "translate(" + (margin.left + event.transform.x) + "," + (margin.top + event.transform.y) + ") scale(" + event.transform.k + ")");
                });

            d3.select("#d3-chart-container svg").call(zoom);

            // X Axis (use normalized matrix numbers for domain)
            const x = d3.scalePoint()
                .domain(matrixNumbers)
                .range([0, chartWidth])
                .padding(0.5);

            svg.append("g")
                .attr("transform", "translate(0," + chartHeight + ")")
                .call(d3.axisBottom(x))
                .selectAll("text")
                .style("fill", "#e0e0e0")
                .style("font-family", "JetBrains Mono");

            svg.append("text")
                .attr("text-anchor", "end")
                .attr("x", chartWidth)
                .attr("y", chartHeight + 40)
                .style("fill", "#5c5c66")
                .style("font-size", "12px")
                .text("Matrix Input");

            // Y Axis
            const y = d3.scaleLog()
                .domain([minTime, maxTime])
                .range([chartHeight, 0]);

            svg.append("g")
                .call(d3.axisLeft(y)
                    .ticks(5)
                    .tickFormat((d) => {
                        const ticks = y.ticks(5);
                        if (ticks.indexOf(d) % 2 === 0 || ticks.length <= 3) {
                            return d >= 1 ? d3.format(".1f")(d) + "s" : d3.format(".2f")(d) + "s";
                        }
                        return "";
                    })
                )
                .selectAll("text")
                .style("fill", "#e0e0e0")
                .style("font-family", "JetBrains Mono");

            svg.append("text")
                .attr("transform", "rotate(-90)")
                .attr("y", 0 - margin.left)
                .attr("x", 0 - (chartHeight / 2))
                .attr("dy", "1em")
                .style("text-anchor", "middle")
                .style("fill", "#5c5c66")
                .style("font-size", "12px")
                .text("Time (seconds) - Log Scale");

            // Grid lines
            svg.append("g")
                .attr("class", "grid")
                .attr("opacity", 0.1)
                .call(d3.axisLeft(y).tickSize(-chartWidth).tickFormat(""));

            // Line Generator
            const line = d3.line()
                .x(d => x(normalizeMatrix(d.matrix)))
                .y(d => y(Math.max(d.time, minTime)));

            // Draw Lines
            data.forEach(solver => {
                const solverData = solver.results.filter(r => matrixNumbers.includes(normalizeMatrix(r.matrix)));
                const safeSolverClass = "dot-" + solver.solver.replace(/[^a-zA-Z0-9]/g, '_');

                svg.append("path")
                    .datum(solverData)
                    .attr("fill", "none")
                    .attr("stroke", color(solver.solver))
                    .attr("stroke-width", 2)
                    .attr("d", line)
                    .attr("class", "line-path");

                const pointGroup = svg.selectAll("." + safeSolverClass)
                    .data(solverData)
                    .enter().append("g")
                    .attr("class", safeSolverClass)
                    .attr("transform", d => "translate(" + x(normalizeMatrix(d.matrix)) + ", " + y(Math.max(d.time, minTime)) + ")");

                // Logo Image
                pointGroup.append("image")
                    .attr("class", "chart-node-image") // Class for toggling
                    .attr("xlink:href", solver.logo)
                    .attr("x", -8)
                    .attr("y", -8)
                    .attr("width", 16)
                    .attr("height", 16)
                    .attr("preserveAspectRatio", "xMidYMid meet")
                    .attr("filter", d => {
                        const baseLang = solver.solver.replace(/ \((AI)\)$/, '');
                        const config = tailoring[baseLang] || tailoring[solver.solver];
                        if (config?.invert) return "url(#filter-invert)";
                        if (config?.transparent_white) return "url(#filter-transparent-white)";
                        return null;
                    });

                // Text Label (Hidden by default via CSS)
                pointGroup.append("text")
                    .attr("class", "chart-node-label")
                    .text(solver.solver)
                    .attr("text-anchor", "middle")
                    .attr("y", 20) // Position below image
                    .style("pointer-events", "none");

                // Docker Icon (Whale)
                if (solver.runType === 'Docker') {
                    pointGroup.append("text")
                        .text("🐳")
                        .attr("x", 6)
                        .attr("y", 6)
                        .attr("font-size", "10px")
                        .style("pointer-events", "none");
                }

                // Interactions
                pointGroup.on("mouseover", function (event, d) {
                    d3.select(this).raise();
                    d3.select(this).select("image")
                        .attr("width", 24)
                        .attr("height", 24)
                        .attr("x", -12)
                        .attr("y", -12);

                    const tooltip = document.getElementById('tooltip');
                    tooltip.style.display = 'block';
                    tooltip.style.left = (event.clientX + 15) + 'px';
                    tooltip.style.top = (event.clientY + 15) + 'px';
                    tooltip.innerHTML = "<strong style='color:" + color(solver.solver) + "'>" + solver.solver + "</strong>" +
                        (solver.runType === 'Docker' ? " 🐳" : "") +
                        "<br>Matrix: " + d.matrix + "<br>Time: " + d.time.toFixed(6) + "s<br>Iters: " + d.iterations;
                })
                    .on("mouseout", function () {
                        d3.select(this).select("image")
                            .attr("width", 16)
                            .attr("height", 16)
                            .attr("x", -8)
                            .attr("y", -8);
                        document.getElementById('tooltip').style.display = 'none';
                    })
                    .on("click", function (event, d) {
                        event.stopPropagation();
                        // Open Language Modal
                        if (typeof window.showLanguageDetails === 'function') {
                            window.showLanguageDetails(solver.solver, event.clientX, event.clientY);
                        }
                    });

                // Label
                const lastPoint = solverData[solverData.length - 1];
                if (lastPoint) {
                    svg.append("text")
                        .attr("x", x(normalizeMatrix(lastPoint.matrix)) + 10)
                        .attr("y", y(Math.max(lastPoint.time, minTime)))
                        .attr("dy", "0.35em")
                        .style("fill", color(solver.solver))
                        .style("font-size", "12px")
                        .style("font-weight", "bold")
                        .text(solver.displayName);
                }
            });
        }

        function drawJockeyChart() {
            const container = document.getElementById('d3-chart-container');
            const width = container.clientWidth;
            const height = container.clientHeight;
            const margin = { top: 20, right: 60, bottom: 20, left: 150 };

            const svg = d3.select("#d3-chart-container")
                .append("svg")
                .attr("width", width)
                .attr("height", height)
                .append("g")
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

            const chartWidth = width - margin.left - margin.right;
            const chartHeight = height - margin.top - margin.bottom;

            // Zoom
            // Zoom
            const zoom = d3.zoom();
            currentZoomBehavior = zoom; // Store for global access

            zoom
                .scaleExtent([0.5, 5])
                .filter(function (event) {
                    // Only allow zoom if mouse is within the chart area
                    if (event.type === 'wheel' || event.type === 'mousedown') {
                        const [mx, my] = d3.pointer(event, this);
                        return mx >= 0 && mx <= chartWidth && my >= 0 && my <= chartHeight;
                    }
                    return true;
                })
                .on("zoom", (event) => {
                    svg.attr("transform", "translate(" + (margin.left + event.transform.x) + "," + (margin.top + event.transform.y) + ") scale(" + event.transform.k + ")");
                });

            d3.select("#d3-chart-container svg").call(zoom);

            // Calculate Total Time for sorting
            const sortedData = [...data].map(s => {
                const totalTime = s.results.reduce((acc, r) => acc + r.time, 0);
                return { ...s, totalTime: totalTime > 0 ? totalTime : 0.000001 }; // Prevent 0 for log scale
            }).sort((a, b) => a.totalTime - b.totalTime); // Fastest first

            const minTotal = d3.min(sortedData, d => d.totalTime);
            const maxTotal = d3.max(sortedData, d => d.totalTime);

            // Y Axis (Solvers)
            const y = d3.scaleBand()
                .domain(sortedData.map(d => d.solver))
                .range([0, chartHeight])
                .padding(0.2);

            // X Axis (Total Time - Log Scale)
            const x = d3.scaleLog()
                .domain([minTotal, maxTotal])
                .range([0, chartWidth]);

            // Draw Tracks (Background Lines)
            svg.selectAll(".track")
                .data(sortedData)
                .enter().append("line")
                .attr("x1", 0)
                .attr("x2", chartWidth)
                .attr("y1", d => y(d.solver) + y.bandwidth() / 2)
                .attr("y2", d => y(d.solver) + y.bandwidth() / 2)
                .attr("stroke", "#2a2a35")
                .attr("stroke-width", 1)
                .attr("stroke-dasharray", "4");

            // Draw Bars (Progress)
            svg.selectAll(".bar")
                .data(sortedData)
                .enter().append("rect")
                .attr("y", d => y(d.solver) + y.bandwidth() / 2 - 2)
                .attr("height", 4)
                .attr("x", 0)
                .attr("width", d => x(d.totalTime))
                .attr("fill", d => color(d.solver))
                .attr("opacity", 0.6);

            // Draw Logos (Jockeys)
            if (window.showLogos) {
                svg.selectAll(".jockey")
                    .data(sortedData)
                    .enter().append("image")
                    .attr("xlink:href", d => d.logo)
                    .attr("x", d => x(d.totalTime) - 20) // Center image on end of bar
                    .attr("y", d => y(d.solver) + y.bandwidth() / 2 - 10)
                    .attr("width", 20)
                    .attr("height", 20)
                    .attr("preserveAspectRatio", "xMidYMid meet")
                    .attr("filter", d => {
                        const baseLang = d.solver.replace(/ \((Manual|AI)\)$/, '');
                        const config = tailoring[baseLang] || tailoring[d.solver];
                        if (config?.invert) return "url(#filter-invert)";
                        if (config?.transparent_white) return "url(#filter-transparent-white)";
                        return null;
                    })
                    .on("mouseover", function (event, d) {
                        d3.select(this).attr("width", 32).attr("height", 32).attr("x", x(d.totalTime) - 16).attr("y", y(d.solver) + y.bandwidth() / 2 - 16);
                        const tooltip = document.getElementById('tooltip');
                        tooltip.style.display = 'block';
                        tooltip.style.left = (event.clientX + 15) + 'px';
                        tooltip.style.top = (event.clientY + 15) + 'px';
                        const timeStr = d.totalTime < 0.0001 && d.totalTime > 0 ? d.totalTime.toExponential(4) + "s" : d.totalTime.toFixed(4) + "s";
                        tooltip.innerHTML = "<strong style='color:" + color(d.solver) + "'>" + d.solver + "</strong><br>Total Time: " + timeStr;
                    })
                    .on("mouseout", function (event, d) {
                        d3.select(this).attr("width", 24).attr("height", 24).attr("x", x(d.totalTime) - 12).attr("y", y(d.solver) + y.bandwidth() / 2 - 12);
                        document.getElementById('tooltip').style.display = 'none';
                    });
            }

            // Y Axis Labels
            // Draw axis, but potentially hide text
            const yAxis = svg.append("g")
                .call(d3.axisLeft(y));

            yAxis.selectAll("text")
                .style("fill", "#e0e0e0")
                .style("font-family", "JetBrains Mono")
                .style("font-size", "10px")
                .style("display", window.showLogos ? "none" : ""); // Toggle display

            // X Axis
            svg.append("g")
                .attr("transform", "translate(0," + chartHeight + ")")
                .call(d3.axisBottom(x).ticks(5, ".1f"))
                .selectAll("text")
                .style("fill", "#5c5c66");
        } // End of drawJockeyChart

        function drawAlgorithmComparisonChart() {
            const container = d3.select("#d3-chart-container");
            const width = document.getElementById('d3-chart-container').clientWidth;
            const height = document.getElementById('d3-chart-container').clientHeight;
            const margin = { top: 80, right: 60, bottom: 100, left: 80 };
            const chartWidth = width - margin.left - margin.right;
            const chartHeight = height - margin.top - margin.bottom;

            // Filter data by algorithm type
            const algorithmTypes = ['BruteForce', 'DLX', 'CP'];
            const algorithmData = {};

            algorithmTypes.forEach(algo => {
                const filtered = metricsData.filter(d => {
                    const algoType = d.algorithmType || 'BruteForce';
                    return algoType === algo && d.results && d.results.length > 0;
                });

                if (filtered.length > 0) {
                    // Calculate averages across all results
                    let totalTime = 0;
                    let totalIterations = 0;
                    let resultCount = 0;

                    filtered.forEach(lang => {
                        lang.results.forEach(r => {
                            totalTime += r.time;
                            totalIterations += r.iterations || 0;
                            resultCount++;
                        });
                    });

                    algorithmData[algo] = {
                        avgTime: totalTime / resultCount,
                        avgIterations: totalIterations / resultCount,
                        languageCount: filtered.length
                    };
                }
            });

            // Only show algorithms with data
            const availableAlgos = Object.keys(algorithmData);
            if (availableAlgos.length === 0) {
                container.html("<div style='color:#ff4444; padding:40px; text-align:center; font-family:monospace;'><h3>No algorithm data available</h3></div>");
                return;
            }

            const svg = container.append("svg")
                .attr("width", width)
                .attr("height", height);

            const g = svg.append("g")
                .attr("transform", `translate(${margin.left},${margin.top})`);

            // Title
            g.append("text")
                .attr("x", chartWidth / 2)
                .attr("y", -40)
                .attr("text-anchor", "middle")
                .style("font-size", "24px")
                .style("font-weight", "bold")
                .style("fill", "#00ff9d")
                .style("font-family", "monospace")
                .text("Algorithm Comparison");

            // X scale - algorithm types
            const x0 = d3.scaleBand()
                .domain(availableAlgos)
                .range([0, chartWidth])
                .padding(0.2);

            // X1 scale for grouped bars (3 metrics per algorithm)
            const x1 = d3.scaleBand()
                .domain(['time', 'iterations', 'efficiency'])
                .range([0, x0.bandwidth()])
                .padding(0.05);

            // Y scale - log scale to handle wide range
            const allValues = availableAlgos.flatMap(algo => [
                algorithmData[algo].avgTime,
                algorithmData[algo].avgIterations / 1000, // Scale down iterations
                algorithmData[algo].avgTime * 10 // Efficiency proxy
            ]);

            const yMin = Math.max(0.001, d3.min(allValues) * 0.5);
            const yMax = d3.max(allValues) * 1.5;

            const y = d3.scaleLog()
                .domain([yMin, yMax])
                .range([chartHeight, 0])
                .nice();

            // Colors for the three metrics
            const colors = {
                time: '#00ff9d',
                iterations: '#00d4ff',
                efficiency: '#ff9d00'
            };

            // Draw bars for each algorithm
            const algoGroups = g.selectAll(".algo-group")
                .data(availableAlgos)
                .enter().append("g")
                .attr("class", "algo-group")
                .attr("transform", d => `translate(${x0(d)},0)`);

            // Time bars
            algoGroups.append("rect")
                .attr("x", x1('time'))
                .attr("y", d => y(algorithmData[d].avgTime))
                .attr("width", x1.bandwidth())
                .attr("height", d => chartHeight - y(algorithmData[d].avgTime))
                .attr("fill", colors.time)
                .attr("opacity", 0.8)
                .on("mouseover", function(event, d) {
                    d3.select(this).attr("opacity", 1);
                    g.append("text")
                        .attr("class", "tooltip-text")
                        .attr("x", x0(d) + x1('time') + x1.bandwidth() / 2)
                        .attr("y", y(algorithmData[d].avgTime) - 10)
                        .attr("text-anchor", "middle")
                        .style("fill", "#00ff9d")
                        .style("font-family", "monospace")
                        .style("font-size", "12px")
                        .text(`${algorithmData[d].avgTime.toFixed(3)}s`);
                })
                .on("mouseout", function() {
                    d3.select(this).attr("opacity", 0.8);
                    g.selectAll(".tooltip-text").remove();
                });

            // Iterations bars (scaled down by 1000 for visibility)
            algoGroups.append("rect")
                .attr("x", x1('iterations'))
                .attr("y", d => y(algorithmData[d].avgIterations / 1000))
                .attr("width", x1.bandwidth())
                .attr("height", d => chartHeight - y(algorithmData[d].avgIterations / 1000))
                .attr("fill", colors.iterations)
                .attr("opacity", 0.8)
                .on("mouseover", function(event, d) {
                    d3.select(this).attr("opacity", 1);
                    g.append("text")
                        .attr("class", "tooltip-text")
                        .attr("x", x0(d) + x1('iterations') + x1.bandwidth() / 2)
                        .attr("y", y(algorithmData[d].avgIterations / 1000) - 10)
                        .attr("text-anchor", "middle")
                        .style("fill", "#00d4ff")
                        .style("font-family", "monospace")
                        .style("font-size", "12px")
                        .text(`${Math.round(algorithmData[d].avgIterations)} iter`);
                })
                .on("mouseout", function() {
                    d3.select(this).attr("opacity", 0.8);
                    g.selectAll(".tooltip-text").remove();
                });

            // Efficiency bars (time * 10 as proxy)
            algoGroups.append("rect")
                .attr("x", x1('efficiency'))
                .attr("y", d => y(algorithmData[d].avgTime * 10))
                .attr("width", x1.bandwidth())
                .attr("height", d => chartHeight - y(algorithmData[d].avgTime * 10))
                .attr("fill", colors.efficiency)
                .attr("opacity", 0.8)
                .on("mouseover", function(event, d) {
                    d3.select(this).attr("opacity", 1);
                    g.append("text")
                        .attr("class", "tooltip-text")
                        .attr("x", x0(d) + x1('efficiency') + x1.bandwidth() / 2)
                        .attr("y", y(algorithmData[d].avgTime * 10) - 10)
                        .attr("text-anchor", "middle")
                        .style("fill", "#ff9d00")
                        .style("font-family", "monospace")
                        .style("font-size", "12px")
                        .text(`${algorithmData[d].languageCount} langs`);
                })
                .on("mouseout", function() {
                    d3.select(this).attr("opacity", 0.8);
                    g.selectAll(".tooltip-text").remove();
                });

            // X axis
            g.append("g")
                .attr("transform", `translate(0,${chartHeight})`)
                .call(d3.axisBottom(x0))
                .selectAll("text")
                .style("fill", "#00ff9d")
                .style("font-family", "monospace")
                .style("font-size", "14px");

            // Y axis
            g.append("g")
                .call(d3.axisLeft(y).ticks(5, ".2f"))
                .selectAll("text")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace");

            // Y axis label
            g.append("text")
                .attr("transform", "rotate(-90)")
                .attr("y", -60)
                .attr("x", -chartHeight / 2)
                .attr("text-anchor", "middle")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace")
                .style("font-size", "12px")
                .text("Log Scale");

            // Legend
            const legend = g.append("g")
                .attr("transform", `translate(${chartWidth - 200}, -30)`);

            const legendItems = [
                { label: 'Avg Time (s)', color: colors.time },
                { label: 'Avg Iterations (÷1000)', color: colors.iterations },
                { label: 'Languages Count', color: colors.efficiency }
            ];

            legendItems.forEach((item, i) => {
                const legendRow = legend.append("g")
                    .attr("transform", `translate(0, ${i * 20})`);

                legendRow.append("rect")
                    .attr("width", 12)
                    .attr("height", 12)
                    .attr("fill", item.color)
                    .attr("opacity", 0.8);

                legendRow.append("text")
                    .attr("x", 18)
                    .attr("y", 10)
                    .style("fill", "#fff")
                    .style("font-family", "monospace")
                    .style("font-size", "11px")
                    .text(item.label);
            });
        } // End of drawAlgorithmComparisonChart

        function drawLanguagePerformanceChart() {
            const container = d3.select("#d3-chart-container");
            const width = document.getElementById('d3-chart-container').clientWidth;
            const height = document.getElementById('d3-chart-container').clientHeight;
            const margin = { top: 80, right: 120, bottom: 60, left: 180 };
            const chartWidth = width - margin.left - margin.right;
            const chartHeight = height - margin.top - margin.bottom;

            // Get current algorithm filter (default to BruteForce if not set)
            const selectedAlgo = window.currentAlgorithm || 'BruteForce';
            const algoLabel = {
                'BruteForce': 'Brute Force',
                'DLX': 'Dancing Links',
                'CP': 'Constraint Propagation',
                'all': 'All Algorithms'
            }[selectedAlgo] || 'BruteForce';

            // Filter data by algorithm type
            const filtered = metricsData.filter(d => {
                const algoType = d.algorithmType || 'BruteForce';
                if (selectedAlgo === 'all') return true;
                return algoType === selectedAlgo;
            });

            if (filtered.length === 0) {
                container.html("<div style='color:#ff4444; padding:40px; text-align:center; font-family:monospace;'><h3>No data for selected algorithm</h3></div>");
                return;
            }

            // Calculate average time for each language
            const langStats = filtered.map(lang => {
                const times = lang.results.map(r => r.time);
                const avgTime = times.reduce((a, b) => a + b, 0) / times.length;
                const algoType = lang.algorithmType || 'BruteForce';

                // Determine display name with algorithm suffix
                let displayName = lang.solver;
                if (selectedAlgo === 'all') {
                    // Add suffix for all algorithm types in "All Algorithms" mode
                    const algoSuffix = algoType === 'DLX' ? ' (DLX)' :
                                      algoType === 'CP' ? ' (CP)' : ' (BF)';
                    displayName = lang.solver + algoSuffix;
                }

                return {
                    solver: lang.solver,
                    displayName: displayName,
                    avgTime: avgTime,
                    matrixCount: lang.results.length,
                    algorithmType: algoType
                };
            });

            // Sort by performance (fastest first) and take top 15
            langStats.sort((a, b) => a.avgTime - b.avgTime);
            const topLanguages = langStats.slice(0, 15);

            // Get C baselines for each algorithm type
            const cBaselinesByAlgo = {};
            ['BruteForce', 'DLX', 'CP'].forEach(algo => {
                const cEntry = metricsData.find(d => {
                    const algoType = d.algorithmType || 'BruteForce';
                    return d.solver === 'C' && algoType === algo;
                });
                if (cEntry) {
                    cBaselinesByAlgo[algo] = cEntry.results.reduce((a, b) => a + b.time, 0) / cEntry.results.length;
                }
            });

            // For single algorithm mode, use the specific C baseline (backward compat)
            const cEntry = filtered.find(d => d.solver === 'C');
            const cBaseline = cEntry ? cEntry.results.reduce((a, b) => a + b.time, 0) / cEntry.results.length : null;

            const svg = container.append("svg")
                .attr("width", width)
                .attr("height", height);

            const g = svg.append("g")
                .attr("transform", `translate(${margin.left},${margin.top})`);

            // Title
            g.append("text")
                .attr("x", chartWidth / 2)
                .attr("y", -40)
                .attr("text-anchor", "middle")
                .style("font-size", "24px")
                .style("font-weight", "bold")
                .style("fill", "#00ff9d")
                .style("font-family", "monospace")
                .text(`Top Languages - ${algoLabel}`);

            // X scale - log scale for time
            const xMin = Math.max(0.001, d3.min(topLanguages, d => d.avgTime) * 0.5);
            const xMax = d3.max(topLanguages, d => d.avgTime) * 1.2;

            const x = d3.scaleLog()
                .domain([xMin, xMax])
                .range([0, chartWidth])
                .nice();

            // Y scale - languages
            const y = d3.scaleBand()
                .domain(topLanguages.map(d => d.displayName))
                .range([0, chartHeight])
                .padding(0.2);

            // Color scale - gradient from green (fast) to red (slow)
            const colorScale = d3.scaleSequential()
                .domain([0, topLanguages.length - 1])
                .interpolator(t => d3.interpolateRgb("#00ff9d", "#ff4444")(t));

            // Draw C baseline reference line(s)
            if (selectedAlgo === 'all') {
                // In "All Algorithms" mode, show multiple C baselines
                const baselineColors = {
                    'BruteForce': '#00ff9d',
                    'DLX': '#7aa2f7',
                    'CP': '#bb9af7'
                };
                const baselineLabels = {
                    'BruteForce': 'BF/C',
                    'DLX': 'DLX/C',
                    'CP': 'CP/C'
                };

                Object.entries(cBaselinesByAlgo).forEach(([algo, baseline]) => {
                    g.append("line")
                        .attr("x1", x(baseline))
                        .attr("x2", x(baseline))
                        .attr("y1", 0)
                        .attr("y2", chartHeight)
                        .attr("stroke", baselineColors[algo])
                        .attr("stroke-width", 2)
                        .attr("stroke-dasharray", "4,4")
                        .attr("opacity", 0.4);

                    g.append("text")
                        .attr("x", x(baseline))
                        .attr("y", -10)
                        .attr("text-anchor", "middle")
                        .style("fill", baselineColors[algo])
                        .style("font-family", "monospace")
                        .style("font-size", "10px")
                        .text(`${baselineLabels[algo]}: ${baseline.toFixed(3)}s`);
                });
            } else if (cBaseline) {
                // Single algorithm mode - show one C baseline
                g.append("line")
                    .attr("x1", x(cBaseline))
                    .attr("x2", x(cBaseline))
                    .attr("y1", 0)
                    .attr("y2", chartHeight)
                    .attr("stroke", "#00ff9d")
                    .attr("stroke-width", 2)
                    .attr("stroke-dasharray", "4,4")
                    .attr("opacity", 0.6);

                g.append("text")
                    .attr("x", x(cBaseline))
                    .attr("y", -10)
                    .attr("text-anchor", "middle")
                    .style("fill", "#00ff9d")
                    .style("font-family", "monospace")
                    .style("font-size", "11px")
                    .text(`C Baseline: ${cBaseline.toFixed(3)}s`);
            }

            // Draw bars
            const bars = g.selectAll(".lang-bar")
                .data(topLanguages)
                .enter().append("g")
                .attr("class", "lang-bar");

            bars.append("rect")
                .attr("x", 0)
                .attr("y", d => y(d.displayName))
                .attr("width", d => x(d.avgTime))
                .attr("height", y.bandwidth())
                .attr("fill", (d, i) => colorScale(i))
                .attr("opacity", 0.8)
                .on("mouseover", function(event, d) {
                    d3.select(this).attr("opacity", 1);
                })
                .on("mouseout", function() {
                    d3.select(this).attr("opacity", 0.8);
                });

            // Add delta % labels on bars
            bars.append("text")
                .attr("x", d => x(d.avgTime) + 5)
                .attr("y", d => y(d.displayName) + y.bandwidth() / 2)
                .attr("dy", "0.35em")
                .style("fill", "#fff")
                .style("font-family", "monospace")
                .style("font-size", "11px")
                .text(d => {
                    // In "All Algorithms" mode, use algorithm-specific C baseline
                    let baseline;
                    if (selectedAlgo === 'all') {
                        const langAlgo = d.algorithmType || 'BruteForce';
                        baseline = cBaselinesByAlgo[langAlgo];
                    } else {
                        baseline = cBaseline;
                    }

                    if (baseline && d.avgTime > 0) {
                        const delta = ((d.avgTime / baseline - 1) * 100).toFixed(0);
                        return delta >= 0 ? `+${delta}%` : `${delta}%`;
                    }
                    return `${d.avgTime.toFixed(3)}s`;
                });

            // Y axis - language names
            const yAxis = g.append("g")
                .call(d3.axisLeft(y));

            yAxis.selectAll("text")
                .style("fill", "#00ff9d")
                .style("font-family", "monospace")
                .style("font-size", "13px")
                .style("font-weight", "bold");

            // Add algorithm type coloring to Y-axis labels when in "All Algorithms" mode
            if (selectedAlgo === 'all') {
                const langMap = new Map(topLanguages.map(d => [d.displayName, d]));

                yAxis.selectAll("text").each(function(displayName) {
                    const langData = langMap.get(displayName);
                    if (!langData) return;

                    const algoType = langData.algorithmType || 'BruteForce';

                    // Color the algorithm type suffix in the label
                    const algoColors = {
                        'BF': '#00ff9d',
                        'DLX': '#7aa2f7',
                        'CP': '#bb9af7'
                    };

                    // Change color of the "(BF)", "(DLX)", or "(CP)" suffix
                    const textEl = d3.select(this);
                    const fullText = textEl.text();
                    const match = fullText.match(/^(.+?)(\s*\((BF|DLX|CP)\))$/);

                    if (match) {
                        const baseName = match[1];
                        const suffix = match[2];
                        const algoName = match[3];

                        // Clear existing text
                        textEl.text('');

                        // Add base name in default color
                        textEl.append('tspan')
                            .style('fill', '#00ff9d')
                            .text(baseName + ' ');

                        // Add algorithm suffix in algorithm color
                        textEl.append('tspan')
                            .style('fill', algoColors[algoName])
                            .style('font-weight', 'bold')
                            .text(`(${algoName})`);
                    }
                });
            }

            // X axis
            g.append("g")
                .attr("transform", `translate(0,${chartHeight})`)
                .call(d3.axisBottom(x).ticks(5, ".3f"))
                .selectAll("text")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace");

            // X axis label
            g.append("text")
                .attr("x", chartWidth / 2)
                .attr("y", chartHeight + 45)
                .attr("text-anchor", "middle")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace")
                .style("font-size", "12px")
                .text("Average Time (seconds, log scale)");

            // Legend
            const legend = g.append("g")
                .attr("transform", `translate(${chartWidth + 10}, 20)`);

            legend.append("text")
                .attr("x", 0)
                .attr("y", 0)
                .style("fill", "#fff")
                .style("font-family", "monospace")
                .style("font-size", "11px")
                .style("font-weight", "bold")
                .text("Performance");

            // Color gradient legend
            const gradientLegend = legend.append("g")
                .attr("transform", "translate(0, 15)");

            const gradientHeight = 100;
            const gradientStops = 10;

            for (let i = 0; i < gradientStops; i++) {
                gradientLegend.append("rect")
                    .attr("x", 0)
                    .attr("y", i * (gradientHeight / gradientStops))
                    .attr("width", 15)
                    .attr("height", gradientHeight / gradientStops)
                    .attr("fill", colorScale(i * (topLanguages.length - 1) / (gradientStops - 1)));
            }

            gradientLegend.append("text")
                .attr("x", 20)
                .attr("y", 0)
                .attr("dy", "0.7em")
                .style("fill", "#00ff9d")
                .style("font-family", "monospace")
                .style("font-size", "10px")
                .text("Fast");

            gradientLegend.append("text")
                .attr("x", 20)
                .attr("y", gradientHeight)
                .attr("dy", "-0.3em")
                .style("fill", "#ff4444")
                .style("font-family", "monospace")
                .style("font-size", "10px")
                .text("Slow");
        } // End of drawLanguagePerformanceChart

        function drawIterationCountChart() {
            const container = d3.select("#d3-chart-container");
            const width = document.getElementById('d3-chart-container').clientWidth;
            const height = document.getElementById('d3-chart-container').clientHeight;
            const margin = { top: 80, right: 150, bottom: 80, left: 100 };
            const chartWidth = width - margin.left - margin.right;
            const chartHeight = height - margin.top - margin.bottom;

            // Get current algorithm filter (default to BruteForce if not set)
            const selectedAlgo = window.currentAlgorithm || 'BruteForce';
            const algoLabel = {
                'BruteForce': 'Brute Force',
                'DLX': 'Dancing Links',
                'CP': 'Constraint Propagation',
                'all': 'All Algorithms'
            }[selectedAlgo] || 'BruteForce';

            // Filter data by algorithm type
            const filtered = metricsData.filter(d => {
                const algoType = d.algorithmType || 'BruteForce';
                if (selectedAlgo === 'all') return selectedAlgo === 'all';
                return algoType === selectedAlgo;
            });

            if (filtered.length === 0) {
                container.html("<div style='color:#ff4444; padding:40px; text-align:center; font-family:monospace;'><h3>No data for selected algorithm</h3></div>");
                return;
            }

            // Extract iteration data across matrices for each language
            const languageData = filtered.map(lang => {
                // Create a map of matrix -> iterations
                const matrixIterations = {};
                lang.results.forEach(r => {
                    if (r.iterations) {
                        matrixIterations[r.matrix] = r.iterations;
                    }
                });

                // Only include languages with at least 4 matrices of data
                const matrixCount = Object.keys(matrixIterations).length;

                return {
                    solver: lang.solver,
                    matrixIterations: matrixIterations,
                    matrixCount: matrixCount,
                    totalIterations: Object.values(matrixIterations).reduce((a, b) => a + b, 0),
                    algorithmType: lang.algorithmType || 'BruteForce'
                };
            }).filter(d => d.matrixCount >= 4); // Filter to languages with at least 4 matrices

            if (languageData.length === 0) {
                container.html("<div style='color:#ff4444; padding:40px; text-align:center; font-family:monospace;'><h3>No sufficient data for iteration chart</h3></div>");
                return;
            }

            // Sort by total iterations and take top 10
            languageData.sort((a, b) => a.totalIterations - b.totalIterations);
            const topLanguages = languageData.slice(0, 10);

            // Find C baseline
            const cLang = topLanguages.find(d => d.solver === 'C');

            const svg = container.append("svg")
                .attr("width", width)
                .attr("height", height);

            const g = svg.append("g")
                .attr("transform", `translate(${margin.left},${margin.top})`);

            // Title
            g.append("text")
                .attr("x", chartWidth / 2)
                .attr("y", -40)
                .attr("text-anchor", "middle")
                .style("font-size", "24px")
                .style("font-weight", "bold")
                .style("fill", "#00ff9d")
                .style("font-family", "monospace")
                .text(`Iteration Counts - ${algoLabel} Across Matrices`);

            // Get all matrices present in the data
            const allMatrices = Array.from(new Set(
                topLanguages.flatMap(lang => Object.keys(lang.matrixIterations).map(Number))
            )).sort((a, b) => a - b);

            // X scale - matrix numbers
            const x = d3.scaleLinear()
                .domain([d3.min(allMatrices), d3.max(allMatrices)])
                .range([0, chartWidth]);

            // Y scale - iteration count (log scale for wide range)
            const allIterations = topLanguages.flatMap(lang => Object.values(lang.matrixIterations));
            const yMin = Math.max(1, d3.min(allIterations) * 0.5);
            const yMax = d3.max(allIterations) * 1.5;

            const y = d3.scaleLog()
                .domain([yMin, yMax])
                .range([chartHeight, 0])
                .nice();

            // Color scale for languages
            const colorScale = d3.scaleOrdinal(d3.schemeCategory10);

            // Line generator
            const line = d3.line()
                .x(d => x(d.matrix))
                .y(d => y(d.iterations))
                .defined(d => d.iterations !== null && d.iterations !== undefined);

            // Draw C baseline first (so it's most prominent)
            if (cLang) {
                const cData = allMatrices.map(m => ({
                    matrix: m,
                    iterations: cLang.matrixIterations[m] || null
                })).filter(d => d.iterations !== null);

                g.append("path")
                    .datum(cData)
                    .attr("class", "line-c")
                    .attr("fill", "none")
                    .attr("stroke", "#00ff9d")
                    .attr("stroke-width", 3)
                    .attr("d", line);

                // Add circles for C baseline
                g.selectAll(".dot-c")
                    .data(cData)
                    .enter().append("circle")
                    .attr("class", "dot-c")
                    .attr("cx", d => x(d.matrix))
                    .attr("cy", d => y(d.iterations))
                    .attr("r", 4)
                    .attr("fill", "#00ff9d")
                    .attr("stroke", "#000")
                    .attr("stroke-width", 1)
                    .on("mouseover", function(event, d) {
                        d3.select(this).attr("r", 6);

                        // Show tooltip
                        const tooltip = g.append("g")
                            .attr("class", "tooltip")
                            .attr("transform", `translate(${x(d.matrix)},${y(d.iterations) - 20})`);

                        const text = tooltip.append("text")
                            .attr("text-anchor", "middle")
                            .style("font-family", "monospace")
                            .style("font-size", "12px")
                            .style("fill", "#00ff9d")
                            .text(`C: Matrix ${d.matrix}, ${d.iterations.toLocaleString()} iter`);

                        const bbox = text.node().getBBox();
                        tooltip.insert("rect", "text")
                            .attr("x", bbox.x - 5)
                            .attr("y", bbox.y - 2)
                            .attr("width", bbox.width + 10)
                            .attr("height", bbox.height + 4)
                            .attr("fill", "#1a1a1a")
                            .attr("stroke", "#00ff9d")
                            .attr("stroke-width", 1)
                            .attr("rx", 3);
                    })
                    .on("mouseout", function() {
                        d3.select(this).attr("r", 4);
                        g.selectAll(".tooltip").remove();
                    });
            }

            // Draw lines for other languages
            const otherLanguages = topLanguages.filter(lang => lang.solver !== 'C');

            otherLanguages.forEach((lang, idx) => {
                const langData = allMatrices.map(m => ({
                    matrix: m,
                    iterations: lang.matrixIterations[m] || null,
                    solver: lang.solver
                })).filter(d => d.iterations !== null);

                const color = colorScale(idx);

                g.append("path")
                    .datum(langData)
                    .attr("class", `line-${idx}`)
                    .attr("fill", "none")
                    .attr("stroke", color)
                    .attr("stroke-width", 1.5)
                    .attr("opacity", 0.8)
                    .attr("d", line)
                    .on("mouseover", function() {
                        d3.select(this)
                            .attr("stroke-width", 2.5)
                            .attr("opacity", 1);
                    })
                    .on("mouseout", function() {
                        d3.select(this)
                            .attr("stroke-width", 1.5)
                            .attr("opacity", 0.8);
                    });

                // Add circles
                g.selectAll(`.dot-${idx}`)
                    .data(langData)
                    .enter().append("circle")
                    .attr("class", `dot-${idx}`)
                    .attr("cx", d => x(d.matrix))
                    .attr("cy", d => y(d.iterations))
                    .attr("r", 3)
                    .attr("fill", color)
                    .attr("stroke", "#000")
                    .attr("stroke-width", 1)
                    .on("mouseover", function(event, d) {
                        d3.select(this).attr("r", 5);

                        // Highlight line
                        g.select(`.line-${idx}`)
                            .attr("stroke-width", 2.5)
                            .attr("opacity", 1);

                        // Show tooltip
                        const tooltip = g.append("g")
                            .attr("class", "tooltip")
                            .attr("transform", `translate(${x(d.matrix)},${y(d.iterations) - 20})`);

                        const text = tooltip.append("text")
                            .attr("text-anchor", "middle")
                            .style("font-family", "monospace")
                            .style("font-size", "12px")
                            .style("fill", color)
                            .text(`${d.solver}: Matrix ${d.matrix}, ${d.iterations.toLocaleString()} iter`);

                        const bbox = text.node().getBBox();
                        tooltip.insert("rect", "text")
                            .attr("x", bbox.x - 5)
                            .attr("y", bbox.y - 2)
                            .attr("width", bbox.width + 10)
                            .attr("height", bbox.height + 4)
                            .attr("fill", "#1a1a1a")
                            .attr("stroke", color)
                            .attr("stroke-width", 1)
                            .attr("rx", 3);
                    })
                    .on("mouseout", function() {
                        d3.select(this).attr("r", 3);
                        g.select(`.line-${idx}`)
                            .attr("stroke-width", 1.5)
                            .attr("opacity", 0.8);
                        g.selectAll(".tooltip").remove();
                    });
            });

            // X axis
            g.append("g")
                .attr("transform", `translate(0,${chartHeight})`)
                .call(d3.axisBottom(x).ticks(allMatrices.length).tickFormat(d => `Matrix ${d}`))
                .selectAll("text")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace")
                .style("font-size", "11px")
                .attr("transform", "rotate(-15)")
                .attr("text-anchor", "end");

            // Y axis
            g.append("g")
                .call(d3.axisLeft(y).ticks(8, ".0f"))
                .selectAll("text")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace")
                .style("font-size", "11px");

            // Y axis label
            g.append("text")
                .attr("transform", "rotate(-90)")
                .attr("x", -chartHeight / 2)
                .attr("y", -60)
                .attr("text-anchor", "middle")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace")
                .style("font-size", "12px")
                .text("Iteration Count (log scale)");

            // X axis label
            g.append("text")
                .attr("x", chartWidth / 2)
                .attr("y", chartHeight + 60)
                .attr("text-anchor", "middle")
                .style("fill", "#5c5c66")
                .style("font-family", "monospace")
                .style("font-size", "12px")
                .text("Matrix Difficulty");

            // Legend
            const legend = g.append("g")
                .attr("transform", `translate(${chartWidth + 20}, 0)`);

            legend.append("text")
                .attr("x", 0)
                .attr("y", 0)
                .style("fill", "#fff")
                .style("font-family", "monospace")
                .style("font-size", "12px")
                .style("font-weight", "bold")
                .text("Languages");

            // C baseline in legend
            if (cLang) {
                const cLegend = legend.append("g")
                    .attr("transform", `translate(0, 20)`);

                cLegend.append("line")
                    .attr("x1", 0)
                    .attr("x2", 20)
                    .attr("y1", 0)
                    .attr("y2", 0)
                    .attr("stroke", "#00ff9d")
                    .attr("stroke-width", 3);

                cLegend.append("text")
                    .attr("x", 25)
                    .attr("y", 0)
                    .attr("dy", "0.35em")
                    .style("fill", "#00ff9d")
                    .style("font-family", "monospace")
                    .style("font-size", "11px")
                    .text("C (baseline)");
            }

            // Other languages in legend
            otherLanguages.forEach((lang, idx) => {
                const yPos = 20 + (cLang ? 20 : 0) + (idx * 18);
                const legendItem = legend.append("g")
                    .attr("transform", `translate(0, ${yPos})`)
                    .style("cursor", "pointer")
                    .on("click", function() {
                        // Toggle line visibility
                        const line = g.select(`.line-${idx}`);
                        const currentOpacity = line.attr("opacity");
                        const newOpacity = currentOpacity === "0" ? "0.8" : "0";
                        line.attr("opacity", newOpacity);

                        g.selectAll(`.dot-${idx}`).attr("opacity", newOpacity === "0" ? 0 : 1);

                        // Update legend text opacity
                        d3.select(this).style("opacity", newOpacity === "0" ? 0.3 : 1);
                    });

                legendItem.append("line")
                    .attr("x1", 0)
                    .attr("x2", 20)
                    .attr("y1", 0)
                    .attr("y2", 0)
                    .attr("stroke", colorScale(idx))
                    .attr("stroke-width", 2);

                legendItem.append("text")
                    .attr("x", 25)
                    .attr("y", 0)
                    .attr("dy", "0.35em")
                    .style("fill", "#fff")
                    .style("font-family", "monospace")
                    .style("font-size", "10px")
                    .text(lang.solver);
            });
        } // End of drawIterationCountChart

        // Initial Draw
        if (typeof d3 !== 'undefined') {
            drawLineChart();
        } else {
            throw new Error("D3.js not loaded");
        }

    } catch (e) {
        console.error("Error initializing D3 charts:", e);
        // If initialization fails, switchChart is still defined and will show error when clicked
        // Show error immediately
        window.switchChart('line');
    }
})();
// End of D3 closure

// Fullscreen change handler - resize charts when exiting fullscreen
document.addEventListener('fullscreenchange', function() {
    if (!document.fullscreenElement && window.currentChart && typeof window.switchChart === 'function') {
        // User exited fullscreen - redraw the current chart to fit normal container
        console.log('Exited fullscreen, resizing chart:', window.currentChart);
        setTimeout(() => {
            window.switchChart(window.currentChart);
        }, 100); // Small delay to allow DOM to settle
    }
});

// Beginning of Matrix Screensaver closure
// Inject Matrix Data
// Matrix Data injected globally
// const matrixPuzzles = ...

// Matrix Screensaver Logic
// Define variables in outer scope to be accessible
let startScreensaverGlobal;

(function () {
    try {


        console.log("Initializing Matrix Screensaver...");
        const canvas = document.getElementById('matrix-canvas');
        if (!canvas) console.error("Matrix canvas not found!");

        const ctx = canvas.getContext('2d');
        const container = canvas.parentElement;
        const chartContainer = document.getElementById('d3-chart-container');

        let width, height;
        let columns;
        let active = false;
        let ignoreInput = false;
        let animationId;
        let frame = 0;

        // Scroll position management for smooth screensaver transitions
        let savedScrollX = 0;
        let savedScrollY = 0;

        // Expose globally immediately
        window.startScreensaver = startScreensaver;
        startScreensaverGlobal = startScreensaver;

        // Puzzle Overlay State
        let puzzleY = -1000; // Start way above
        let currentPuzzleIndex = 0;
        let puzzleLines = [];

        // Puzzle Colors
        let puzzleColorIndex = 0;
        const puzzleColors = [
            '#0f0',           // Classic Matrix Green
            '#00ffff',        // Neon Cyan
            '#ff00ff',        // Neon Magenta
            '#ffff00',        // Neon Yellow
            '#ff0080',        // Deep Pink
            '#ff4500'         // Neon Orange
        ];

        // Expose to window for control
        window.cyclePuzzleColor = function () {
            puzzleColorIndex = (puzzleColorIndex + 1) % puzzleColors.length;
        };

        function prepareNextPuzzle() {
            if (matrixPuzzles.length === 0) return;

            let attempts = 0;
            // Try up to length times to find a valid puzzle
            if (!matrixPuzzles || matrixPuzzles.length === 0) {
                console.warn("Matrix puzzles empty, using fallback.");
                puzzleLines = ["M A T R I X", "S Y S T E M", "F A I L U R E", "0 1 0 1 0 1"];
                currentPuzzleIndex = 0;
                return;
            }

            while (attempts < matrixPuzzles.length) {
                const rawText = matrixPuzzles[currentPuzzleIndex];

                // Robust split: handles literal '\n' (from JSON) and real newlines
                // Also trims lines to avoid empty entries
                let lines = rawText.split(/\r?\n|\\n/);
                lines = lines.map(l => l.trim()).filter(l => l.length > 0);

                // Check if puzzle is substantial enough to show
                if (lines.length > 3) {
                    puzzleLines = lines;
                    console.log('Prepared puzzle index', currentPuzzleIndex, 'with', puzzleLines.length, 'lines');

                    // Start from the BOTTOM of the screen
                    puzzleY = height || window.innerHeight;

                    // Advance index for next time
                    currentPuzzleIndex = (currentPuzzleIndex + 1) % matrixPuzzles.length;
                    specialRows.clear(); // Reset special rows for new puzzle
                    return;
                }

                // If we get here, puzzle was too short or empty
                console.warn('Skipping invalid/short puzzle at index', currentPuzzleIndex);
                currentPuzzleIndex = (currentPuzzleIndex + 1) % matrixPuzzles.length;
                attempts++;
            }

            console.error("No valid puzzles found after checking all candidates.");
            puzzleLines = [];
        }

        // Matrix characters (Katakana + Latin)
        const chars = '/Cars10アァカサタナハマヤャラワガザダバパイィキシチニヒミリヰギジヂビピウゥクスツヌフムユュルグズブヅプエェケセテネヘメレヱゲゼデベペオォコソトノホモヨョロヲゴゾドボポヴッン0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

        function resize() {
            if (document.fullscreenElement) {
                canvas.style.position = 'fixed';
                canvas.style.top = '0';
                canvas.style.left = '0';
                canvas.style.width = '100vw';
                canvas.style.height = '100vh';
                canvas.style.zIndex = '1000';
                width = window.innerWidth;
                height = window.innerHeight;
            } else {
                canvas.style.position = 'absolute';
                canvas.style.top = '0';
                canvas.style.left = '0';
                canvas.style.width = '100%';
                canvas.style.height = '100%';
                canvas.style.zIndex = '10';
                // Use current parent, which might change
                const parent = canvas.parentElement || container;
                width = parent.clientWidth;
                height = parent.clientHeight;
            }

            canvas.width = width;
            canvas.height = height;

            const fontSize = 14;
            columns = Math.ceil(width / fontSize);

            // Initialize drops and state
            drops = [];
            cars10State = []; // Track index for each column
            for (let i = 0; i < columns; i++) {
                drops[i] = Math.random() * -100; // Start above screen
                cars10State[i] = -1; // -1 means normal random char
            }
        }

        // Glow State
        let glowHue = 0; // 0 for Red, 240 for Blue
        let glowDirection = 1;

        // Dancing Highlight State
        let dancingRow = 0;
        let dancingCol = 0;
        let lastDanceTime = 0;

        // Slide-in Animation State
        let slideInComplete = false;

        // Easter Egg State
        let lastCars10Time = 0;
        let specialRows = new Set();
        let cars10State = []; // Added state array
        const secretMessage = "/Cars10 "; // Normal reading order with trailing space

        let currentMode = 'red'; // Default to full screen

        function draw() {
            if (active) {
                animationId = requestAnimationFrame(draw);
            }

            frame++;
            if (frame % 2 !== 0) return;

            // Black background with opacity for trail effect
            // Lower opacity = longer trails
            ctx.fillStyle = 'rgba(0, 0, 0, 0.03)';
            ctx.fillRect(0, 0, width, height);

            ctx.fillStyle = '#0F0'; // Green text
            ctx.font = '14px monospace';

            // Draw Matrix Rain
            for (let i = 0; i < drops.length; i++) {
                const x = i * 14;
                const y = drops[i] * 14;
                // Easter Egg Logic
                let char = '';
                let isSpecial = false;

                // If this column is in special state
                if (cars10State[i] >= 0) {
                    const idx = cars10State[i] % secretMessage.length;
                    char = secretMessage[idx];
                    cars10State[i]++; // Move to next char for next frame
                    isSpecial = true;
                } else {
                    // Random chance to start secret
                    // Only if at top of screen to make it clean? Or anytime?
                    // Let's settle for random trigger anytime, but visually it looks best if distinct.
                    // Or maybe trigger when it resets?
                    // Let's stick to standard random char
                    char = chars.charAt(Math.floor(Math.random() * chars.length));
                }

                // Draw
                if (isSpecial) {
                    // Crystal Effect: White with Cyan/Magenta subtle glow
                    ctx.fillStyle = '#FFFFFF';
                    ctx.font = 'bold 14px monospace';

                    // Flash on entry (first 20 frames)
                    if (cars10State[i] < 20) {
                        ctx.shadowColor = '#FFFFFF';
                        ctx.shadowBlur = 15; // Bright flash
                        // Occasional flicker
                        if (Math.random() > 0.5) ctx.fillStyle = '#E0FFFF';
                    } else {
                        // Stable Crystal Glow
                        ctx.shadowColor = 'rgba(200, 255, 255, 0.8)'; // Ice blue glow
                        ctx.shadowBlur = 8;
                    }
                } else {
                    ctx.fillStyle = '#0F0';
                    ctx.font = '14px monospace';
                    // Glow Effect
                    ctx.shadowBlur = 8;
                    ctx.shadowColor = 'rgba(0, 255, 0, 0.5)';
                }

                ctx.fillText(char, x, y);

                // Reset
                ctx.shadowBlur = 0;

                // Reset shadows
                if (isSpecial) {
                    ctx.shadowBlur = 0;
                }

                // Reset drop to top randomly after it has crossed the screen
                if (y > height && Math.random() > 0.975) {
                    drops[i] = 0;
                    // Chance to become special when spawning new drop
                    if (Math.random() > 0.98) { // 2% chance per drop cycle
                        cars10State[i] = 0;
                    } else {
                        cars10State[i] = -1;
                    }
                }

                drops[i]++;
            }

            // Check if rain has reached halfway
            let rainHalfway = false;
            const gridHeight = height / 14; // 14px font for rain
            for (let i = 0; i < drops.length; i++) {
                if (drops[i] > gridHeight / 2) {
                    rainHalfway = true;
                    break;
                }
            }

            // Draw Puzzle Overlay (after slide-in completes)
            if (puzzleLines.length > 0 && slideInComplete && currentMode === 'red' && window.puzzleVisible) {
                // 1. Calculate Font Size
                // Goal: Row fits 2/3 of screen width
                const targetWidth = width * 0.66;
                const charCount = 17; // 9 digits + 8 spaces
                let fontSize = Math.floor(targetWidth / charCount * 1.6); // Multiplier to adjust

                // "Font size should also increase by 30%" - ONLY for Red Mode (Full Screen)
                // For Blue Mode (Chart), we scale it down a bit relative to that
                if (currentMode === 'red') {
                    fontSize = Math.floor(fontSize * 0.675); // Half the previous size
                } else {
                    // Blue mode: maybe keep it standard or slightly smaller
                    fontSize = Math.floor(fontSize * 0.8);
                }

                ctx.font = 'bold ' + fontSize + 'px "JetBrains Mono", monospace';
                ctx.textAlign = 'center';

                // Base color for fallback
                const color = '#FF0055'; // Bright neon red

                // Reduced glow for "fading trails" look
                ctx.shadowBlur = 5;
                ctx.shadowColor = color;
                ctx.fillStyle = color;

                const lineHeight = fontSize * 3.0; // Double spacing (simulates blank row)
                const startX = width / 2;

                // 3. Dancing Highlight Logic
                if (Date.now() - lastDanceTime > 200) {
                    dancingRow = Math.floor(Math.random() * 9);
                    dancingCol = Math.floor(Math.random() * 9);
                    lastDanceTime = Date.now();
                }

                // 4. Easter Egg Logic: /cars10
                if (Date.now() - lastCars10Time > 1000) { // Every 1 second
                    const visibleRows = [];
                    for (let i = 0; i < puzzleLines.length; i++) {
                        const lineY = puzzleY + (i * lineHeight);
                        // Check if fully visible on screen
                        if (lineY > lineHeight && lineY < height - lineHeight) {
                            visibleRows.push(i);
                        }
                    }

                    if (visibleRows.length > 0) {
                        // Pick a random row that isn't already special
                        const candidates = visibleRows.filter(r => !specialRows.has(r));
                        if (candidates.length > 0) {
                            const randomRow = candidates[Math.floor(Math.random() * candidates.length)];
                            specialRows.add(randomRow);
                        }
                    }
                    lastCars10Time = Date.now();
                }

                // Define columns to draw based on mode
                let xPositions = [];
                if (currentMode === 'red') {
                    // Single centered column for fullscreen
                    xPositions = [width * 0.5];
                } else {
                    // Single centered column for chart mode
                    xPositions = [width * 0.5];
                }

                // 3. Iterate over each column position
                for (const startX of xPositions) {
                    // Determine visible rows for this column
                    const visibleRows = [];
                    for (let i = 0; i < puzzleLines.length; i++) {
                        const lineY = puzzleY + (i * lineHeight);

                        // Only consider visible rows
                        if (lineY > -lineHeight * 0.2 && lineY < height + lineHeight * 0.2) {
                            visibleRows.push(i);
                        }
                    }

                    // No background clearing - completely transparent
                    // Numbers will appear directly over Matrix rain

                    // Draw each line of the puzzle
                    for (let i = 0; i < puzzleLines.length; i++) {
                        const lineY = puzzleY + (i * lineHeight);

                        // Only draw if visible
                        if (lineY > -lineHeight && lineY < height + lineHeight) {
                            const line = puzzleLines[i];



                            const parts = line.trim().split(/\s+/);

                            if (parts.length === 9) {
                                const spacing = fontSize * 1.2;
                                // Calculate total width of the puzzle block
                                const totalBlockWidth = (9 * fontSize * 1.2); // 9 chars * spacing

                                // Clear the entire puzzle area for this column to prevent streaks
                                // We do this ONCE per column loop, not per character, but here we are inside the loop.
                                // Actually, let's do it per character but simpler:
                                // Initialize starting X position for this row to center it
                                let currentX = startX - (totalBlockWidth / 2) + (spacing / 2);

                                for (let j = 0; j < parts.length; j++) {
                                    const isDancing = (i % 9 === dancingRow) && (j === dancingCol);

                                    // Calculate distance from center for scale effect
                                    const distFromCenter = Math.abs(lineY - height / 2);
                                    const maxDist = height / 2;
                                    const centerFactor = 1 - (distFromCenter / maxDist); // 1 at center, 0 at edges

                                    // Scale animation: grow at center (applies to all numbers)
                                    const scaleBoost = 1 + (centerFactor * 0.3); // 1.0 to 1.3x at center

                                    ctx.save();
                                    // Tall Numbers: Scale Y by 2, plus center boost
                                    ctx.translate(currentX, lineY);
                                    ctx.scale(scaleBoost, 2 * scaleBoost);

                                    // Draw a semi-transparent background box to hide streaks but keep it "semi transparent"
                                    ctx.globalCompositeOperation = 'source-over';
                                    ctx.fillStyle = 'rgba(0, 0, 0, 0.8)'; // Semi-transparent black
                                    // Clear from way above to way below
                                    ctx.fillRect(-fontSize * 0.6, -fontSize * 2, fontSize * 1.2, fontSize * 6);

                                    // Draw number
                                    ctx.globalCompositeOperation = 'source-over';
                                    ctx.shadowBlur = 0;
                                    ctx.shadowColor = 'transparent';
                                    const pColor = puzzleColors[puzzleColorIndex] || '#FF0055';
                                    ctx.strokeStyle = isDancing ? '#00b8ff' : pColor; // Blue if dancing, Dynamic otherwise
                                    ctx.lineWidth = isDancing ? 4 : 2;

                                    if (isDancing) {
                                        ctx.scale(1.5, 1.5); // Extra scale for dancing
                                        ctx.shadowBlur = 10;
                                        ctx.shadowColor = '#00b8ff';
                                    }
                                    ctx.strokeText(parts[j], 0, 0);

                                    ctx.restore();
                                    currentX += spacing;
                                }
                            } else {
                                // Header or other text
                                ctx.save();
                                ctx.translate(startX, lineY);
                                ctx.scale(1, 2);
                                ctx.strokeStyle = color;
                                ctx.lineWidth = 2;
                                ctx.strokeText(line, 0, 0);
                                ctx.restore();
                            }
                        }
                    }
                }

                // Reset shadow
                ctx.shadowBlur = 0;

                puzzleY -= 5; // Scroll UP (Ascent)

                // Reset if puzzle went off screen (top)
                // Total height of puzzle = lines * lineHeight
                const totalPuzzleHeight = puzzleLines.length * lineHeight;
                if (puzzleY < -totalPuzzleHeight) {
                    prepareNextPuzzle();
                }
            } else {
                // If not ready or no lines, ensure we are ready for next
                if (puzzleLines.length === 0) prepareNextPuzzle();
            }
        }


        window.setChartMode = function (mode) {
            if (currentChartMode === mode) return;

            // Deactivate current
            document.querySelectorAll('.chart-options button').forEach(b => b.classList.remove('active'));
            // Activate new (find button by text content or index - imperfect but works for now)
            // Better: add data-mode to buttons. For now, rely on text or order.
            // Actually, let's just use the index mapping if we can, or just loop labels.
            const labels = ['3D Cube', 'Scatter', 'Bar', 'Radar'];
            const idx = labels.indexOf(mode.charAt(0).toUpperCase() + mode.slice(1));
            if (idx >= 0) {
                const buttons = document.querySelectorAll('.chart-options button');
                if (buttons[idx]) buttons[idx].classList.add('active');
            }

            currentChartMode = mode;

            // Hide all views first
            document.getElementById('d3-chart').style.display = 'none';
            // Show chart controls by default
            document.querySelector('.controls').style.display = 'flex';
            // Restore table if it was hidden
            document.getElementById('benchmark-table').style.display = '';

            // Restore header and options by default
            const container = document.getElementById('chart-container');
            const header = container.querySelector('h2');
            const options = document.getElementById('chart-options');
            if (header) header.style.display = '';
            if (options) options.style.display = '';

            // Standard D3 Modes
            document.getElementById('d3-chart').style.display = 'block';

            // Draw standard charts
            if (mode === '3d') init3DChart();
            else if (mode === 'scatter') drawScatterPlot();
            else if (mode === 'bar') drawBarChart();
            else if (mode === 'radar') drawRadarChart();
        };
        function startScreensaver(mode) {
            console.log("startScreensaver called with mode:", mode);
            // Allow switching modes?
            if (active) {
                if (currentMode !== mode) {
                    stopScreensaver();
                    // Slight delay to allow cleanup?
                    setTimeout(() => startScreensaver(mode), 100);
                    return;
                }
                return;
            }
            active = true;
            ignoreInput = true;
            setTimeout(() => { ignoreInput = false; }, 1500); // Grace period

            currentMode = mode || 'red'; // Default to red

            const canvas = document.getElementById('matrix-canvas');
            const content = document.getElementById('main-content');
            const chartContainer = document.getElementById('d3-chart-container');

            if (currentMode === 'red') {
                // Full Screen Mode

                // Save scroll position BEFORE any DOM changes to prevent bounce
                savedScrollX = window.scrollX;
                savedScrollY = window.scrollY;

                // Scroll to top BEFORE adding fullscreen-active class
                // This prevents the "bounce" when overflow:hidden is applied
                window.scrollTo({ top: 0, left: 0, behavior: 'instant' });

                // Add fullscreen class to body to hide scrollbars (now safe - already at top)
                document.body.classList.add('fullscreen-active');

                // Now move canvas to body
                document.body.appendChild(canvas);
                canvas.style.position = 'fixed';
                canvas.style.top = '0';
                canvas.style.left = '0';
                canvas.style.width = '100vw';
                canvas.style.height = '100vh';
                canvas.style.zIndex = '1000';

                // Show fullscreen header
                const fsHeader = document.getElementById('fullscreen-header');
                if (fsHeader) {
                    fsHeader.style.display = 'block';
                    fsHeader.style.opacity = '1'; // Ensure visibility
                    if (window.startRiddleAnimation) window.startRiddleAnimation();
                    if (window.startTimerAnimation) window.startTimerAnimation();
                }

                // Initial State for Animation
                canvas.classList.add('matrix-slide-enter');
                content.classList.add('content-slide-exit');

                // Force Reflow
                void canvas.offsetWidth;

                // Start Animation
                requestAnimationFrame(() => {
                    canvas.classList.add('matrix-slide-active');

                    // Delay content slide until Matrix is halfway down (0.75s of 1.5s total)
                    setTimeout(() => {
                        content.classList.add('content-slide-active');
                    }, 750);

                    // Set flag earlier so numbers appear sooner (0.5s instead of 1.5s)
                    setTimeout(() => {
                        slideInComplete = true;
                        console.log('slideInComplete set to true');
                    }, 500);

                    // Trigger Browser Fullscreen AFTER animation starts
                    // This prevents the browser's fullscreen transition from interfering
                    setTimeout(() => {
                        if (document.documentElement.requestFullscreen) {
                            document.documentElement.requestFullscreen().catch(e => console.log(e));
                        }
                    }, 100);
                });

            } else {
                // Chart Mode (Blue Pill)
                chartContainer.appendChild(canvas); // Move to chart container
                canvas.style.position = 'absolute';
                canvas.style.top = '0';
                canvas.style.left = '0';
                canvas.style.width = '100%';
                canvas.style.height = '100%';
                canvas.style.zIndex = '10';

                // Add slide-down animation class
                canvas.classList.add('slide-down-slow');

                // For blue mode, we also need to trigger the dancing numbers after animation
                setTimeout(() => {
                    slideInComplete = true;
                }, 1000); // 1s delay (animation is 3s but we can start earlier)
            }

            canvas.style.display = 'block';

            resize();
            window.addEventListener('resize', resize);

            // Reset puzzle state
            puzzleLines = [];
            currentPuzzleIndex = 0;
            prepareNextPuzzle();

            // Start stats update interval for rotating displays
            if (window.statsUpdateInterval) {
                clearInterval(window.statsUpdateInterval);
            }
            window.statsUpdateInterval = setInterval(() => {
                if (active) {
                    window.updateSolverStats();
                }
            }, 1000); // Update every second for smooth rotation

            draw();
        }

        function stopScreensaver() {
            // Clear stats update interval
            if (window.statsUpdateInterval) {
                clearInterval(window.statsUpdateInterval);
                window.statsUpdateInterval = null;
            }

            if (!active) return;
            active = false;
            slideInComplete = false; // Reset flag
            const canvas = document.getElementById('matrix-canvas');
            const content = document.getElementById('main-content');

            canvas.style.display = 'none';

            // Remove fullscreen class from body
            document.body.classList.remove('fullscreen-active');

            // Hide fullscreen header
            const fsHeader = document.getElementById('fullscreen-header');
            if (fsHeader) fsHeader.style.display = 'none';
            if (window.stopTimerAnimation) window.stopTimerAnimation();

            // Remove animation classes
            content.classList.remove('content-slide-active');

            // Remove matrix-slide-active and slide-down-slow if present
            canvas.classList.remove('matrix-slide-active');
            canvas.classList.remove('slide-down-slow');
            canvas.classList.remove('matrix-slide-enter');

            // Reset canvas styles
            canvas.style.position = '';
            canvas.style.top = '';
            canvas.style.left = '';
            canvas.style.width = '';
            canvas.style.height = '';
            canvas.style.zIndex = '';
            canvas.style.display = 'none';

            // Reset body class
            document.body.classList.remove('fullscreen-active');

            // Exit Browser Fullscreen if active
            if (document.fullscreenElement) {
                document.exitFullscreen().catch(e => console.log(e));
            }

            cancelAnimationFrame(animationId);
            window.removeEventListener('resize', resize);

            canvas.className = '';
            content.className = 'content-wrapper';

            // Reset to body for safety
            document.body.appendChild(canvas);

            // Restore saved scroll position AFTER all cleanup is done
            // Use requestAnimationFrame to ensure DOM has settled
            requestAnimationFrame(() => {
                window.scrollTo({ top: savedScrollY, left: savedScrollX, behavior: 'instant' });
            });
        }

        // Idle Timer removed per user request

        function resetTimer(e) {
            if (active && !ignoreInput) {
                // Blue Pill Mode (Chart): Exit on ANY interaction
                if (currentMode !== 'red') {
                    stopScreensaver();
                    return;
                }

                // Red Pill Mode (Full Screen): Specific keys only
                if (e && e.type === 'keydown') {
                    if (e.key === 'Shift') {
                        if (window.cyclePuzzleColor) window.cyclePuzzleColor();
                        return;
                    }
                    if (e.key === 'Alt') {
                        stopScreensaver();
                    }
                }
            }
        }

        // Events to handle interactions when screensaver is active
        window.onload = resetTimer;
        document.onmousemove = resetTimer;
        document.onkeydown = resetTimer;
        document.onclick = resetTimer;
        document.onscroll = resetTimer;

        // Handle resize
        window.addEventListener('resize', () => {
            if (active) resize();
        });

        // Expose globally


        console.log("Matrix Screensaver initialized successfully. startScreensaver is:", typeof window.startScreensaver);


    } catch (e) {
        console.error("CRITICAL ERROR in Matrix Screensaver IIFE:", e);
        const errDiv = document.createElement('div');
        errDiv.style.position = 'fixed';
        errDiv.style.top = '0';
        errDiv.style.left = '0';
        errDiv.style.width = '100%';
        errDiv.style.background = 'red';
        errDiv.style.color = 'white';
        errDiv.style.zIndex = '9999';
        errDiv.style.padding = '10px';
        errDiv.innerText = 'SCREENSAVER ERROR: ' + e.toString();
        document.body.appendChild(errDiv);
    }
})();

// Ensure global access


window.showMismatch = function (solverName, matrixName, cell) {
    const solver = metricsData.find(s => s.solver === solverName);
    if (!solver) return;
    const result = solver.results.find(r => normalizeMatrix(r.matrix) === normalizeMatrix(matrixName));
    if (!result) return;

    document.getElementById('mismatchTitle').innerText = 'Mismatch: ' + solverName + ' on ' + matrixName;

    // Actual Output
    let actual = result.output || 'No output captured.';

    document.getElementById('actualOutput').innerText = actual;

    // Expected Output
    const expected = referenceOutputs[matrixName] || 'No reference output found.';
    document.getElementById('expectedOutput').innerText = expected;

    // Rerun Command
    currentRerunCommand = '# Run from SudokuSolver root\\nfind . -name runMe_ai.sh | grep "/' + solverName + '/" | head -n 1 | xargs -I { } sh -c \'cd $(dirname {}) && ./runMe_ai.sh ../../Matrices/' + matrixName + '\'';

    const modal = document.getElementById('mismatchModal');
    const content = modal.querySelector('.modal-content');
    modal.classList.add('visible');
    document.body.classList.add('modal-open');

    // Default center positioning
    content.style.position = '';
    content.style.top = '';
    content.style.left = '';
    content.style.margin = 'auto';
    content.style.transform = '';
    content.style.width = '90%';
    content.style.maxWidth = '900px';
}

window.closeMismatchModal = function () {
    document.getElementById('mismatchModal').classList.remove('visible');
    document.body.classList.remove('modal-open');
}

window.copyRerunCommand = function () {
    const btn = document.getElementById('copyRerunBtn');
    navigator.clipboard.writeText(currentRerunCommand).then(() => {
        const originalText = btn.innerText;
        btn.innerText = 'Copied!';
        setTimeout(() => {
            btn.innerText = originalText;
        }, 2000);
    });
}




// Modal Dragging Logic
function makeElementDraggable(elmnt, header) {
    if (!elmnt || !header) return;

    let pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;

    header.style.cursor = 'move';
    header.onmousedown = dragMouseDown;

    function dragMouseDown(e) {
        if (e.target.classList.contains('modal-close')) return;
        e.preventDefault();
        // get the mouse cursor position at startup:
        pos3 = e.clientX;
        pos4 = e.clientY;

        // If element is not absolute/fixed yet, make it so (keeping current visual position)
        const computedStyle = window.getComputedStyle(elmnt);
        if (computedStyle.position !== 'absolute') {
            const rect = elmnt.getBoundingClientRect();
            // We need to calculate position relative to the viewport (since overlay is fixed full screen)
            // rect.left is relative to viewport.

            elmnt.style.position = 'absolute';
            elmnt.style.left = rect.left + 'px';
            elmnt.style.top = rect.top + 'px';
            elmnt.style.margin = '0'; // Remove centering margins if any
            elmnt.style.transform = 'none'; // Remove centering transforms
            // Adjust width to fixed pixel value to prevent resizing when switching position
            elmnt.style.width = rect.width + 'px';
        }

        document.onmouseup = closeDragElement;
        // call a function whenever the cursor moves:
        document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
        e.preventDefault();
        // calculate the new cursor position:
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        // set the element's new position:
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
    }

    function closeDragElement() {
        // stop moving when mouse button is released:
        document.onmouseup = null;
        document.onmousemove = null;
    }
}

// Initialize dragging for known modals
function enableModalDragging() {
    const modals = ['methodModal', 'goalsModal', 'whyModal'];
    modals.forEach(id => {
        const modal = document.getElementById(id);
        if (modal) {
            const content = modal.querySelector('.modal-content');
            const title = modal.querySelector('.modal-title');
            if (content && title) {
                makeElementDraggable(content, title);
            }
        }
    });
}

// Call initially and on load
enableModalDragging();
window.addEventListener('load', enableModalDragging);

// ============================================================
// Score Modal Functions
// ============================================================

// Tier color mapping
const tierColors = {
    'S': '#ffd700', // Gold
    'A': '#00ff9d', // Neon Green
    'B': '#7aa2f7', // Blue
    'C': '#ff9e64', // Orange
    'D': '#f7768e', // Pink/Red
    'F': '#db4b4b'  // Dark Red
};

// Close Score Modal
window.closeScoreModal = function(event) {
    if (event) event.stopPropagation();
    const modal = document.getElementById('scoreModal');
    if (modal) {
        modal.style.display = 'none';
        document.body.classList.remove('modal-open');
    }
};

// Open Score Modal for a language
window.openScoreModal = function(lang) {
    const modal = document.getElementById('scoreModal');
    if (!modal) return;

    // Find the row data
    const row = document.querySelector(`tr[data-lang="${lang}"]`);
    if (!row) return;

    // Track current language for variant selection (US-007)
    currentScoreModalLang = lang;

    // Populate variant selector asynchronously
    populateVariantSelector(lang);

    // Get language data from metricsData
    // Get current algorithm filter
    const selectedAlgo = window.currentAlgorithm || 'BruteForce';

    // Find metrics for this language with matching algorithm type
    const langMetrics = metricsData.find(m => {
        const matchesName = m.solver === lang;
        const algoType = m.algorithmType || 'BruteForce';

        // In "all" mode, find the algorithm type from the clicked row
        if (selectedAlgo === 'all') {
            const clickedRow = document.querySelector(`tr[data-lang="${lang}"]`);
            const rowAlgo = clickedRow?.getAttribute('data-algorithm-type') || 'BruteForce';
            return matchesName && algoType === rowAlgo;
        }

        return matchesName && algoType === selectedAlgo;
    });

    // Find C baseline matching the same algorithm type
    const langAlgoType = langMetrics?.algorithmType || 'BruteForce';
    const cMetrics = metricsData.find(m => {
        return m.solver === 'C' && (m.algorithmType || 'BruteForce') === langAlgoType;
    });

    // Get score and tier from row
    const score = parseFloat(row.getAttribute('data-score')) || 0;
    const tier = row.getAttribute('data-tier') || 'F';
    const tierColor = tierColors[tier] || tierColors['F'];

    // Get breakdown data
    const breakdown = row.getAttribute('data-score-breakdown') || '';
    const breakdownParts = breakdown.split(' | ').map(p => {
        const match = p.match(/(\w+):\s*([\d.]+)x/);
        return match ? { label: match[1], value: parseFloat(match[2]) } : null;
    }).filter(Boolean);

    // Populate header
    const img = document.getElementById('scoreModalImg');
    const title = document.getElementById('scoreModalTitle');
    const subtitle = document.getElementById('scoreModalSubtitle');
    const tierBadge = document.getElementById('scoreModalTierBadge');

    if (langMetrics && langMetrics.logo) {
        img.src = langMetrics.logo;
        img.style.display = 'block';
    } else {
        img.style.display = 'none';
    }

    title.textContent = lang;
    const algoLabel = {
        'BruteForce': 'Brute Force',
        'DLX': 'Dancing Links',
        'CP': 'Constraint Propagation'
    }[langAlgoType] || 'Brute Force';
    subtitle.textContent = `Performance Score Analysis - ${algoLabel}`;

    tierBadge.textContent = tier;
    tierBadge.className = 'tier-badge tier-' + tier.toLowerCase();
    tierBadge.style.background = tierColor;
    tierBadge.style.color = tier === 'S' ? '#000' : '#fff';

    // Populate composite score
    const scoreValue = document.getElementById('scoreModalValue');
    scoreValue.textContent = score.toFixed(2) + 'x';
    scoreValue.style.color = score <= 1 ? '#00ff9d' : (score <= 2 ? '#ff9e64' : '#f7768e');

    // Populate breakdown ratios (3 factors: Time, Mem, CPU - NO iterations)
    const timeRatio = breakdownParts.find(p => p.label === 'Time');
    const memRatio = breakdownParts.find(p => p.label === 'Mem');
    const cpuRatio = breakdownParts.find(p => p.label === 'CPU');

    document.getElementById('scoreTimeRatio').textContent = timeRatio ? timeRatio.value.toFixed(2) + 'x' : '-';
    document.getElementById('scoreMemRatio').textContent = memRatio ? memRatio.value.toFixed(2) + 'x' : '-';
    document.getElementById('scoreCpuRatio').textContent = cpuRatio ? cpuRatio.value.toFixed(2) + 'x' : '-';

    // Draw radar chart with tier color
    drawScoreRadarChart(lang, tier, tierColor, breakdownParts, langAlgoType);

    // Update legend with tier color
    const legendSwatch = document.getElementById('legendLangSwatch');
    const legendName = document.getElementById('legendLangName');
    if (legendSwatch) legendSwatch.style.background = tierColor;
    if (legendName) legendName.textContent = lang;

    // Populate matrix results table
    populateMatrixResults(lang, langMetrics, cMetrics);

    // Show modal
    modal.style.display = 'flex';
    document.body.classList.add('modal-open');
};

// Draw radar chart in Score Modal
function drawScoreRadarChart(lang, tier, tierColor, breakdownParts, algorithmType = 'BruteForce') {
    const canvas = document.getElementById('scoreRadarChart');
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    const width = canvas.width;
    const height = canvas.height;
    const centerX = width / 2;
    const centerY = height / 2;
    const radius = Math.min(centerX, centerY) - 40;

    // Clear canvas
    ctx.clearRect(0, 0, width, height);

    // Labels for the 3 axes (NO iterations - iterations are validation, not performance)
    const labels = ['Speed', 'Memory', 'CPU'];
    const values = [
        breakdownParts.find(p => p.label === 'Time')?.value || 1,
        breakdownParts.find(p => p.label === 'Mem')?.value || 1,
        breakdownParts.find(p => p.label === 'CPU')?.value || 1
    ];

    const numAxes = labels.length;
    const angleSlice = (Math.PI * 2) / numAxes;

    // Calculate max scale (at least 3, or round up to nearest integer)
    const maxValue = Math.max(3, Math.ceil(Math.max(...values)));

    // Draw grid circles
    ctx.strokeStyle = '#414868';
    ctx.lineWidth = 1;
    for (let i = 1; i <= 3; i++) {
        const r = (radius / maxValue) * i;
        ctx.beginPath();
        ctx.arc(centerX, centerY, r, 0, Math.PI * 2);
        ctx.stroke();
    }

    // Draw 1.0 baseline circle (C baseline)
    ctx.strokeStyle = 'rgba(122, 162, 247, 0.5)';
    ctx.lineWidth = 2;
    ctx.setLineDash([5, 5]);
    const baselineR = radius / maxValue;
    ctx.beginPath();
    ctx.arc(centerX, centerY, baselineR, 0, Math.PI * 2);
    ctx.stroke();
    ctx.setLineDash([]);

    // Draw axes
    ctx.strokeStyle = '#414868';
    ctx.lineWidth = 1;
    for (let i = 0; i < numAxes; i++) {
        const angle = angleSlice * i - Math.PI / 2;
        const x = centerX + radius * Math.cos(angle);
        const y = centerY + radius * Math.sin(angle);
        ctx.beginPath();
        ctx.moveTo(centerX, centerY);
        ctx.lineTo(x, y);
        ctx.stroke();
    }

    // Draw labels
    ctx.fillStyle = '#c0caf5';
    ctx.font = '12px JetBrains Mono, monospace';
    ctx.textAlign = 'center';
    for (let i = 0; i < numAxes; i++) {
        const angle = angleSlice * i - Math.PI / 2;
        const x = centerX + (radius + 25) * Math.cos(angle);
        const y = centerY + (radius + 25) * Math.sin(angle);
        ctx.fillText(labels[i], x, y + 4);
    }

    // Draw C baseline polygon (all at 1.0)
    ctx.fillStyle = 'rgba(122, 162, 247, 0.2)';
    ctx.strokeStyle = 'rgba(122, 162, 247, 0.5)';
    ctx.lineWidth = 2;
    ctx.beginPath();
    for (let i = 0; i < numAxes; i++) {
        const angle = angleSlice * i - Math.PI / 2;
        const r = baselineR;
        const x = centerX + r * Math.cos(angle);
        const y = centerY + r * Math.sin(angle);
        if (i === 0) ctx.moveTo(x, y);
        else ctx.lineTo(x, y);
    }
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    // Draw language polygon with tier color
    const tierColorRgba = hexToRgba(tierColor, 0.3);
    ctx.fillStyle = tierColorRgba;
    ctx.strokeStyle = tierColor;
    ctx.lineWidth = 2;
    ctx.beginPath();
    for (let i = 0; i < numAxes; i++) {
        const angle = angleSlice * i - Math.PI / 2;
        const val = Math.min(values[i], maxValue); // Cap at maxValue
        const r = (radius / maxValue) * val;
        const x = centerX + r * Math.cos(angle);
        const y = centerY + r * Math.sin(angle);
        if (i === 0) ctx.moveTo(x, y);
        else ctx.lineTo(x, y);
    }
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    // Draw data points
    ctx.fillStyle = tierColor;
    for (let i = 0; i < numAxes; i++) {
        const angle = angleSlice * i - Math.PI / 2;
        const val = Math.min(values[i], maxValue);
        const r = (radius / maxValue) * val;
        const x = centerX + r * Math.cos(angle);
        const y = centerY + r * Math.sin(angle);
        ctx.beginPath();
        ctx.arc(x, y, 4, 0, Math.PI * 2);
        ctx.fill();
    }
}

// Helper: hex to rgba
function hexToRgba(hex, alpha) {
    const r = parseInt(hex.slice(1, 3), 16);
    const g = parseInt(hex.slice(3, 5), 16);
    const b = parseInt(hex.slice(5, 7), 16);
    return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

// Populate matrix results table
function populateMatrixResults(lang, langMetrics, cMetrics) {
    const tbody = document.getElementById('scoreMatrixBody');
    if (!tbody) return;

    tbody.innerHTML = '';

    const cResults = cMetrics?.results || [];
    const langResults = langMetrics?.results || [];

    // Get time unit
    const row = document.querySelector(`tr[data-lang="${lang}"]`);
    const isMs = row?.getAttribute('data-time-unit') === 'ms';

    for (let i = 1; i <= 5; i++) {
        const langResult = langResults.find(r => normalizeMatrix(r.matrix) === String(i));
        const cResult = cResults.find(r => normalizeMatrix(r.matrix) === String(i));

        const tr = document.createElement('tr');

        // Matrix number
        const tdMatrix = document.createElement('td');
        tdMatrix.textContent = i;
        tdMatrix.style.fontWeight = 'bold';
        tdMatrix.style.color = '#7aa2f7';
        tr.appendChild(tdMatrix);

        // Language time
        const tdTime = document.createElement('td');
        if (langResult && langResult.time) {
            const timeVal = parseFloat(langResult.time);
            tdTime.textContent = isMs ? timeVal.toFixed(2) + ' ms' : timeVal.toFixed(4) + ' s';
        } else {
            tdTime.textContent = '-';
            tdTime.style.color = '#565f89';
        }
        tr.appendChild(tdTime);

        // C time
        const tdCTime = document.createElement('td');
        if (cResult && cResult.time) {
            const cTimeVal = parseFloat(cResult.time);
            tdCTime.textContent = isMs ? cTimeVal.toFixed(2) + ' ms' : cTimeVal.toFixed(4) + ' s';
        } else {
            tdCTime.textContent = '-';
            tdCTime.style.color = '#565f89';
        }
        tr.appendChild(tdCTime);

        // Ratio
        const tdRatio = document.createElement('td');
        if (langResult && cResult && langResult.time && cResult.time) {
            const ratio = langResult.time / cResult.time;
            tdRatio.textContent = ratio.toFixed(2) + 'x';
            if (ratio < 1) {
                tdRatio.style.color = '#9ece6a'; // Faster - green
            } else if (ratio > 1.5) {
                tdRatio.style.color = '#f7768e'; // Much slower - red
            } else {
                tdRatio.style.color = '#ff9e64'; // Similar - orange
            }
        } else {
            tdRatio.textContent = '-';
            tdRatio.style.color = '#565f89';
        }
        tr.appendChild(tdRatio);

        // Iterations
        const tdIters = document.createElement('td');
        if (langResult && langResult.iterations) {
            tdIters.textContent = parseInt(langResult.iterations).toLocaleString();
        } else {
            tdIters.textContent = '-';
            tdIters.style.color = '#565f89';
        }
        tr.appendChild(tdIters);

        // Memory
        const tdMem = document.createElement('td');
        if (langResult && langResult.memory) {
            const memMb = langResult.memory / 1024 / 1024;
            tdMem.textContent = memMb.toFixed(1) + ' MB';
        } else {
            tdMem.textContent = '-';
            tdMem.style.color = '#565f89';
        }
        tr.appendChild(tdMem);

        tbody.appendChild(tr);
    }
}

// Track current language for variant selection
let currentScoreModalLang = null;

// Populate variant selector (US-007)
async function populateVariantSelector(lang) {
    const selector = document.getElementById('scoreVariantSelector');
    if (!selector) return;

    try {
        const res = await fetch(`/api/variants/${encodeURIComponent(lang)}`);
        if (!res.ok) {
            selector.style.display = 'none';
            return;
        }

        const variants = await res.json();

        // Hide selector if only one variant
        if (!variants || variants.length <= 1) {
            selector.style.display = 'none';
            return;
        }

        // Populate options
        selector.innerHTML = '';
        variants.forEach((v, idx) => {
            const option = document.createElement('option');
            option.value = v.variant;
            const dateStr = v.timestamp ? new Date(v.timestamp).toLocaleDateString() : '';
            option.textContent = `${v.variant}${dateStr ? ' (' + dateStr + ')' : ''}`;
            if (idx === 0) option.selected = true;
            selector.appendChild(option);
        });

        selector.style.display = 'block';
    } catch (e) {
        console.error('Error fetching variants:', e);
        selector.style.display = 'none';
    }
}

// Handle variant selection (US-007)
window.onVariantSelect = async function(variant) {
    if (!currentScoreModalLang || !variant) return;

    try {
        const res = await fetch(`/api/metrics/${encodeURIComponent(currentScoreModalLang)}/${encodeURIComponent(variant)}`);
        if (!res.ok) {
            console.error('Failed to fetch variant metrics');
            return;
        }

        const variantMetrics = await res.json();

        // Get C baseline
        const cRes = await fetch('/api/metrics/baseline/c');
        const cMetrics = cRes.ok ? await cRes.json() : null;

        // Recalculate scores from the variant metrics
        const results = variantMetrics.results || [];
        const cResults = cMetrics?.results || [];

        // Calculate average ratios (only for matrices this language ran vs C's same matrices)
        let timeRatios = [], memRatios = [], cpuRatios = [];

        results.forEach(r => {
            const matrixNum = String(r.matrix).replace('.matrix', '');
            const cResult = cResults.find(cr => String(cr.matrix).replace('.matrix', '') === matrixNum);

            if (cResult) {
                if (r.time && cResult.time) timeRatios.push(r.time / cResult.time);
                if (r.memory && cResult.memory) memRatios.push(r.memory / cResult.memory);
                if (r.cpu_user !== undefined && cResult.cpu_user !== undefined) {
                    const langCpu = (r.cpu_user || 0) + (r.cpu_sys || 0);
                    const cCpu = (cResult.cpu_user || 0) + (cResult.cpu_sys || 0);
                    if (cCpu > 0) cpuRatios.push(langCpu / cCpu);
                }
            }
        });

        const avgTime = timeRatios.length > 0 ? timeRatios.reduce((a, b) => a + b, 0) / timeRatios.length : 1;
        const avgMem = memRatios.length > 0 ? memRatios.reduce((a, b) => a + b, 0) / memRatios.length : 1;
        const avgCpu = cpuRatios.length > 0 ? cpuRatios.reduce((a, b) => a + b, 0) / cpuRatios.length : 1;

        // Composite score: geometric mean of 3 ratios (time, mem, cpu - NO iterations)
        const score = Math.pow(Math.max(0.001, avgTime) * Math.max(0.001, avgMem) * Math.max(0.001, avgCpu), 1/3);

        // Determine tier (same thresholds as main report)
        let tier = 'F';
        if (score < 0.95) tier = 'S';
        else if (score < 1.05) tier = 'A';
        else if (score < 1.50) tier = 'B';
        else if (score < 3.00) tier = 'C';
        else if (score < 10.00) tier = 'D';

        const tierColor = tierColors[tier] || tierColors['F'];

        // Update UI
        const tierBadge = document.getElementById('scoreModalTierBadge');
        tierBadge.textContent = tier;
        tierBadge.className = 'tier-badge tier-' + tier.toLowerCase();
        tierBadge.style.background = tierColor;
        tierBadge.style.color = tier === 'S' ? '#000' : '#fff';

        const scoreValue = document.getElementById('scoreModalValue');
        scoreValue.textContent = score.toFixed(2) + 'x';
        scoreValue.style.color = score <= 1 ? '#00ff9d' : (score <= 2 ? '#ff9e64' : '#f7768e');

        document.getElementById('scoreTimeRatio').textContent = avgTime.toFixed(2) + 'x';
        document.getElementById('scoreMemRatio').textContent = avgMem.toFixed(2) + 'x';
        document.getElementById('scoreCpuRatio').textContent = avgCpu.toFixed(2) + 'x';

        // Redraw radar chart (3 axes: Time, Mem, CPU - NO iterations)
        const breakdownParts = [
            { label: 'Time', value: avgTime },
            { label: 'Mem', value: avgMem },
            { label: 'CPU', value: avgCpu }
        ];
        const langAlgoType = variantMetrics?.algorithmType || 'BruteForce';
        drawScoreRadarChart(currentScoreModalLang, tier, tierColor, breakdownParts, langAlgoType);

        // Update legend
        const legendSwatch = document.getElementById('legendLangSwatch');
        if (legendSwatch) legendSwatch.style.background = tierColor;

        // Repopulate matrix results
        populateMatrixResults(currentScoreModalLang, variantMetrics, cMetrics);

    } catch (e) {
        console.error('Error loading variant:', e);
    }
};

// Add click handler for score cells
document.addEventListener('DOMContentLoaded', function() {
    document.querySelectorAll('.score-col').forEach(cell => {
        cell.addEventListener('click', (e) => {
            e.preventDefault();
            e.stopPropagation();

            const row = cell.closest('tr');
            const lang = row?.getAttribute('data-lang');
            if (lang) {
                window.openScoreModal(lang);
            }
        });
        cell.style.cursor = 'pointer';
    });
});

