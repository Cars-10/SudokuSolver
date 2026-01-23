
// Table filtering, sorting, and display operations
import { state } from './globals.js';
import { updateRelativeTimes, normalizeMatrix } from './utils.js';

// Search/filter languages in table
export function filterLanguages() {
    const input = document.getElementById('search-input');
    const filter = input ? input.value.toUpperCase() : '';
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

// Calculate mismatch count for a given algorithm
export function calculateMismatchCount(algorithmType) {
    if (typeof window.metricsData === 'undefined') return 0;

    const baseline = algorithmType === 'all' ? null : window.cBaselines[algorithmType];
    let count = 0;

    window.metricsData.forEach(m => {
        if (m.solver === 'C') return;
        const mAlgo = m.algorithmType || 'BruteForce';

        // Skip if filtering by algo and this isn't that algo
        if (algorithmType !== 'all' && mAlgo !== algorithmType) return;

        // Get the right baseline for this metric's algorithm
        const useBaseline = algorithmType === 'all' ? window.cBaselines[mAlgo] : baseline;
        if (!useBaseline) return;

        // Check for mismatches only on matrices that have run data
        let hasMismatch = false;
        m.results.forEach(r => {
            if (r.status !== 'success') return;
            const matrix = String(r.matrix).replace('.matrix', '');
            const expected = useBaseline[matrix];
            if (expected !== undefined && r.iterations !== expected) {
                hasMismatch = true;
            }
        });
        if (hasMismatch) count++;
    });

    return count;
}

// Update mismatch display
export function updateMismatchDisplay(algorithmType) {
    const count = calculateMismatchCount(algorithmType);
    const mismatchEl = document.querySelector('.mismatch-counter');
    if (mismatchEl) {
        mismatchEl.textContent = 'MISMATCHES: ' + count;
        mismatchEl.style.color = count > 0 ? '#ff0055' : '#565f89';
    }
}

// Algorithm filtering function
export function filterByAlgorithm(algorithmType) {
    console.log('[TableOps] filterByAlgorithm:', algorithmType);
    state.currentAlgorithm = algorithmType;
    window.currentAlgorithm = algorithmType; // Sync legacy global

    // Persist selection to localStorage
    try {
        localStorage.setItem('sudoku-benchmark-algorithm', algorithmType);
    } catch (e) { /* localStorage unavailable */ }

    const rows = document.querySelectorAll('#mainTableBody tr');

    rows.forEach(row => {
        const rowAlgo = row.getAttribute('data-algorithm-type');
        // Only toggle display if it's not a mismatch row that should be hidden by mismatch filter
        const isMismatchHidden = document.getElementById('toggleMismatchesBtn')?.classList.contains('active') && row.classList.contains('mismatch-iterations');

        if (algorithmType === 'all' || rowAlgo === algorithmType) {
            if (!isMismatchHidden) row.style.display = '';
        } else {
            row.style.display = 'none';
        }
    });

    // Update button label
    const btn = document.getElementById('algorithmSelectorBtn');
    const labels = {
        'all': 'ALL ALGORITHMS',
        'BruteForce': 'BRUTE FORCE',
        'DLX': 'DANCING LINKS',
        'CP': 'CONSTRAINT PROPAGATION'
    };
    if (btn) {
        btn.textContent = (labels[algorithmType] || 'ALGORITHM') + ' ▾';
    }

    // Update active state in dropdown
    const dropdownLinks = document.querySelectorAll('#algorithmSelectorBtn + .dropdown-content a');
    dropdownLinks.forEach(link => {
        link.classList.remove('active');
        // Find the link that matches this algorithm
        const onclick = link.getAttribute('onclick') || '';
        if (onclick.includes("'" + algorithmType + "'")) {
            link.classList.add('active');
        }
    });

    // Update mismatch count for selected algorithm
    if (window.cBaselines) updateMismatchDisplay(algorithmType);

    // Update language count to show unique languages (not rows)
    const visibleRows = document.querySelectorAll('#mainTableBody tr:not([style*="display: none"])');
    const uniqueLangs = new Set();
    visibleRows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        if (lang) uniqueLangs.add(lang);
    });
    const uniqueCount = uniqueLangs.size;
    const langCountEl = document.getElementById('langSelectorBtn');
    if (langCountEl) {
        langCountEl.textContent = uniqueCount + ' LANGUAGES ▾';
    }
    const solverTextEl = document.getElementById('solver-text');
    if (solverTextEl) {
        solverTextEl.textContent = uniqueCount + ' EXPERIMENTAL SUBJECTS';
    }

    // Refresh current chart to reflect algorithm filter
    if (typeof window.currentChart !== 'undefined' && typeof window.switchChart === 'function') {
        window.switchChart(window.currentChart);
    }
}

// Toggle language selector dropdown (moved from inline)
export function toggleLanguageSelector() {
    const dropdown = document.getElementById('language-selector-dropdown');
    if (dropdown) {
        dropdown.style.display = dropdown.style.display === 'none' ? 'block' : 'none';
    }
}

// Open language details modal (moved from inline)
export function openLanguageModal(lang, event) {
    if (event) event.stopPropagation();

    // Find metrics for this lang by iterating window.metricsData
    // We need to match based on the row that was clicked, which might imply a specific algorithm variant
    // For now, simpler look up:
    const data = window.metricsData.find(m => m.solver === lang && (state.currentAlgorithm === 'all' || m.algorithmType === state.currentAlgorithm || 'BruteForce'));
    // Fallback if no exact match or 'all': just find first match for lang
    const metric = data || window.metricsData.find(m => m.solver === lang);

    if (!metric) {
        console.error("No metrics found for", lang);
        return;
    }

    // Populate Modal
    document.getElementById('modalTitle').innerText = lang;
    document.getElementById('modalDesc').innerText = window.languageMetadata[lang]?.description || "No description available.";

    // History
    const historyText = window.languageHistories?.[lang] || "No history available.";
    document.getElementById('modalHistory').innerText = historyText;

    // Compiler Info
    const compiler = window.languageMetadata[lang]?.compiler || "Unknown";
    document.getElementById('modalCompiler').innerText = compiler;

    // Show Modal
    const modal = document.getElementById('langModal');
    modal.classList.add('visible');
    document.body.classList.add('modal-open');
}

// Sort main table by column
export function sortRows(metric, btn) {
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
    if (state.currentSort.metric === metric) {
        state.currentSort.dir *= -1;
    } else {
        state.currentSort.metric = metric;
        // Natural direction: ascending for names, ascending (lowest first) for numeric values
        state.currentSort.dir = 1;
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
        if (state.currentSort.dir === -1) {
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
            return (aScore - bScore) * state.currentSort.dir;
        }

        if (metric === 'lang') {
            return aVal.localeCompare(bVal) * state.currentSort.dir;
        } else if (metric === 'year') {
            return (parseInt(aVal) - parseInt(bVal)) * state.currentSort.dir;
        } else {
            return (parseFloat(aVal) - parseFloat(bVal)) * state.currentSort.dir;
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

// Sort by matrix column
export function sortMatrix(index, metric, btn) {
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

    if (state.currentSort.metric === fullMetric) {
        state.currentSort.dir *= -1;
    } else {
        state.currentSort.metric = fullMetric;
        state.currentSort.dir = metric === 'time' || metric === 'score' ? 1 : -1;
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
        if (state.currentSort.dir === -1) {
            btn.classList.add('desc');  // CSS rotates the .sort-arrow inside via .desc class
        }
    }

    rowPairs.sort((a, b) => {
        const aVal = parseFloat(a.dataRow.getAttribute(attr));
        const bVal = parseFloat(b.dataRow.getAttribute(attr));
        return (aVal - bVal) * state.currentSort.dir;
    });

    // Re-append rows with their expanded-content siblings
    rowPairs.forEach(pair => {
        tbody.appendChild(pair.dataRow);
        if (pair.expandedRow) {
            tbody.appendChild(pair.expandedRow);
        }
    });
}

// Toggle mismatch visibility
export function toggleMismatches() {
    const btn = document.getElementById('toggleMismatchesBtn');
    const isHidingMismatches = btn.classList.toggle('active');

    const selector = document.getElementById('personality-selector');
    const persona = selector ? selector.value : 'Standard';
    const curseWord = window.mismatchLabels?.[persona] || window.mismatchLabels?.['Standard'] || "MISMATCHES";

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
