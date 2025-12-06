let currentSort = { metric: 'time', dir: 1 }; // 1 = Asc, -1 = Desc

// Search Logic
function filterLanguages() {
    const input = document.getElementById('search-input');
    const filter = input.value.toUpperCase();
    const tbody = document.querySelector('tbody');
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
    const tbody = document.querySelector('tbody');
    const rows = Array.from(tbody.querySelectorAll('tr'));

    // Toggle direction
    if (currentSort.metric === metric) {
        currentSort.dir *= -1;
    } else {
        currentSort.metric = metric;
        currentSort.dir = 1;
    }

    // Update buttons
    document.querySelectorAll('.btn, .sort-btn').forEach(b => {
        b.classList.remove('active');
        b.classList.remove('rotate-180');
    });

    if (btn) {
        btn.classList.add('active');
        if (currentSort.dir === -1) {
            btn.classList.add('rotate-180');
        }
    }

    rows.sort((a, b) => {
        const aVal = a.getAttribute('data-' + metric);
        const bVal = b.getAttribute('data-' + metric);

        if (metric === 'lang') {
            return aVal.localeCompare(bVal) * currentSort.dir;
        } else if (metric === 'year') {
            return (parseInt(aVal) - parseInt(bVal)) * currentSort.dir;
        } else {
            return (parseFloat(aVal) - parseFloat(bVal)) * currentSort.dir;
        }
    });

    rows.forEach(row => tbody.appendChild(row));
}

function sortMatrix(index, metric, btn) {
    const tbody = document.querySelector('tbody');
    const rows = Array.from(tbody.querySelectorAll('tr'));
    const attr = 'data-m' + index + '-' + metric;
    const fullMetric = 'm' + index + '_' + metric;

    if (currentSort.metric === fullMetric) {
        currentSort.dir *= -1;
    } else {
        currentSort.metric = fullMetric;
        currentSort.dir = metric === 'time' || metric === 'score' ? 1 : -1;
    }

    // Update buttons
    document.querySelectorAll('.btn, .sort-btn').forEach(b => {
        b.classList.remove('active');
        b.classList.remove('rotate-180');
    });

    if (btn) {
        btn.classList.add('active');
        if (currentSort.dir === -1) {
            btn.classList.add('rotate-180');
        }
    }

    rows.sort((a, b) => {
        const aVal = parseFloat(a.getAttribute(attr));
        const bVal = parseFloat(b.getAttribute(attr));
        return (aVal - bVal) * currentSort.dir;
    });

    rows.forEach(row => tbody.appendChild(row));
}

// Mismatch Filter
function toggleMismatches() {
    const btn = document.getElementById('toggleMismatchesBtn');
    const isFilterActive = btn.classList.toggle('active');

    if (isFilterActive) {
        btn.classList.add('filter-active-red');
        const selector = document.getElementById('personality-selector');
        const persona = selector ? selector.value : 'Standard';
        const curseWord = mismatchLabels[persona] || mismatchLabels['Standard'] || "MISMATCHES";
        btn.querySelector('span').textContent = curseWord;
    } else {
        btn.classList.remove('filter-active-red');
        const selector = document.getElementById('personality-selector');
        const persona = selector ? selector.value : 'Standard';
        const curseWord = mismatchLabels[persona] || mismatchLabels['Standard'] || "MISMATCHES";
        btn.querySelector('span').textContent = "Show " + curseWord;
    }

    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        if (isFilterActive) {
            if (row.classList.contains('mismatch-iterations')) {
                row.style.display = 'none';
            } else {
                row.style.display = '';
            }
        } else {
            row.style.display = '';
        }
    });
}

// Personality Selector
function changePersonality() {
    const selector = document.getElementById('personality-selector');
    const persona = selector.value;
    const intro = document.getElementById('personality-intro');

    // Update Intro Text
    intro.innerText = narratorIntros[persona] || narratorIntros['Standard'] || "Welcome to the Sudoku Benchmark.";

    // Update Tooltips
    const rows = document.querySelectorAll('tbody tr');
    rows.forEach(row => {
        const lang = row.getAttribute('data-lang');
        const quotes = personalities[persona] || personalities['Standard'];
        let quote = quotes[lang] || quotes['default'] || "Unknown.";

        // Append Efficiency
        const score = row.getAttribute('data-score');
        let time = parseFloat(row.getAttribute('data-time'));
        let timeStr = time < 0.0001 && time > 0 ? time.toExponential(2) + "s" : time.toFixed(6) + "s";
        quote += " Efficiency: " + parseFloat(score).toFixed(2) + " MB/s | Time: " + timeStr;

        row.setAttribute('data-quote', quote);
    });
    // Update Methodology Modal
    const methodDesc = document.querySelector('#methodModal .modal-desc');
    if (methodDesc) {
        methodDesc.innerHTML = methodologyTexts[persona] || methodologyTexts['Standard'];
    }

    // Update Mismatch Button if active
    const btn = document.getElementById('toggleMismatchesBtn');
    if (btn) {
        const curseWord = mismatchLabels[persona] || mismatchLabels['Standard'] || "MISMATCHES";
        if (btn.classList.contains('active')) {
            btn.querySelector('span').textContent = curseWord;
        } else {
            btn.querySelector('span').textContent = "Show " + curseWord; // Dynamic "Show Mismatches" text
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
// Attach to all cells
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
                content = '<strong style="color: var(--primary)">Matrix ' + (matrixIdx + 1) + '</strong><br>' +
                    '<span style="color: var(--secondary)">' + lang + '</span><br>' +
                    '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                    'Time: <span style="color: #fff">' + parseFloat(time).toFixed(5) + 's</span><br>' +
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
            tooltip.style.left = (e.clientX + 15) + 'px';
            tooltip.style.top = (e.clientY + 15) + 'px';
            tooltip.innerHTML = content;
        }
    });

    cell.addEventListener('mouseleave', () => {
        const tooltip = document.getElementById('tooltip');
        tooltip.style.display = 'none';
    });
});

// Add click handler to language cells to show modal
document.querySelectorAll('.lang-col').forEach(cell => {
    cell.addEventListener('click', (e) => {
        const row = cell.parentElement;
        const lang = row.getAttribute('data-lang');
        if (lang) {
            showLanguageDetails(lang, e.clientX, e.clientY);
        }
    });
    cell.style.cursor = 'pointer';  // Make it obvious it's clickable
});
// Modal Logic
let currentEditingLang = null;
let currentMetadata = null;

// Updated Show Function
window.showLanguageDetails = async function (lang, x, y) {
    console.log("Opening modal for:", lang);
    currentEditingLang = lang;
    const modal = document.getElementById('langModal');
    const modalContent = document.getElementById('modalContent');

    // Fetch dynamic metadata from backend first? 
    // We fallback to static if fetch fails.
    let meta = languageMetadata[lang] || {};

    try {
        const res = await fetch(`http://localhost:9101/api/metadata/${lang}`);
        if (res.ok) {
            const dynamicMeta = await res.json();
            // Merge: dynamic takes precedence, but keep static defaults if missing
            meta = { ...meta, ...dynamicMeta };
        }
    } catch (e) {
        console.warn("Could not fetch dynamic metadata:", e);
    }

    currentMetadata = meta;

    // View Mode Population
    const img = meta.image || meta.logo || "";
    document.getElementById('modalImg').src = img;

    const displayName = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
    document.getElementById('modalTitle').innerText = displayName;
    document.getElementById('modalSubtitle').innerText = (meta.creator || "?") + " • " + (meta.date || "????");
    document.getElementById('modalLocation').innerText = "📍 " + (meta.location || "Unknown Location");
    document.getElementById('modalBenefits').innerText = "✨ " + (meta.benefits || "Unknown Benefits");
    const grokepiaLink = document.getElementById('modalGrokepia');
    if (grokepiaLink) {
        const encodedQuery = encodeURIComponent(displayName + " Programming Language");
        grokepiaLink.href = `https://grokipedia.com/search?q=${encodedQuery}`;
        grokepiaLink.innerText = "🔗 View on Grokipedia";
        grokepiaLink.style.display = "inline-block";
    }
    document.getElementById('modalDesc').innerText = meta.description || "No description available.";

    // Edit Mode Population
    document.getElementById('editInputs-title').value = displayName;
    document.getElementById('editInputs-creator').value = meta.creator || "";
    document.getElementById('editInputs-image').value = meta.image || meta.logo || "";
    document.getElementById('editInputs-date').value = meta.date || "";
    document.getElementById('editInputs-location').value = meta.location || "";
    document.getElementById('editInputs-benefits').value = meta.benefits || "";
    document.getElementById('editInputs-desc').value = meta.description || "";

    // Authors Population
    const authorList = document.getElementById('authorList');
    authorList.innerHTML = '';

    const authors = meta.authors || [];
    // If no specific authors list, use creator + main image as one entry? or just rely on main header?
    // Let's rely on main header for primary, but allow adding more.

    authors.forEach((auth, idx) => {
        const div = document.createElement('div');
        div.className = 'author-item';
        div.innerHTML = `
                        <img src="${auth.image || ''}" class="author-img">
                        <span class="view-only">${auth.name}</span>
                        <input type="text" class="modal-edit-input edit-only" value="${auth.name}" placeholder="Name" onchange="updateAuthor(${idx}, 'name', this.value)">
                        <input type="text" class="modal-edit-input edit-only" value="${auth.image}" placeholder="Image URL" onchange="updateAuthor(${idx}, 'image', this.value)">
                        <button class="btn edit-only" style="background:#ff5555; padding: 2px 5px; font-size: 0.7em;" onclick="removeAuthor(${idx})"> Remove </button>
                    `;
        authorList.appendChild(div);
    });

    modalContent.classList.remove('editing');
    document.getElementById('editBtn').innerText = "Edit";
    modal.style.display = 'flex';

    // Positioning - Constrain to Table
    if (x !== undefined && y !== undefined) {
        const rect = modalContent.getBoundingClientRect();
        const table = document.querySelector('table');

        if (table) {
            const tableRect = table.getBoundingClientRect();
            // We need document relative coordinates because absolute positioning is usually document relative 
            // unless a parent has relative positioning. The modal container is 'display:flex' overlay usually.
            // But if we are positioning absolute, let's assume body relative.

            const scrollX = window.scrollX || window.pageXOffset;
            const scrollY = window.scrollY || window.pageYOffset;

            // Boundaries in document coords
            const tableTop = tableRect.top + scrollY;
            const tableLeft = tableRect.left + scrollX;
            const tableRight = tableRect.right + scrollX;
            const tableBottom = tableRect.bottom + scrollY;

            let desiredTop = y + scrollY;
            let desiredLeft = x + scrollX;

            // Ensure we don't go above the table (into the chart)
            if (desiredTop < tableTop) desiredTop = tableTop;

            // Ensure we calculate with modal height/width
            // Note: rect.height might be 0 if display none, but we just set display flex above.
            // Force layout recalc?
            const modalHeight = modalContent.offsetHeight || 600;
            const modalWidth = modalContent.offsetWidth || 500;

            // Clamp Right
            if (desiredLeft + modalWidth > tableRight) {
                desiredLeft = tableRight - modalWidth;
            }
            // Clamp Left
            if (desiredLeft < tableLeft) {
                desiredLeft = tableLeft;
            }

            // Clamp Bottom
            if (desiredTop + modalHeight > tableBottom) {
                desiredTop = tableBottom - modalHeight;
            }

            modalContent.style.margin = '0';
            modalContent.style.position = 'absolute';
            modalContent.style.top = desiredTop + 'px';
            modalContent.style.left = desiredLeft + 'px';

            // Also set alignment on parent to avoid centering fights
            modal.style.justifyContent = 'flex-start';
            modal.style.alignItems = 'flex-start';

        } else {
            // Fallback
            modal.style.justifyContent = 'center';
            modal.style.alignItems = 'center';
            modalContent.style.position = 'relative';
            modalContent.style.top = 'auto';
            modalContent.style.left = 'auto';
        }

    } else {
        // Center it
        modal.style.justifyContent = 'center';
        modal.style.alignItems = 'center';
        modalContent.style.position = 'relative';
        modalContent.style.top = 'auto';
        modalContent.style.left = 'auto';
    }
};

window.toggleEditMode = function () {
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

window.toggleLock = async function () {
    if (!currentEditingLang) return;

    const btn = document.getElementById('lockBtn');
    const isCurrentlyLocked = btn.innerText.includes('Locked 🔒');
    const newLockState = !isCurrentlyLocked;

    try {
        const res = await fetch('http://localhost:9101/api/lock', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ lang: currentEditingLang, locked: newLockState })
        });

        if (res.ok) {
            btn.innerText = newLockState ? '🔒 Locked' : '🔓 Unlocked';
            btn.style.background = newLockState ? '#4caf50' : '';
            btn.style.color = newLockState ? '#fff' : '';
        } else {
            alert("Error locking: " + res.statusText);
        }
    } catch (e) {
        alert("Error locking: " + e.message + ". Is ContentServer running?");
    }
};

window.saveLanguageDetails = async function () {
    if (!currentEditingLang) return;

    const newData = {
        creator: document.getElementById('editInputs-creator').value,
        image: document.getElementById('editInputs-image').value,
        date: document.getElementById('editInputs-date').value,
        location: document.getElementById('editInputs-location').value,
        benefits: document.getElementById('editInputs-benefits').value,
        description: document.getElementById('editInputs-desc').value,
        authors: currentMetadata.authors || []
    };

    // Save to backend
    try {
        const res = await fetch('http://localhost:9101/api/save-metadata', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ lang: currentEditingLang, metadata: newData })
        });

        if (res.ok) {
            alert("Saved!");
            window.showLanguageDetails(currentEditingLang); // Refresh
        } else {
            alert("Error saving: " + res.statusText);
        }
    } catch (e) {
        alert("Error saving: " + e.message);
    }
};

window.addAuthorField = function () {
    if (!currentMetadata.authors) currentMetadata.authors = [];
    currentMetadata.authors.push({ name: "New Author", image: "" });
    window.showLanguageDetails(currentEditingLang); // Re-render triggers populate
    document.getElementById('modalContent').classList.add('editing'); // Keep edit mode
};

window.removeAuthor = function (idx) {
    if (!currentMetadata.authors) return;
    currentMetadata.authors.splice(idx, 1);
    window.showLanguageDetails(currentEditingLang);
    document.getElementById('modalContent').classList.add('editing');
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

// Paste Listener for Modal
document.addEventListener('paste', async (e) => {
    // Only if modal is open and editing
    const modal = document.getElementById('langModal');
    const modalContent = document.getElementById('modalContent');
    if (modal.style.display !== 'flex' || !modalContent.classList.contains('editing')) return;

    const items = (e.clipboardData || e.originalEvent.clipboardData).items;
    for (let index in items) {
        const item = items[index];
        if (item.kind === 'file') {
            const blob = item.getAsFile();
            const reader = new FileReader();
            reader.onload = function (event) {
                const base64 = event.target.result;
                document.getElementById('modalImg').src = base64;
                document.getElementById('editInputs-image').value = "[Base64 Image Data]";
                // We assume user understands this is temporary unless we have a backend
                // But for the visual feedback request, this works.
                if (currentMetadata) currentMetadata.image = base64;
            };
            reader.readAsDataURL(blob);
        }
    }
});

// Upload Input
window.uploadLogo = async function (input) {
    if (input.files && input.files[0]) {
        const formData = new FormData();
        formData.append('file', input.files[0]);
        formData.append('lang', currentEditingLang);

        try {
            const res = await fetch('http://localhost:9101/api/upload-media', {
                method: 'POST',
                body: formData
            });
            if (res.ok) {
                const data = await res.json();
                const relativePath = `CleanedUp/Languages/${currentEditingLang}/Media/${data.filename}`;
                currentMetadata.image = relativePath;
                document.getElementById('modalImg').src = relativePath;
            }
        } catch (e) {
            alert("Upload failed");
        }
    }
};


function closeModal(event) {
    if (event.target.id === 'langModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('langModal').style.display = 'none';
    }
}

function showMethodology() {
    document.getElementById('methodModal').style.display = 'flex';
}

function closeMethodology(event) {
    if (event.target.id === 'methodModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('methodModal').style.display = 'none';
    }
}

function showGoals() {
    document.getElementById('goalsModal').style.display = 'flex';
}

function closeGoals(event) {
    if (event.target.id === 'goalsModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('goalsModal').style.display = 'none';
    }
}

function showWhy() {
    document.getElementById('whyModal').style.display = 'flex';
}

function closeWhy(event) {
    if (event.target.id === 'whyModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('whyModal').style.display = 'none';
    }
}

// Initialize
toggleMismatches(); // Default hide

// --- D3.js Chart Implementation ---
(function () {
    // Inject metrics with logo data
    // Data is now injected globally by HTMLGenerator before this script runs.
    // const historyData = ...
    // const referenceOutputs = ...
    // const tailoring = ...
    // const metricsData = ...

    let data = metricsData;
    const allTimes = data.flatMap(d => d.results.map(r => r ? r.time : 999999)).filter(t => t < 999999);
    const minTime = allTimes.length ? Math.min(...allTimes) : 0.001;
    const maxTime = allTimes.length ? Math.max(...allTimes) : 100;

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
        document.getElementById('logModal').style.display = 'none';
    };

    window.runSolver = async function (lang, matrix, event) {
        if (event) event.stopPropagation(); // Prevent row click or tooltip

        const outputDiv = document.getElementById('logOutput');
        const headerTitle = document.getElementById('logTitle');

        // Reset Modal
        outputDiv.innerText = "Running " + lang + " on " + matrix + "...\nPlease wait...";
        headerTitle.innerText = "Execution Log: " + lang + " (" + matrix + ")";
        document.getElementById('logModal').style.display = 'flex';

        try {
            // Use relative path - assumes server is running on the same origin
            const res = await fetch('/api/run', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ language: lang, matrix: matrix })
            });

            if (!res.ok) {
                const errorText = await res.text();
                outputDiv.innerText = "Error: " + res.status + "\n" + errorText;
                return;
            }

            const data = await res.json();
            if (data.success) {
                outputDiv.innerText = "--- STDOUT ---\n" + data.stdout + "\n\n--- STDERR ---\n" + data.stderr;
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
            container.selectAll("*").remove();

            if (type === 'line') {
                drawLineChart();
            } else if (type === 'jockey') {
                drawJockeyChart();
            } else if (type === 'race') {
                drawMatrixRace();
            } else if (type === 'history') {
                // Embed History Report
                container.append("iframe")
                    .attr("src", "benchmark_history.html")
                    .style("width", "100%")
                    .style("height", "100%")
                    .style("border", "none")
                    .style("background", "transparent");
            }
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
        const topN = 15;

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
            .attr("transform", "translate(0," + margin.top + ")")
            .call(d3.axisTop(x).ticks(5).tickFormat(d => "Matrix " + d))
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

            // Rank and Slice
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
            cSolver.results.forEach(r => cIters[r.matrix] = r.iterations);

            data = data.filter(s => {
                if (s.solver === 'C') return true;
                const hasMismatch = s.results.some(r => {
                    const expected = cIters[r.matrix];
                    return expected && r.iterations !== expected;
                });
                return !hasMismatch;
            });
        }

        // Use global minTime/maxTime
        const matrices = ["1.matrix", "2.matrix", "3.matrix", "4.matrix", "5.matrix", "6.matrix"];


        // Color Palette
        color = d3.scaleOrdinal()
            .domain(data.map(d => d.solver))
            .range(["#00ff9d", "#00b8ff", "#ff0055", "#ffcc00", "#bd00ff", "#00ffff"]);



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

            // X Axis
            const x = d3.scalePoint()
                .domain(matrices)
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
                .x(d => x(d.matrix))
                .y(d => y(Math.max(d.time, minTime)));

            // Draw Lines
            data.forEach(solver => {
                const solverData = solver.results.filter(r => matrices.includes(r.matrix));
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
                    .attr("transform", d => "translate(" + x(d.matrix) + ", " + y(Math.max(d.time, minTime)) + ")");

                // Logo Image
                pointGroup.append("image")
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
                    });

                // Label
                const lastPoint = solverData[solverData.length - 1];
                if (lastPoint) {
                    svg.append("text")
                        .attr("x", x(lastPoint.matrix) + 10)
                        .attr("y", y(Math.max(lastPoint.time, minTime)))
                        .attr("dy", "0.35em")
                        .style("fill", color(solver.solver))
                        .style("font-size", "12px")
                        .style("font-weight", "bold")
                        .text(solver.solver);
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

            // Y Axis Labels
            svg.append("g")
                .call(d3.axisLeft(y))
                .selectAll("text")
                .style("fill", "#e0e0e0")
                .style("font-family", "JetBrains Mono")
                .style("font-size", "10px");

            // X Axis
            svg.append("g")
                .attr("transform", "translate(0," + chartHeight + ")")
                .call(d3.axisBottom(x).ticks(5, ".1f"))
                .selectAll("text")
                .style("fill", "#5c5c66");
        } // End of drawJockeyChart

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
        // let active = false; // Moved to global scope
        let ignoreInput = false;
        let animationId;
        let frame = 0;

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


        function startScreensaver(mode) {
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
                document.body.appendChild(canvas); // Move to body
                canvas.style.position = 'fixed';
                canvas.style.top = '0';
                canvas.style.left = '0';
                canvas.style.width = '100vw';
                canvas.style.height = '100vh';
                canvas.style.zIndex = '1000';

                // Add fullscreen class to body to hide scrollbars
                document.body.classList.add('fullscreen-active');

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

            draw();
        }

        function stopScreensaver() {
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
            canvas.classList.remove('slide-down-slow');
            canvas.classList.remove('matrix-slide-enter');
            canvas.classList.remove('matrix-slide-active');
            content.classList.remove('content-slide-exit');
            content.classList.remove('content-slide-active');

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
        }

        // Idle Timer with Persistence
        const idleLimit = 5 * 60 * 1000; // 5 minutes

        function getLastActive() {
            return parseInt(localStorage.getItem('lastActive') || Date.now().toString());
        }

        function setLastActive() {
            localStorage.setItem('lastActive', Date.now().toString());
        }

        function checkIdle() {
            const lastActive = getLastActive();
            const diff = Date.now() - lastActive;
            if (diff >= idleLimit) {
                startScreensaver();
            }
        }

        setInterval(checkIdle, 1000);

        function resetTimer(e) {
            setLastActive();
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

        // Events to reset timer
        window.onload = resetTimer;
        document.onmousemove = resetTimer;
        document.onkeydown = resetTimer;
        document.onclick = resetTimer;
        document.onscroll = resetTimer;

        // Initialize
        if (!localStorage.getItem('lastActive')) {
            setLastActive();
        }
        checkIdle();

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
    const result = solver.results.find(r => r.matrix === matrixName);
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
    modal.style.display = 'block';

    // Anchor to cell
    const rect = cell.getBoundingClientRect();
    const content = modal.querySelector('.modal-content');

    // Calculate position (right of cell, aligned top)
    const top = rect.top + window.scrollY;
    const left = rect.right + window.scrollX + 20; // 20px gap

    content.style.position = 'absolute';
    content.style.top = top + 'px';
    content.style.left = left + 'px';
    content.style.margin = '0';
    content.style.transform = 'none';
    content.style.width = 'auto';
    content.style.maxWidth = '600px'; // Constrain width if needed
}

window.closeMismatchModal = function () {
    document.getElementById('mismatchModal').style.display = 'none';
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
function makeDraggable(elmnt, header) {
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
                makeDraggable(content, title);
            }
        }
    });
}

// Call initially and on load
enableModalDragging();
window.addEventListener('load', enableModalDragging);

