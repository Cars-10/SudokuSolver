const state = {
    matrices: [],
    languages: [],
    currentMatrix: null,
    matrixContent: null
};

// DOM Elements
const matrixSelect = document.getElementById('matrixSelect');
const sudokuGrid = document.getElementById('sudokuGrid');
const contextMenu = document.getElementById('contextMenu');
const languageList = document.getElementById('languageList');
const langSearch = document.getElementById('langSearch');
const outputLog = document.getElementById('outputLog');
const statusIndicator = document.getElementById('statusIndicator');

// Initialize
async function init() {
    await fetchMatrices();
    await fetchLanguages();

    // Event Listeners
    matrixSelect.addEventListener('change', async (e) => loadMatrix(e.target.value));

    // Global click to close context menu
    document.addEventListener('click', (e) => {
        if (!contextMenu.contains(e.target)) {
            hideContextMenu();
        }
    });

    // Language Search
    langSearch.addEventListener('input', (e) => {
        renderLanguageList(e.target.value);
    });
}

// Fetch Lists
async function fetchMatrices() {
    try {
        const res = await fetch('/api/matrices');
        state.matrices = await res.json();

        // Populate select
        state.matrices.forEach(m => {
            const option = document.createElement('option');
            option.value = m;
            option.textContent = m;
            matrixSelect.appendChild(option);
        });
    } catch (err) {
        log('Error fetching matrices: ' + err, 'error');
    }
}

async function fetchLanguages() {
    try {
        const res = await fetch('/api/languages');
        state.languages = await res.json();
        renderLanguageList();
    } catch (err) {
        log('Error fetching languages: ' + err, 'error');
    }
}

// Render Language Menu
function renderLanguageList(filter = '') {
    languageList.innerHTML = '';
    const filtered = state.languages.filter(l => l.toLowerCase().includes(filter.toLowerCase()));

    filtered.forEach(lang => {
        const item = document.createElement('div');
        item.className = 'menu-item';
        item.textContent = lang;
        item.onclick = () => runMatrix(lang);
        languageList.appendChild(item);
    });
}

// Load & Render Matrix
async function loadMatrix(filename) {
    state.currentMatrix = filename;
    try {
        const res = await fetch(`/api/matrix/${filename}`);
        const data = await res.json();
        state.matrixContent = data.content;
        renderGrid(data.content);
    } catch (err) {
        log('Error loading matrix: ' + err, 'error');
    }
}

function renderGrid(content) {
    sudokuGrid.innerHTML = '';
    // Parse content: simple split by whitespace/newlines, ignore empty
    // The format seems to be 9 lines of 9 numbers
    const lines = content.trim().split('\n').filter(l => l.trim().length > 0 && !l.startsWith('#'));

    // Flatten
    let numbers = [];
    lines.forEach(line => {
        numbers = numbers.concat(line.trim().split(/\s+/));
    });

    // Ensure 81 numbers
    if (numbers.length < 81) {
        // Just fill with placeholders if parsing fails gracefully
        log('Warning: Matrix file format unexpected', 'warn');
    }

    for (let i = 0; i < 81; i++) {
        const cell = document.createElement('div');
        cell.className = 'cell';
        cell.textContent = numbers[i] === '0' ? '' : numbers[i]; // 0 usually empty in Sudoku files

        // Add thick borders for 3x3 subgrids
        const col = i % 9;
        const row = Math.floor(i / 9);

        if (col === 2 || col === 5) cell.classList.add('thick-right');
        if (row === 2 || row === 5) cell.classList.add('thick-bottom');

        // Context Menu Event
        cell.addEventListener('contextmenu', (e) => {
            e.preventDefault();
            showContextMenu(e.clientX, e.clientY);
        });

        sudokuGrid.appendChild(cell);
    }
}

// Context Menu Logic
function showContextMenu(x, y) {
    if (!state.currentMatrix) return;

    contextMenu.style.display = 'block';

    // Adjust position to stay on screen
    const w = contextMenu.offsetWidth;
    const h = contextMenu.offsetHeight; // might vary if list is long

    // Simple boundary check
    if (x + w > window.innerWidth) x -= w;
    // Don't check Y too strictly for now as the list scrolls

    contextMenu.style.left = `${x}px`;
    contextMenu.style.top = `${y}px`;
}

function hideContextMenu() {
    contextMenu.style.display = 'none';
}

// Execution
async function runMatrix(language) {
    hideContextMenu();
    if (!state.currentMatrix) return;

    statusIndicator.textContent = `Running ${language}...`;
    statusIndicator.style.backgroundColor = 'var(--accent)';
    log(`Starting execution of ${state.currentMatrix} with ${language}...`);

    try {
        const res = await fetch('/api/run', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                language,
                matrix: state.currentMatrix
            })
        });

        const result = await res.json();

        if (result.success) {
            statusIndicator.textContent = 'Success';
            statusIndicator.style.backgroundColor = 'var(--success)';
            log('Execution Successful', 'success');
            log('STDOUT:\n' + result.stdout);
            if (result.stderr) log('STDERR:\n' + result.stderr);
        } else {
            statusIndicator.textContent = 'Failed';
            statusIndicator.style.backgroundColor = 'var(--error)';
            log('Execution Failed (Exit Code: ' + result.exitCode + ')', 'error');
            log('STDERR:\n' + result.stderr);
            log('STDOUT:\n' + result.stdout);
        }

    } catch (err) {
        statusIndicator.textContent = 'Error';
        statusIndicator.style.backgroundColor = 'var(--error)';
        log('Network Error: ' + err, 'error');
    }
}

function log(msg, type = 'info') {
    const timestamp = new Date().toLocaleTimeString();
    outputLog.textContent = `[${timestamp}] ${msg}\n\n` + outputLog.textContent;
}

init();
