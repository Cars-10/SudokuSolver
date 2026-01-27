// Sudoku Benchmark UI v2.0 - Application Entry Point
import { appState } from './core/StateManager';
import { eventBus, Events } from './core/EventBus';
import { componentRegistry } from './core/ComponentRegistry';
import { persistenceService } from './services/PersistenceService';
import { personalityService } from './services/PersonalityService';
import { metricsService } from './services/MetricsService';

// Import modular styles
import './styles/main.css';

// Import components
import { benchmarkTable } from './components/BenchmarkTable';
import { screensaver } from './components/Screensaver';
import { chartContainer } from './components/ChartContainer';

// Import components (modals)
import { languageDetailsModal } from './components/modals/LanguageDetailsModal';
import { methodologyModal } from './components/modals/MethodologyModal';
import { goalsModal } from './components/modals/GoalsModal';
import { whyModal } from './components/modals/WhyModal';
import { sourceCodeModal } from './components/modals/SourceCodeModal';
import { diagnosticsModal } from './components/modals/DiagnosticsModal';
import { scoreAnalysisModal } from './components/modals/ScoreAnalysisModal';
import { scoringInsightsModal } from './components/modals/ScoringInsightsModal';
import { interactiveSolverModal } from './components/modals/InteractiveSolverModal';

// Personas available
const PERSONAS = [
  'Standard', 'Neuromancer', 'Jockey', 'Professor', 'Surfer', 'Matrix',
  'Galactica', 'Star Trek', 'Star Wars', 'BTTF', 'Babylon 5', 'Expanse',
  'Terminator', 'LotR', 'Dune', 'Buck Rogers', 'Flash Gordon', 'Batman',
  'Alien', 'Blade Runner', 'Farscape', 'Apocalypse Now', 'Airplane',
  'Fast Times', 'Tron', 'Bill & Ted', 'John Wick', 'Dark Knight'
];

/**
 * Load language metadata from Algorithms/metadata.json via API
 * This provides author photos, descriptions, history, etc. for modals
 */
async function loadLanguageMetadata(): Promise<void> {
  try {
    const response = await fetch('/Algorithms/metadata.json');
    if (response.ok) {
      const data = await response.json();
      // Extract languageMetadata and expose globally for modals
      if (data.languageMetadata) {
        (window as any).languageMetadata = data.languageMetadata;
        console.log('📚 [App] Loaded language metadata for', Object.keys(data.languageMetadata).length, 'languages');
      }
    }
  } catch (err) {
    console.warn('[App] Failed to load language metadata:', err);
  }
}

async function init() {
  console.log('🚀 [App] Initializing Sudoku Benchmark UI v2.0');

  try {
    // 1. Restore persisted state
    persistenceService.init();

    // 2. Load metrics data
    await metricsService.load();

    // 2.5. Load language metadata for modals (authors, descriptions, etc.)
    await loadLanguageMetadata();

    // 3. Render main UI
    renderApp();

    // 4. Restore UI state from persisted state
    restoreUIState();

    // 5. Log component registry
    console.log('📦 [App] Registered components:', componentRegistry.list().length);
    console.log(componentRegistry.summary());

    // 6. Listen for persona changes
    eventBus.on(Events.PERSONA_CHANGED, (persona) => {
      console.log('🎭 [App] Persona changed to:', persona);
      updatePersonaUI(persona);
      // Update state
      appState.setSlice('ui', { currentPersona: persona });
    });

    // 7. Save scroll position on scroll (debounced)
    let scrollTimeout: number;
    window.addEventListener('scroll', () => {
      clearTimeout(scrollTimeout);
      scrollTimeout = window.setTimeout(() => {
        appState.setSlice('ui', { scrollY: window.scrollY });
      }, 500);
    });

    // 8. Debug helpers in dev mode
    if (import.meta.env.DEV) {
      console.log('🛠️ [App] Debug mode enabled');
      (window as any).appState = appState;
      (window as any).eventBus = eventBus;
      (window as any).metricsService = metricsService;
    }

    console.log('✅ [App] Initialization complete');
  } catch (err) {
    console.error('❌ [App] Initialization failed:', err);
    renderError(err);
  }
}

function renderApp() {
  const app = document.getElementById('app');
  if (!app) {
    console.error('[App] #app element not found');
    return;
  }

  const metrics = metricsService.getAll();
  const metricsCount = metrics.length;
  const mismatchCount = metrics.filter(m => m.hasMismatch).length;
  const failedCount = metrics.filter(m => m.failed).length;


  app.innerHTML = `
    <div class="app-container">
      <!-- Title -->
      <h1>Sudoku Benchmark Report</h1>

      <!-- Chart Section (at top, like legacy) -->
      <div id="chart-wrapper" class="chart-wrapper">
        <div class="chart-controls">
          <button class="zoom-btn" id="toggle-logo-mode" title="Toggle Logos/Text">
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
              <rect x="3" y="3" width="18" height="18" rx="2" ry="2"/>
              <circle cx="8.5" cy="8.5" r="1.5"/>
              <polyline points="21 15 16 10 5 21"/>
            </svg>
          </button>
          <button class="zoom-btn" id="chart-zoom-extent" title="Zoom Extent">
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
              <path d="M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3"/>
            </svg>
          </button>
          <button class="zoom-btn" id="chart-fullscreen" title="Fullscreen">
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
              <path d="M15 3h6v6M9 21H3v-6M21 3l-7 7M3 21l7-7"/>
            </svg>
          </button>
          <select id="chart-selector" class="btn active">
            <option value="bar">Performance Bar Chart</option>
            <option value="scatter">Time vs Memory</option>
            <option value="heatmap">Matrix Heatmap</option>
            <option value="histogram">Score Distribution</option>
            <option value="line">Line Chart</option>
            <option value="race">Matrix Race</option>
            <option value="jockey">Horse Race</option>
            <option value="iterations">Iteration Counts</option>
            <option value="algorithm">Algorithm Comparison</option>
          </select>
        </div>
        <div id="chart-container" class="chart-area"></div>
      </div>

      <!-- Top Bar (Legacy Structure) -->
      <div class="top-bar">
        <div id="personality-intro" class="personality-intro">
          Welcome to the Polyglot Sudoku Benchmark. Click on any language name for creator details. Use the controls to sort data and analyze performance metrics across different languages.
        </div>

        <div class="controls">
          <select id="persona-select" class="btn">
            <option value="" disabled selected>PERSONA</option>
            ${PERSONAS.map(p => `<option value="${p}">${p}</option>`).join('')}
          </select>

          <button class="btn" id="toggleMismatchesBtn" title="Toggle visibility of languages with iteration counts that don't match the C reference">
            <span>Hide Mismatches</span>
          </button>

          <button class="btn" id="toggleFailedBtn">Hide Failed</button>

          <button class="btn" id="btn-diagnostics">Diagnostics</button>

          <div class="dropdown">
            <button class="btn">Info ▾</button>
            <div class="dropdown-content">
              <a id="btn-methodology">Methodology</a>
              <a id="btn-goals">Goals</a>
              <a id="btn-why">Why???</a>
              <a id="btn-insights">Insights</a>
              <a id="btn-solver">Solver</a>
            </div>
          </div>

          <div class="dropdown">
            <button class="btn" id="langSelectorBtn">${metricsCount} LANGUAGES ▾</button>
            <div class="dropdown-content" id="language-selector-dropdown">
              <!-- Populated by JS -->
            </div>
          </div>

          <div class="dropdown">
            <button class="btn" id="algorithmSelectorBtn">ALL ALGORITHMS ▾</button>
            <div class="dropdown-content">
              <a data-algo="all" class="algo-filter active">All Algorithms</a>
              <a data-algo="BruteForce" class="algo-filter">Brute Force</a>
              <a data-algo="DLX" class="algo-filter">Dancing Links</a>
              <a data-algo="CP" class="algo-filter">Constraint Propagation</a>
            </div>
          </div>
        </div>
      </div>

      <!-- Tier Legend -->
      <div class="legend-container">
        <div class="legend-item">
          <span class="tier-badge tier-s">S</span> <span class="tier-label">&lt; 0.95 (Godlike)</span>
        </div>
        <div class="legend-item">
          <span class="tier-badge tier-a">A</span> <span class="tier-label">0.95 - 1.05 (Baseline)</span>
        </div>
        <div class="legend-item">
          <span class="tier-badge tier-b">B</span> <span class="tier-label">1.05 - 1.50 (Efficient)</span>
        </div>
        <div class="legend-item">
          <span class="tier-badge tier-c">C</span> <span class="tier-label">1.50 - 3.00 (Acceptable)</span>
        </div>
        <div class="legend-item">
          <span class="tier-badge tier-d">D</span> <span class="tier-label">3.00 - 10.00 (Slow)</span>
        </div>
        <div class="legend-item">
          <span class="tier-badge tier-f">F</span> <span class="tier-label">&gt; 10.00 (Glacial)</span>
        </div>
      </div>

      <!-- Table Section -->
      <div id="benchmark-table-container" class="table-section"></div>

      <!-- Footer -->
      <footer class="app-footer">
        <p>Powered by Vite 2.0 | <a href="https://github.com/yourrepo" target="_blank">View Source</a></p>
      </footer>
    </div>

    <!-- Pill Overlay (fixed top-right) -->
    <div class="pill-overlay">
      <div class="pill-container">
        <div class="pill-combined" title="Enter the Matrix">
          <div class="pill-half blue" id="blue-pill" title="Blue Pill - Stay in Wonderland"></div>
          <div class="pill-half red" id="red-pill" title="Red Pill - See how deep the rabbit hole goes"></div>
        </div>
      </div>
    </div>

    <!-- Table Scroll Buttons -->
    <div class="table-scroll-buttons">
      <button class="scroll-btn scroll-to-top" id="scroll-to-top" title="Scroll to Top">
        <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
          <polyline points="18 15 12 9 6 15"></polyline>
        </svg>
      </button>
      <button class="scroll-btn scroll-to-bottom" id="scroll-to-bottom" title="Scroll to Bottom">
        <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
          <polyline points="6 9 12 15 18 9"></polyline>
        </svg>
      </button>
    </div>
  `;

  // Render the chart inside the chart-area
  chartContainer.render('chart-container');

  // Render the benchmark table
  benchmarkTable.render('benchmark-table-container');

  // Wire up all controls
  wireUpControls();

  // Wire up chart controls
  wireUpChartControls();
}

function wireUpChartControls() {
  // Chart selector dropdown
  const chartSelector = document.getElementById('chart-selector') as HTMLSelectElement;
  chartSelector?.addEventListener('change', (e) => {
    const type = (e.target as HTMLSelectElement).value;
    chartContainer.renderChart(type as any);
    // Save to state
    appState.setSlice('charts', { mode: type as any });
  });

  // Toggle logo mode button
  document.getElementById('toggle-logo-mode')?.addEventListener('click', () => {
    const btn = document.getElementById('toggle-logo-mode');
    const showLogos = chartContainer.toggleLogos();
    btn?.classList.toggle('active', !showLogos);
    btn?.setAttribute('title', showLogos ? 'Hide Logos' : 'Show Logos');
    console.log('[Chart] Logo mode:', showLogos ? 'logos' : 'text');
  });

  // Zoom extent button
  document.getElementById('chart-zoom-extent')?.addEventListener('click', () => {
    // Reset chart zoom to fit all data
    console.log('[Chart] Zoom to extent');
    chartContainer.resize(); // Re-render at current size
  });

  // Fullscreen button
  document.getElementById('chart-fullscreen')?.addEventListener('click', () => {
    const wrapper = document.getElementById('chart-wrapper');
    if (wrapper) {
      if (!document.fullscreenElement) {
        wrapper.requestFullscreen().catch(err => console.warn('Fullscreen failed:', err));
      } else {
        document.exitFullscreen();
      }
    }
  });
}

function wireUpControls() {
  // Track toggle states
  let showMismatches = true;
  let showFailed = true;

  // Info dropdown buttons
  document.getElementById('btn-methodology')?.addEventListener('click', () => {
    methodologyModal.open();
  });

  document.getElementById('btn-goals')?.addEventListener('click', () => {
    goalsModal.open();
  });

  document.getElementById('btn-why')?.addEventListener('click', () => {
    whyModal.open();
  });

  document.getElementById('btn-diagnostics')?.addEventListener('click', () => {
    diagnosticsModal.open();
  });

  document.getElementById('btn-solver')?.addEventListener('click', () => {
    interactiveSolverModal.open();
  });

  document.getElementById('btn-insights')?.addEventListener('click', () => {
    // Show scoring insights modal with statistical analysis
    scoringInsightsModal.open();
  });

  // Screensaver pills
  document.getElementById('red-pill')?.addEventListener('click', () => {
    screensaver.start('red');
  });

  document.getElementById('blue-pill')?.addEventListener('click', () => {
    screensaver.start('blue');
  });

  // Table scroll buttons
  document.getElementById('scroll-to-top')?.addEventListener('click', () => {
    const tableContainer = document.getElementById('benchmark-table-container');
    if (tableContainer) {
      tableContainer.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  });

  document.getElementById('scroll-to-bottom')?.addEventListener('click', () => {
    const table = document.getElementById('benchmark-table');
    if (table) {
      // Find last visible main row (not expand rows, not hidden rows)
      const visibleRows = table.querySelectorAll('tbody tr.main-row:not([style*="display: none"])');
      if (visibleRows.length > 0) {
        const lastRow = visibleRows[visibleRows.length - 1];
        lastRow.scrollIntoView({ behavior: 'smooth', block: 'end' });
      }
    }
  });

  // Toggle mismatches button
  const toggleMismatchesBtn = document.getElementById('toggleMismatchesBtn');
  toggleMismatchesBtn?.addEventListener('click', () => {
    showMismatches = !showMismatches;
    const span = toggleMismatchesBtn.querySelector('span');
    if (span) {
      span.textContent = showMismatches ? 'Hide Mismatches' : 'Show Mismatches';
    }
    toggleMismatchesBtn.classList.toggle('active', !showMismatches);
    benchmarkTable.filter({ showMismatches });
    // Save to state
    appState.setSlice('ui', { showMismatches });
  });

  // Toggle failed button
  const toggleFailedBtn = document.getElementById('toggleFailedBtn');
  toggleFailedBtn?.addEventListener('click', () => {
    showFailed = !showFailed;
    toggleFailedBtn.textContent = showFailed ? 'Hide Failed' : 'Show Failed';
    toggleFailedBtn.classList.toggle('active', !showFailed);
    benchmarkTable.filter({ showFailed });
    // Save to state
    appState.setSlice('ui', { showFailed });
  });

  // Algorithm filter dropdown
  const algoNames: Record<string, string> = {
    'all': 'All Algorithms',
    'BruteForce': 'Brute Force',
    'DLX': 'Dancing Links',
    'CP': 'Constraint Propagation'
  };

  const algoFilterElements = document.querySelectorAll('.algo-filter');
  console.log(`[App] Found ${algoFilterElements.length} .algo-filter elements`);

  algoFilterElements.forEach(el => {
    el.addEventListener('click', (e) => {
      const target = e.target as HTMLElement;
      const algo = target.dataset.algo;
      console.log(`[App] Algorithm filter clicked: "${algo}"`);

      // Update active state
      document.querySelectorAll('.algo-filter').forEach(a => a.classList.remove('active'));
      target.classList.add('active');

      // Filter table
      const filterValue = algo === 'all' ? null : algo || null;
      console.log(`[App] Calling benchmarkTable.filter with algorithm: "${filterValue}"`);
      benchmarkTable.filter({ algorithm: filterValue });

      // Update button text
      const btn = document.getElementById('algorithmSelectorBtn');
      if (btn) {
        btn.textContent = (algoNames[algo || 'all'] || algo) + ' ▾';
      }
    });
  });

  // Persona selector
  const personaSelect = document.getElementById('persona-select') as HTMLSelectElement;
  personaSelect?.addEventListener('change', (e) => {
    const persona = (e.target as HTMLSelectElement).value;
    personalityService.setPersona(persona);
    eventBus.emit(Events.PERSONA_CHANGED, persona);
  });

  // Global function for language details modal (used by BenchmarkTable)
  (window as any).showLanguageDetails = (lang: string) => {
    languageDetailsModal.showForLanguage(lang);
  };

  // Populate language selector dropdown
  populateLanguageDropdown();
}

// Store locked languages in memory (and restore from state)
const lockedLanguages: Map<string, number> = new Map();
const selectedLanguages: Set<string> = new Set();

function getLanguageStatus(lang: string): { status: string; color: string } {
  const metrics = metricsService.getByLanguage(lang);
  if (!metrics) return { status: 'Unknown', color: '#565f89' };
  if (metrics.failed) return { status: 'Failed', color: '#ff5555' };
  if (metrics.hasMismatch) return { status: 'Mismatch', color: '#ffaa00' };
  if (metrics.score && metrics.score < 2) return { status: 'Fast', color: '#00ff9d' };
  return { status: 'Ready', color: '#00b8ff' };
}

function populateLanguageDropdown() {
  const dropdown = document.getElementById('language-selector-dropdown');
  if (!dropdown) return;

  const metrics = metricsService.getAll();

  // Initialize selectedLanguages with all languages if empty
  if (selectedLanguages.size === 0) {
    metrics.forEach(m => {
      const lang = m.language || m.solver;
      selectedLanguages.add(lang);
    });
  }

  // Bucket languages by status
  const locked: typeof metrics = [];
  const selected: typeof metrics = [];
  const available: typeof metrics = [];

  metrics.forEach(m => {
    const lang = m.language || m.solver;
    if (lockedLanguages.has(lang)) {
      locked.push(m);
    } else if (selectedLanguages.has(lang)) {
      selected.push(m);
    } else {
      available.push(m);
    }
  });

  // Sort within buckets
  locked.sort((a, b) => (a.language || a.solver).localeCompare(b.language || b.solver));
  selected.sort((a, b) => (a.language || a.solver).localeCompare(b.language || b.solver));
  available.sort((a, b) => (a.language || a.solver).localeCompare(b.language || b.solver));

  const renderItem = (m: typeof metrics[0], isSelected: boolean) => {
    const lang = m.language || m.solver;
    const displayName = lang === 'C_Sharp' ? 'C#' : lang === 'F_Sharp' ? 'F#' : lang;
    const isLocked = lockedLanguages.has(lang);
    const { status, color } = getLanguageStatus(lang);

    return `
      <div class="lang-selector-item" data-lang="${lang}">
        <button class="lock-btn ${isLocked ? 'locked' : ''}" data-lang="${lang}" title="${isLocked ? 'Unlock' : 'Lock'}">
          ${isLocked ? '🔒' : '🔓'}
        </button>
        <label class="lang-checkbox-label">
          <input type="checkbox" value="${lang}" ${isSelected ? 'checked' : ''} class="lang-checkbox">
          <span class="lang-name">${displayName}</span>
        </label>
        <span class="lang-status-badge" style="background: ${color}22; color: ${color};">${status}</span>
      </div>
    `;
  };

  let html = `
    <div class="lang-selector-header">
      <button class="btn-select-all">Select All</button>
      <button class="btn-clear-all">Clear All</button>
    </div>
  `;

  if (locked.length > 0) {
    html += `<div class="lang-selector-section-header">🔒 LOCKED (${locked.length})</div>`;
    locked.forEach(m => { html += renderItem(m, selectedLanguages.has(m.language || m.solver)); });
  }

  if (selected.length > 0) {
    html += `<div class="lang-selector-section-header">✓ VISIBLE (${selected.length})</div>`;
    selected.forEach(m => { html += renderItem(m, true); });
  }

  if (available.length > 0) {
    html += `<div class="lang-selector-section-header">○ HIDDEN (${available.length})</div>`;
    available.forEach(m => { html += renderItem(m, false); });
  }

  dropdown.innerHTML = html;

  // Wire up lock buttons
  dropdown.querySelectorAll('.lock-btn').forEach(btn => {
    btn.addEventListener('click', (e) => {
      e.stopPropagation();
      const lang = (e.currentTarget as HTMLElement).dataset.lang;
      if (!lang) return;

      if (lockedLanguages.has(lang)) {
        lockedLanguages.delete(lang);
      } else {
        lockedLanguages.set(lang, Date.now());
      }
      populateLanguageDropdown(); // Refresh
    });
  });

  // Wire up checkboxes
  dropdown.querySelectorAll('.lang-checkbox').forEach(checkbox => {
    checkbox.addEventListener('change', (e) => {
      const input = e.target as HTMLInputElement;
      const lang = input.value;
      if (input.checked) {
        selectedLanguages.add(lang);
      } else {
        selectedLanguages.delete(lang);
      }
      // Update table visibility
      const row = document.querySelector(`tr[data-lang="${lang}"]`) as HTMLElement;
      if (row) {
        row.style.display = input.checked ? '' : 'none';
      }
    });
  });

  // Wire up select all / clear all buttons
  dropdown.querySelector('.btn-select-all')?.addEventListener('click', () => {
    metrics.forEach(m => {
      const lang = m.language || m.solver;
      if (!lockedLanguages.has(lang)) {
        selectedLanguages.add(lang);
      }
    });
    populateLanguageDropdown();
    applyLanguageVisibility();
  });

  dropdown.querySelector('.btn-clear-all')?.addEventListener('click', () => {
    metrics.forEach(m => {
      const lang = m.language || m.solver;
      if (!lockedLanguages.has(lang)) {
        selectedLanguages.delete(lang);
      }
    });
    populateLanguageDropdown();
    applyLanguageVisibility();
  });

  // Scroll to language on item click (anywhere except checkbox/lock)
  dropdown.querySelectorAll('.lang-selector-item').forEach(item => {
    item.addEventListener('click', (e) => {
      const target = e.target as HTMLElement;
      // Don't trigger on checkbox or lock button
      if (target.tagName === 'INPUT' || target.classList.contains('lock-btn')) return;

      const lang = item.getAttribute('data-lang');
      if (lang) {
        const row = document.querySelector(`tr[data-lang="${lang}"]`);
        if (row) {
          row.scrollIntoView({ behavior: 'smooth', block: 'center' });
          row.classList.add('highlight');
          setTimeout(() => row.classList.remove('highlight'), 2000);
        }
      }
    });
  });

  // Update button text
  const langBtn = document.getElementById('langSelectorBtn');
  if (langBtn) {
    const visibleCount = selectedLanguages.size;
    langBtn.textContent = `${visibleCount} LANGUAGES ▾`;
  }
}

function applyLanguageVisibility() {
  const rows = document.querySelectorAll('tr[data-lang]');
  rows.forEach(row => {
    const lang = row.getAttribute('data-lang');
    if (lang) {
      (row as HTMLElement).style.display = selectedLanguages.has(lang) ? '' : 'none';
    }
  });
}

function restoreUIState() {
  const state = appState.get();

  // Restore persona
  const personaSelect = document.getElementById('persona-select') as HTMLSelectElement;
  if (personaSelect && state.ui.currentPersona) {
    personaSelect.value = state.ui.currentPersona;
    if (state.ui.currentPersona !== 'Standard') {
      updatePersonaUI(state.ui.currentPersona);
    }
  }

  // Restore chart type
  const chartSelector = document.getElementById('chart-selector') as HTMLSelectElement;
  if (chartSelector && state.charts.mode) {
    chartSelector.value = state.charts.mode;
    chartContainer.renderChart(state.charts.mode as any);
  }

  // Restore filter states
  const toggleMismatchesBtn = document.getElementById('toggleMismatchesBtn');
  if (toggleMismatchesBtn && !state.ui.showMismatches) {
    toggleMismatchesBtn.classList.add('active');
    const span = toggleMismatchesBtn.querySelector('span');
    if (span) span.textContent = 'Show Mismatches';
    benchmarkTable.filter({ showMismatches: false });
  }

  const toggleFailedBtn = document.getElementById('toggleFailedBtn');
  if (toggleFailedBtn && !state.ui.showFailed) {
    toggleFailedBtn.classList.add('active');
    toggleFailedBtn.textContent = 'Show Failed';
    benchmarkTable.filter({ showFailed: false });
  }

  // Restore scroll position (slight delay for rendering)
  if (state.ui.scrollY > 0) {
    setTimeout(() => {
      window.scrollTo(0, state.ui.scrollY);
    }, 100);
  }

  console.log('🔄 [App] UI state restored');
}

function updatePersonaUI(persona: string) {
  // Access persona data from window globals (loaded from static_data.js)
  const win = window as any;

  // Update intro text
  const introEl = document.getElementById('personality-intro');
  if (introEl) {
    const introText = win.narratorIntros?.[persona] || win.narratorIntros?.['Standard'] ||
      'Welcome to the Sudoku Benchmark. Click on any language name for creator details.';
    introEl.textContent = introText;
  }

  // Update mismatch button label
  const mismatchBtn = document.getElementById('toggleMismatchesBtn');
  if (mismatchBtn) {
    const mismatchLabel = win.mismatchLabels?.[persona] || win.mismatchLabels?.['Standard'] || 'MISMATCHES';
    const span = mismatchBtn.querySelector('span');
    if (span) {
      const isHidden = mismatchBtn.classList.contains('active');
      span.textContent = isHidden ? `Show ${mismatchLabel}` : `Hide ${mismatchLabel}`;
    }
  }

  // Update row tooltips with persona-specific quotes
  const rows = document.querySelectorAll('tr[data-lang]');
  const quotes = win.personalities?.[persona] || win.personalities?.['Standard'];
  rows.forEach(row => {
    const lang = row.getAttribute('data-lang');
    if (lang && quotes) {
      const quote = quotes[lang] || quotes['default'] || '';
      const score = row.getAttribute('data-score') || '0';
      const time = parseFloat(row.getAttribute('data-time') || '0');
      const timeStr = time < 1 ? (time * 1000).toFixed(2) + ' ms' : time.toFixed(4) + 's';
      const fullQuote = quote + ` Efficiency: ${parseFloat(score).toFixed(2)}x | Time: ${timeStr}`;
      row.setAttribute('data-quote', fullQuote);
      row.setAttribute('title', fullQuote);
    }
  });

  // Store current persona globally for other components
  win.currentPersona = persona;

  console.log(`🎭 [App] Persona UI updated: ${persona}`);
}

function renderError(error: any) {
  const app = document.getElementById('app');
  if (!app) return;

  app.innerHTML = `
    <div class="error-container">
      <h1>Initialization Error</h1>
      <p>Failed to initialize the application:</p>
      <pre class="error-details">${error.message || error}</pre>
      <button onclick="location.reload()">Reload Page</button>
    </div>
  `;
}

// Start app
init().catch(console.error);
