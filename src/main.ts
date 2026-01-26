// Sudoku Benchmark UI v2.0 - Application Entry Point
import { appState } from './core/StateManager';
import { eventBus, Events } from './core/EventBus';
import { componentRegistry } from './core/ComponentRegistry';
import { persistenceService } from './services/PersistenceService';
import { personalityService } from './services/PersonalityService';
import { metricsService } from './services/MetricsService';

// Import styles
import './styles/variables.css';
import './styles/base.css';
import './styles/modals.css';
import './styles/tables.css';

// Import components
import { benchmarkTable } from './components/BenchmarkTable';

// Import components (modals)
import { languageDetailsModal } from './components/modals/LanguageDetailsModal';
import { methodologyModal } from './components/modals/MethodologyModal';
import { goalsModal } from './components/modals/GoalsModal';
import { whyModal } from './components/modals/WhyModal';
import { sourceCodeModal } from './components/modals/SourceCodeModal';
import { diagnosticsModal } from './components/modals/DiagnosticsModal';
import { scoreAnalysisModal } from './components/modals/ScoreAnalysisModal';
import { interactiveSolverModal } from './components/modals/InteractiveSolverModal';

async function init() {
  console.log('🚀 [App] Initializing Sudoku Benchmark UI v2.0');

  try {
    // 1. Restore persisted state
    persistenceService.init();

    // 2. Load metrics data
    await metricsService.load();

    // 3. Render main UI
    renderApp();

    // 4. Log component registry
    console.log('📦 [App] Registered components:', componentRegistry.list().length);
    console.log(componentRegistry.summary());

    // 5. Listen for persona changes
    eventBus.on(Events.PERSONA_CHANGED, (persona) => {
      console.log('🎭 [App] Persona changed to:', persona);
    });

    // 6. Debug helpers in dev mode
    if (import.meta.env.DEV) {
      console.log('🛠️ [App] Debug mode enabled');
      console.log('Available globals: appState, eventBus, componentRegistry');
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

  const metricsCount = metricsService.getAll().length;

  app.innerHTML = `
    <div class="app-container">
      <header class="app-header">
        <h1>Sudoku Benchmark Report</h1>
        <p class="subtitle">${metricsCount} Language Implementations</p>
        ${import.meta.env.DEV ? '<div class="dev-banner">⚠️ Development Mode - Vite v2.0</div>' : ''}
      </header>

      <main class="app-main">
        <div style="text-align: center; margin-bottom: 2rem;">
          <button id="test-methodology" class="btn">Methodology</button>
          <button id="test-goals" class="btn">Goals</button>
          <button id="test-why" class="btn">Why</button>
          <button id="test-solver" class="btn">Interactive Solver</button>
        </div>

        <div id="benchmark-table-container"></div>
      </main>
    </div>
  `;

  // Render the benchmark table
  benchmarkTable.render('benchmark-table-container');

  // Wire up modal buttons
  wireUpModalButtons();
}

function wireUpModalButtons() {
  document.getElementById('test-methodology')?.addEventListener('click', () => {
    methodologyModal.open();
  });

  document.getElementById('test-goals')?.addEventListener('click', () => {
    goalsModal.open();
  });

  document.getElementById('test-why')?.addEventListener('click', () => {
    whyModal.open();
  });

  document.getElementById('test-solver')?.addEventListener('click', () => {
    interactiveSolverModal.open();
  });
}

function renderError(error: any) {
  const app = document.getElementById('app');
  if (!app) return;

  app.innerHTML = `
    <div class="error-container">
      <h1>⚠️ Initialization Error</h1>
      <p>Failed to initialize the application:</p>
      <pre class="error-details">${error.message || error}</pre>
      <button onclick="location.reload()">Reload Page</button>
    </div>
  `;
}

// Start app
init().catch(console.error);
