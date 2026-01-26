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
        <h1>Sudoku Benchmark Report v2.0</h1>
        <p class="subtitle">${metricsCount} language implementations</p>
        <div class="dev-banner">
          ⚠️ Development Build - Vite Hot Reload Active
        </div>
      </header>

      <main class="app-main">
        <section class="info-section">
          <h2>🎉 Phase 3 COMPLETE!</h2>
          <p><strong>All 8 modals implemented:</strong></p>
          <ul>
            <li>✅ BaseModal abstract class (draggable, keyboard handlers)</li>
            <li>✅ LanguageDetailsModal (LDM) - shows metrics & metadata</li>
            <li>✅ MethodologyModal (MM) - explains benchmark methodology</li>
            <li>✅ GoalsModal (GM) - project goals</li>
            <li>✅ WhyModal (WM) - project motivation</li>
            <li>✅ SourceCodeModal (SCM) - displays source code</li>
            <li>✅ DiagnosticsModal (DM) - benchmark issues</li>
            <li>✅ ScoreAnalysisModal (SAM) - score breakdown</li>
            <li>✅ InteractiveSolverModal (ISM) - placeholder for Phase 4</li>
            <li>✅ Modal styles CSS</li>
          </ul>

          <h3>Next Steps:</h3>
          <p>Phase 4 (Week 2-3): Migrate Solver Modules</p>
          <ul>
            <li>SolverEngine, SolverState, SolverGrid</li>
            <li>SolverAnimation, SolverControls, SolverEffects</li>
            <li>Complete InteractiveSolver with full UI</li>
            <li>⚠️ CRITICAL: DO NOT modify algorithm logic!</li>
          </ul>
        </section>

        <section class="test-section">
          <h2>🧪 Test Modals</h2>
          <div style="margin-bottom: 1rem;">
            <button id="test-lang-modal" class="btn">Language Details</button>
            <button id="test-methodology" class="btn">Methodology</button>
            <button id="test-goals" class="btn">Goals</button>
            <button id="test-why" class="btn">Why</button>
          </div>
          <div style="margin-bottom: 1rem;">
            <button id="test-source" class="btn">Source Code</button>
            <button id="test-diagnostics" class="btn">Diagnostics</button>
            <button id="test-score" class="btn">Score Analysis</button>
            <button id="test-solver" class="btn">Interactive Solver</button>
          </div>

          <h3>🧪 Test Infrastructure</h3>
          <button id="test-state-btn" class="btn">StateManager</button>
          <button id="test-events-btn" class="btn">EventBus</button>
          <button id="test-registry-btn" class="btn">Component Registry</button>
          <button id="test-persona-btn" class="btn">Persona</button>
          <pre id="test-output" class="test-output"></pre>
        </section>
      </main>
    </div>
  `;

  // Wire up test buttons
  wireUpTests();
}

function wireUpTests() {
  const output = document.getElementById('test-output');
  if (!output) return;

  document.getElementById('test-state-btn')?.addEventListener('click', () => {
    const state = appState.get();
    output.textContent = JSON.stringify(state, null, 2);
  });

  document.getElementById('test-events-btn')?.addEventListener('click', () => {
    eventBus.emit(Events.STATE_UPDATED, 'test event');
    const count = eventBus.listenerCount();
    output.textContent = `EventBus has ${count} total listeners\nEvent emitted: STATE_UPDATED`;
  });

  document.getElementById('test-registry-btn')?.addEventListener('click', () => {
    output.textContent = componentRegistry.summary();
  });

  document.getElementById('test-persona-btn')?.addEventListener('click', () => {
    const current = personalityService.getCurrent();
    output.textContent = JSON.stringify(current, null, 2);
  });

  // Modal test buttons
  document.getElementById('test-lang-modal')?.addEventListener('click', () => {
    const metrics = metricsService.getAll();
    const firstLang = metrics[0]?.language || 'C';
    languageDetailsModal.showForLanguage(firstLang);
    output.textContent = `Opened Language Details for: ${firstLang}`;
  });

  document.getElementById('test-methodology')?.addEventListener('click', () => {
    methodologyModal.open();
    output.textContent = 'Opened Methodology Modal';
  });

  document.getElementById('test-goals')?.addEventListener('click', () => {
    goalsModal.open();
    output.textContent = 'Opened Goals Modal';
  });

  document.getElementById('test-why')?.addEventListener('click', () => {
    whyModal.open();
    output.textContent = 'Opened Why Modal';
  });

  document.getElementById('test-source')?.addEventListener('click', () => {
    const sampleCode = `// Sample C code\n#include <stdio.h>\n\nint main() {\n    printf("Hello, World!\\n");\n    return 0;\n}`;
    sourceCodeModal.showForLanguage('C', sampleCode);
    output.textContent = 'Opened Source Code Modal';
  });

  document.getElementById('test-diagnostics')?.addEventListener('click', () => {
    const sampleDiagnostics = [
      {
        language: 'Python',
        type: 'error',
        message: 'Iteration count mismatch',
        expectedIterations: 656,
        actualIterations: 658
      }
    ];
    diagnosticsModal.showDiagnostics(sampleDiagnostics);
    output.textContent = 'Opened Diagnostics Modal';
  });

  document.getElementById('test-score')?.addEventListener('click', () => {
    const metrics = metricsService.getAll();
    const firstLang = metrics[0]?.language || 'C';
    scoreAnalysisModal.showForLanguage(firstLang);
    output.textContent = `Opened Score Analysis for: ${firstLang}`;
  });

  document.getElementById('test-solver')?.addEventListener('click', () => {
    interactiveSolverModal.open();
    output.textContent = 'Opened Interactive Solver Modal (placeholder)';
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
