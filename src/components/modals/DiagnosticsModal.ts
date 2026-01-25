// Diagnostics Modal - shows benchmark issues and mismatches
import { BaseModal } from './BaseModal';

export class DiagnosticsModal extends BaseModal {
  private diagnostics: any[] = [];

  constructor() {
    super('DM', 'Diagnostics Modal', {
      dependencies: ['MetricsService']
    });
  }

  protected getDomId(): string {
    return 'diagnosticsModal';
  }

  /**
   * Show diagnostics for failed/mismatched benchmarks
   */
  showDiagnostics(diagnostics: any[]): void {
    this.diagnostics = diagnostics;
    this.open();
  }

  protected render(): HTMLElement {
    const title = 'Benchmark Diagnostics';
    const bodyContent = this.renderDiagnostics();

    return this.createModalElement(title, bodyContent);
  }

  private renderDiagnostics(): HTMLElement {
    const container = document.createElement('div');
    container.className = 'diagnostics-container';
    container.setAttribute('data-component-id', `${this.id}-CONTENT`);

    if (this.diagnostics.length === 0) {
      container.innerHTML = `
        <div class="diagnostics-section">
          <p>✅ No diagnostics available. All benchmarks passed!</p>
        </div>
      `;
      return container;
    }

    const sections = this.diagnostics.map((diag, idx) => `
      <div class="diagnostics-section">
        <h4>Issue #${idx + 1}: ${diag.language || 'Unknown'}</h4>
        <div class="diagnostic-item ${diag.type || 'error'}">
          <p><strong>Type:</strong> ${diag.type || 'Error'}</p>
          <p><strong>Message:</strong> ${diag.message || 'No message'}</p>
          ${diag.expectedIterations ? `
            <p><strong>Expected Iterations:</strong> <code>${diag.expectedIterations}</code></p>
            <p><strong>Actual Iterations:</strong> <code>${diag.actualIterations}</code></p>
          ` : ''}
          ${diag.stderr ? `
            <details>
              <summary>Error Output</summary>
              <pre><code>${diag.stderr}</code></pre>
            </details>
          ` : ''}
        </div>
      </div>
    `).join('');

    container.innerHTML = sections;

    return container;
  }
}

export const diagnosticsModal = new DiagnosticsModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).diagnosticsModal = diagnosticsModal;
  (window as any).showDiagnostics = (diagnostics: any[]) => {
    diagnosticsModal.showDiagnostics(diagnostics);
  };
}
