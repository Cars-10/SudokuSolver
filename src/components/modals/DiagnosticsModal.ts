// Diagnostics Modal - shows benchmark issues and mismatches
import { BaseModal } from './BaseModal';
import { metricsService } from '../../services/MetricsService';

// Reference iteration counts for each algorithm
const REFERENCE_ITERATIONS: Record<string, Record<string, number>> = {
  BruteForce: {
    '1': 656,
    '2': 439269,
    '3': 98847,
    '4': 9085,
    '5': 445778,
    '6': 622577597
  },
  DLX: {
    '1': 48,
    '2': 1594,
    '3': 513,
    '4': 152,
    '5': 2331
  },
  CP: {}
};

interface AlgoStats {
  total: number;
  success: number;
  mismatches: number;
  failed: number;
}

interface MismatchDetail {
  language: string;
  algo: string;
  matrices: { matrix: string; actual: number; expected: number }[];
}

export class DiagnosticsModal extends BaseModal {
  constructor() {
    super('DM', 'Diagnostics Modal', {
      dependencies: ['MetricsService']
    });
  }

  protected getDomId(): string {
    return 'diagnosticsModal';
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

    const metrics = metricsService.getAll();

    // Calculate algorithm-specific stats
    const algoStats: Record<string, AlgoStats> = {
      BruteForce: { total: 0, success: 0, mismatches: 0, failed: 0 },
      DLX: { total: 0, success: 0, mismatches: 0, failed: 0 },
      CP: { total: 0, success: 0, mismatches: 0, failed: 0 }
    };

    const mismatchDetails: MismatchDetail[] = [];
    const failedLanguages: { language: string; algo: string; error?: string }[] = [];
    const timeoutLanguages: { language: string; algo: string; matrices: string[] }[] = [];

    metrics.forEach(m => {
      const algo = m.algorithm || 'BruteForce';
      if (!algoStats[algo]) {
        algoStats[algo] = { total: 0, success: 0, mismatches: 0, failed: 0 };
      }
      algoStats[algo].total++;

      // Check for failed benchmarks
      if (m.failed) {
        algoStats[algo].failed++;
        failedLanguages.push({
          language: m.language || m.solver,
          algo,
          error: m.failureReason
        });
        return;
      }

      // Check for successful results
      const hasSuccess = m.results?.some(r => r.status === 'success');
      if (hasSuccess) algoStats[algo].success++;

      // Check for timeouts
      const timeouts = m.results?.filter(r => r.status === 'timeout') || [];
      if (timeouts.length > 0) {
        timeoutLanguages.push({
          language: m.language || m.solver,
          algo,
          matrices: timeouts.map(t => t.matrix)
        });
      }

      // Check for iteration mismatches
      const baseline = REFERENCE_ITERATIONS[algo];
      if (baseline && m.language !== 'C') {
        const mismatchMatrices: { matrix: string; actual: number; expected: number }[] = [];

        m.results?.forEach(r => {
          if (r.status !== 'success') return;
          const expected = baseline[r.matrix];
          if (expected !== undefined && r.iterations !== expected) {
            mismatchMatrices.push({
              matrix: r.matrix,
              actual: r.iterations,
              expected
            });
          }
        });

        if (mismatchMatrices.length > 0) {
          algoStats[algo].mismatches++;
          mismatchDetails.push({
            language: m.language || m.solver,
            algo,
            matrices: mismatchMatrices
          });
        }
      }
    });

    // Build HTML content
    let html = '';

    // Algorithm Summary Section
    html += this.renderAlgorithmSummary(algoStats);

    // C Baseline Reference
    html += this.renderBaselineReference();

    // Error Categories Grid
    html += this.renderErrorCategories(failedLanguages, timeoutLanguages, mismatchDetails);

    // Iteration Mismatches Detail
    if (mismatchDetails.length > 0) {
      html += this.renderMismatchDetails(mismatchDetails);
    }

    // Success message if no issues
    const totalIssues = failedLanguages.length + timeoutLanguages.length + mismatchDetails.length;
    if (totalIssues === 0) {
      html += `
        <div class="diagnostics-success">
          All ${metrics.length} benchmarks passed without issues.
        </div>
      `;
    }

    container.innerHTML = html;
    return container;
  }

  private renderAlgorithmSummary(algoStats: Record<string, AlgoStats>): string {
    const algoLabels: Record<string, string> = {
      BruteForce: 'Brute Force',
      DLX: 'Dancing Links',
      CP: 'Constraint Prop'
    };
    const algoColors: Record<string, string> = {
      BruteForce: '#00ff9d',
      DLX: '#00b8ff',
      CP: '#bb9af7'
    };

    let html = `
      <div class="diagnostics-section">
        <h3 class="diagnostics-section-title" style="color: #7aa2f7;">Algorithm Summary</h3>
        <div class="algo-summary-grid">
    `;

    ['BruteForce', 'DLX', 'CP'].forEach(algo => {
      const stats = algoStats[algo];
      const issues = stats.failed + stats.mismatches;
      html += `
        <div class="algo-summary-card">
          <div class="algo-name" style="color: ${algoColors[algo]};">${algoLabels[algo]}</div>
          <div class="algo-stats">
            <div>Total: <strong>${stats.total}</strong></div>
            <div>Success: <strong style="color: #00ff9d;">${stats.success}</strong></div>
            <div>Issues: <strong style="color: ${issues > 0 ? '#ff5555' : '#565f89'};">${issues}</strong></div>
          </div>
        </div>
      `;
    });

    html += '</div></div>';
    return html;
  }

  private renderBaselineReference(): string {
    const algoLabels: Record<string, string> = {
      BruteForce: 'Brute Force',
      DLX: 'Dancing Links',
      CP: 'Constraint Prop'
    };
    const algoColors: Record<string, string> = {
      BruteForce: '#00ff9d',
      DLX: '#00b8ff',
      CP: '#bb9af7'
    };

    let html = `
      <div class="diagnostics-section">
        <h3 class="diagnostics-section-title" style="color: #ffd700;">C Baseline Iterations (Reference)</h3>
        <div class="baseline-grid">
    `;

    ['BruteForce', 'DLX', 'CP'].forEach(algo => {
      const baseline = REFERENCE_ITERATIONS[algo] || {};
      const matrices = Object.keys(baseline).sort((a, b) => Number(a) - Number(b));

      html += `
        <div class="baseline-card">
          <div class="algo-name" style="color: ${algoColors[algo]};">${algoLabels[algo]}</div>
          ${matrices.length > 0
            ? matrices.map(m => `<div>Matrix ${m}: <strong>${baseline[m].toLocaleString()}</strong></div>`).join('')
            : '<div class="muted">No data</div>'
          }
        </div>
      `;
    });

    html += '</div></div>';
    return html;
  }

  private renderErrorCategories(
    failedLanguages: { language: string; algo: string; error?: string }[],
    timeoutLanguages: { language: string; algo: string; matrices: string[] }[],
    mismatchDetails: MismatchDetail[]
  ): string {
    let html = '<div class="diagnostics-grid">';

    // Failed benchmarks
    html += `
      <div class="diagnostics-card error">
        <h4>Failed Benchmarks (${failedLanguages.length})</h4>
        <div class="diagnostics-list">
          ${failedLanguages.length > 0
            ? failedLanguages.map(f => `
                <div class="diagnostics-item">
                  <strong>${f.language}</strong>
                  <span class="muted">(${f.algo})</span>
                  ${f.error ? `<div class="error-text">${f.error}</div>` : ''}
                </div>
              `).join('')
            : '<div class="muted">None</div>'
          }
        </div>
      </div>
    `;

    // Timeouts
    html += `
      <div class="diagnostics-card warning">
        <h4>Timeouts (${timeoutLanguages.length})</h4>
        <div class="diagnostics-list">
          ${timeoutLanguages.length > 0
            ? timeoutLanguages.map(t => `
                <div class="diagnostics-item">
                  <strong>${t.language}</strong>
                  <span class="muted">(M${t.matrices.join(', M')})</span>
                </div>
              `).join('')
            : '<div class="muted">None</div>'
          }
        </div>
      </div>
    `;

    // Mismatches summary
    html += `
      <div class="diagnostics-card mismatch">
        <h4>Iteration Mismatches (${mismatchDetails.length})</h4>
        <div class="diagnostics-list">
          ${mismatchDetails.length > 0
            ? mismatchDetails.slice(0, 10).map(m => `
                <div class="diagnostics-item">
                  <strong>${m.language}</strong>
                  <span class="muted">(${m.matrices.length} matrices)</span>
                </div>
              `).join('') + (mismatchDetails.length > 10 ? `<div class="muted">...and ${mismatchDetails.length - 10} more</div>` : '')
            : '<div class="muted">None</div>'
          }
        </div>
      </div>
    `;

    html += '</div>';
    return html;
  }

  private renderMismatchDetails(mismatchDetails: MismatchDetail[]): string {
    const algoColors: Record<string, string> = {
      BruteForce: '#00ff9d',
      DLX: '#00b8ff',
      CP: '#bb9af7'
    };

    let html = `
      <div class="diagnostics-section mismatch-details">
        <h3 class="diagnostics-section-title" style="color: #ff5555;">Iteration Mismatch Details (${mismatchDetails.length})</h3>
        <div class="mismatch-list">
    `;

    mismatchDetails.forEach(d => {
      html += `
        <div class="mismatch-item">
          <div class="mismatch-header">
            <strong style="color: ${algoColors[d.algo]};">${d.language}</strong>
            <span class="muted">(${d.algo})</span>
          </div>
          <div class="mismatch-matrices">
      `;

      d.matrices.forEach(m => {
        html += `
          <div class="matrix-mismatch">
            Matrix ${m.matrix}:
            <span class="actual">${m.actual.toLocaleString()}</span>
            vs expected
            <span class="expected">${m.expected.toLocaleString()}</span>
          </div>
        `;
      });

      html += '</div></div>';
    });

    html += '</div></div>';
    return html;
  }
}

export const diagnosticsModal = new DiagnosticsModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).diagnosticsModal = diagnosticsModal;
  (window as any).showDiagnostics = () => {
    diagnosticsModal.open();
  };
}
