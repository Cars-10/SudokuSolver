// Methodology Modal - explains the benchmark methodology
import { BaseModal } from './BaseModal';

export class MethodologyModal extends BaseModal {
  constructor() {
    super('MM', 'Methodology Modal', {
      dependencies: ['PersonalityService']
    });
  }

  protected getDomId(): string {
    return 'methodModal';
  }

  protected render(): HTMLElement {
    const title = this.getPersonaText('methodology') || 'Benchmark Methodology';
    const bodyContent = this.renderMethodologyContent();

    return this.createModalElement(title, bodyContent);
  }

  private renderMethodologyContent(): string {
    return `
      <div class="methodology-content">
        <h3>Algorithm Specification</h3>
        <p>All implementations use an identical brute-force backtracking algorithm:</p>
        <ul>
          <li><strong>Search order:</strong> Row-major (row 0-8, then col 0-8)</li>
          <li><strong>Candidate order:</strong> 1 through 9 in ascending order</li>
          <li><strong>Iteration counting:</strong> Increment BEFORE validity check, on EVERY attempt</li>
          <li><strong>No optimizations:</strong> No MRV, no constraint propagation</li>
        </ul>

        <h3>Verification</h3>
        <p>The iteration count serves as the algorithm's fingerprint. If iteration counts match the C reference, the algorithm is correct:</p>
        <table class="results-table">
          <thead>
            <tr>
              <th>Matrix</th>
              <th>Expected Iterations</th>
            </tr>
          </thead>
          <tbody>
            <tr><td>Matrix 1</td><td>656</td></tr>
            <tr><td>Matrix 2</td><td>439,269</td></tr>
            <tr><td>Matrix 3</td><td>98,847</td></tr>
            <tr><td>Matrix 4</td><td>9,085</td></tr>
            <tr><td>Matrix 5</td><td>445,778</td></tr>
          </tbody>
        </table>

        <h3>Scoring System</h3>
        <p>Performance score is calculated as a weighted composite:</p>
        <div class="score-formula">
          Score = 0.8 × (normalized time) + 0.2 × (normalized memory)
        </div>
        <p>Lower scores indicate better performance. Time is weighted 80% as it's the primary performance indicator.</p>

        <h3>Test Environment</h3>
        <ul>
          <li><strong>Docker:</strong> All languages run in isolated containers with identical resources</li>
          <li><strong>Timeout:</strong> 5 minutes per matrix</li>
          <li><strong>Measurements:</strong> Time (wall clock), Memory (peak RSS), CPU usage</li>
        </ul>
      </div>
    `;
  }

  adaptToPersona(persona: string): void {
    if (!this.isOpen || !this.container) return;

    const titleEl = this.container.querySelector('h2');
    if (titleEl) {
      titleEl.textContent = this.getPersonaText('methodology') || 'Benchmark Methodology';
    }
  }
}

export const methodologyModal = new MethodologyModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).methodologyModal = methodologyModal;
  (window as any).showMethodology = () => methodologyModal.open();
}
